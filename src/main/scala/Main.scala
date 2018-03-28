import AppConfig._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

import com.twitter.finagle.http.filter.Cors
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Request, RequestBuilder, Response}
import com.twitter.io.Buf
import com.twitter.util.{Await, Future}

import scala.collection.JavaConverters._
import java.net.URLEncoder

import io.circe.{Error, _}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.finch._
import io.finch.syntax._
import io.finch.circe._
import com.hypertino.inflector.English

case class Ingredient(name: String, unit: Option[String], qty: Option[String])
case class Recipe(name: String, ingredients: List[Ingredient])
case class ParserPayload(method: String, params: List[List[String]], jsonrpc: String = "2.0", id: Int = 0)
case class URL(name: String)

object Main extends App {

  implicit val encodePayload: Encoder[ParserPayload] = deriveEncoder
  implicit val encodeIngredient: Encoder[Ingredient] = deriveEncoder
  implicit val encodeRecipe: Encoder[Recipe] = deriveEncoder
  implicit val decodeIngredient: Decoder[Ingredient] = Decoder.forProduct3("name", "unit", "qty")(Ingredient.apply)
  implicit val decodeURL: Decoder[URL] = deriveDecoder

  def getDocument(u: String): Document = { // TODO: optimize this function based on URL (maybe use selenium)
    val doc = Jsoup.connect(u).timeout(10000).get
    doc.select("[ng-cloak]").remove()
    doc
  }

  def getChildrenText(e: Element): List[String] = e
    .children
    .asScala.toList
    .map(_.text)

  def suspectList(e: Element): Boolean =
    if (e.children.size < 3) false
    else e.children.asScala.toList
      .map { child => (child.tagName, child.className) }
      .groupBy(identity).mapValues(_.size).values
      .max >= e.children.size / 2.toFloat

  def getUnorderedLists(d: Document): List[List[String]] = d
    .getElementsByTag("ul")
    .asScala.toList
    .map(getChildrenText)

  def inferLists(d: Document): List[List[String]] = d
    .getAllElements
    .asScala.toList
    .filter(suspectList)
    .map(getChildrenText)

  // refactor parser and validator into helper functions
  def callParser(l: List[List[String]]): Future[String] = {
    val jsonString: String = ParserPayload("parse_all", l).asJson.toString
    val client: Service[Request, Response] = ClientBuilder()
      .stack(Http.client)
      .hosts("localhost:4000")
      .build()
    val request: Request = RequestBuilder()
      .url("http://localhost:4000/jsonrpc")
      .buildPost(Buf.Utf8(jsonString))
    client(request).map(_.contentString)
  }

  def callValidator(i: Ingredient): Future[String] = {
    // eventually use word2vec for this
    val client: Service[Request, Response] = ClientBuilder()
      .stack(Http.client)
      .hosts("api.edamam.com:443")
      .tls("api.edamam.com") // for https requests
      .build()
    val paramString = s"ingr=${URLEncoder.encode(i.name, "UTF-8")}&app_id=$appID&app_key=$appKey"
    val request = RequestBuilder()
      .url(s"https://api.edamam.com/api/food-database/parser?" + paramString)
      .buildGet()
    client(request).map(_.contentString)
  }

  def isIngredientList(l: List[Ingredient]): Boolean =
    if (l.isEmpty) false
    else l.count {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    } / l.length.toFloat > .5 // some arbitrary number

  // maybe have a better way of handling errors, but for now just throw an exception
  def decodeJSON[A](s: String)(jsonDecoder: Json => Either[Error, A]): A = {
    val decoded: Either[Error, A] = for {
      json <- parse(s)
      decoded <- jsonDecoder(json)
    } yield decoded
    decoded.getOrElse(throw new Exception("couldn't decode"))
  }

  def validate(j: Json): Either[Error, Boolean] = j
    .hcursor
    .get[List[String]]("parsed")
    .map(_.nonEmpty)

  def split(l: List[List[Ingredient]]): (List[Ingredient], List[Ingredient]) = l
    .filter(isIngredientList) // take out most entries that aren't ingredients
    .flatten // combine to one list of ingredients
    .partition { // separate between sketchy and normal ingredients
      case Ingredient(_, None, None) => false
      case _ => true
    }

  def foodifyText(l: List[List[String]]): Future[List[Ingredient]] = for {
    response: String <- callParser(l)
    allIngredients: List[List[Ingredient]] = decodeJSON(response) { _.hcursor.get[List[List[Ingredient]]]("result")}
    (normalIngredients: List[Ingredient], sketchyIngredients: List[Ingredient]) = split(allIngredients)
    validationJSON: Seq[String] <- Future.collect(sketchyIngredients.map(callValidator)) // make api call to validate
    validatedIngredients: List[Ingredient] = validationJSON.map(decodeJSON(_)(validate))
      .toList
      .zip(sketchyIngredients) // zip sketchy ingredients with boolean values
      .filter { case (bool, _) => bool } // filter out ones that are invalid
      .map { case (_, value) => value }
    ingredients = normalIngredients ::: validatedIngredients
    if ingredients.nonEmpty
  } yield ingredients

  def foodifyHTML(f: Document => List[List[String]]) = f _ andThen foodifyText

  def foodify(d: Document): Future[List[Ingredient]] = foodifyHTML(getUnorderedLists)(d).rescue {
    case _ => foodifyHTML(inferLists)(d)
  }

  def singularize(i: Ingredient) = i.copy(name = English.singular(i.name))

  def execute(s: String): Future[Recipe] = {
    val doc = getDocument(s)
    for {
      ingredients <- foodify(doc)
      formattedIngredients = ingredients.map(singularize)
    } yield Recipe(doc.title, formattedIngredients)
  }

  val parseEndpoint: Endpoint[Recipe] = post("parse" :: jsonBody[URL]) { u: URL => execute(u.name).map(Ok) }
  val service = parseEndpoint.toServiceAs[Application.Json]

  val policy: Cors.Policy = Cors.Policy(
    allowsOrigin = _ => Some("*"),
    allowsMethods = _ => Some(Seq("GET", "POST")),
    allowsHeaders = _ => Some(Seq("Content-Type"))
  )

  val corsService: Service[Request, Response] = new Cors.HttpFilter(policy).andThen(service)
  Await.ready(Http.server.serve(":8081", corsService))

}