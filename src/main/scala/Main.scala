import AppConfig._
import futureconvert._
import cats.data.EitherT
import cats.instances.list._
import cats.syntax.traverse._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Request, RequestBuilder, Response}
import com.twitter.io.Buf
import scala.collection.JavaConverters._
import io.circe.{Error, _}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.finch._
import io.finch.syntax._
import io.finch.circe._

import scala.concurrent.Future
import com.twitter.util.Await

case class Ingredient(name: String, unit: Option[String], qty: Option[String])
case class ParserPayload(method: String, params: List[List[String]], jsonrpc: String = "2.0", id: Int = 0)
case class URL(name: String)

object Main extends App {

  implicit val encodePayload: Encoder[ParserPayload] = deriveEncoder
  implicit val encodeIngredient: Encoder[Ingredient] = deriveEncoder
  implicit val decodeIngredient: Decoder[Ingredient] = Decoder.forProduct3("name", "unit", "qty")(Ingredient.apply)
  implicit val decodeURL: Decoder[URL] = deriveDecoder

  def stringify(ul: Element): List[String] = ul
    .getElementsByTag("li")
    .asScala.toList
    .map(_.text)

  // refactor parser and validator into helper functions
  def callParser(l: List[List[String]]): Future[String] = {
    val jsonString: String = ParserPayload("parse_all", l).asJson.toString
    val client: Service[Request, Response] = ClientBuilder()
      .stack(Http.client)
      .hosts("localhost:4000")
      .build()
    val request: Request = RequestBuilder()
      .url("localhost:4000/jsonrpc")
      .buildPost(Buf.Utf8(jsonString))
    client(request).map(_.contentString).asScala
  }

  def callValidator(i: Ingredient): Future[String] = {
    // eventually use word2vec for this
    val client: Service[Request, Response] = ClientBuilder()
      .stack(Http.client)
      .hosts("api.edamam.com:443")
      .tls("api.edamam.com") // for https requests
      .build()
    val request = RequestBuilder()
      .url(s"https://api.edamam.com/api/food-database/parser?ingr=${java.net.URLEncoder.encode(i.name, "UTF-8")}&app_id=$appID&app_key=$appKey")
      .buildGet()
    client(request).map(_.contentString).asScala
  }

  def isIngredientList(l: List[Ingredient]): Boolean =
    if (l.isEmpty) false
    else l.count {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    } / l.length.toFloat > .5 // some arbitrary number

  // maybe have a better way of handling errors, but for now just throw an exception
  def decodeJSON[A](s: String)(jsonDecoder: Json => Either[Error, A]): Either[Error, A] = for {
      json <- parse(s)
      decoded <- jsonDecoder(json)
    } yield decoded

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

//  val url = "https://www.allrecipes.com/recipe/9027/kung-pao-chicken/"

  def parseUL(url: String): EitherT[Future, Error, List[Ingredient]] = {

    val doc = Jsoup.connect(url).timeout(10000).get()
    // begin hard coding stuff to remove
    doc.select("[ng-cloak]").remove()
    // end hard coding stuff to remove

    val unorderedLists: List[List[String]] = doc
      .getElementsByTag("ul")
      .asScala.toList
      .map(stringify)

    for {
      response <- EitherT.right(callParser(unorderedLists))
      allIngredients <- EitherT(Future {decodeJSON(response) { _.hcursor.get[List[List[Ingredient]]]("result")} })
      (normalIngredients: List[Ingredient], sketchyIngredients: List[Ingredient]) = split(allIngredients)
      validationJSON: List[String] <- EitherT.right(sketchyIngredients.traverse(callValidator)) // make api call to validate
      validation: List[Boolean] <- EitherT(Future {validationJSON.traverse(decodeJSON(_)(validate)) })
      validatedIngredients: List[Ingredient] = validation
        .zip(sketchyIngredients) // zip sketchy ingredients with boolean values
        .filter { case (bool, _) => bool } // filter out ones that are invalid
        .map { case (_, value) => value }
    } yield normalIngredients ::: validatedIngredients

  }

  val parseEndpoint: Endpoint[List[Ingredient]] = post("parse" :: jsonBody[URL]) { u: URL => parseUL(u.name)
    .value
    .map(e => e.getOrElse(throw new Exception("server error !!"))) // incorporate error handling with finch
    .map(Ok)
  }

  val service = parseEndpoint.toServiceAs[Application.Json]
  Await.ready(Http.server.serve(":8081", service))

}

//  def suspectList(e: Element): Boolean =
//    if (e.children.size < 3) false
//    else e.children.asScala.toList
//      .map { child => (child.tagName, child.className) }
//      .groupBy(identity).mapValues(_.size).values
//      .max >= e.children.size / 2.toFloat

// perform suspectList on ALL nodes