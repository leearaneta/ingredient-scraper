import AppConfig._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response, Method}
import com.twitter.util.{Await, Future}

import scala.collection.JavaConverters._

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._

import io.finch._
import io.finch.syntax._
import io.finch.circe._

case class Ingredient(name: Option[String], unit: Option[String], qty: Option[String])
case class ParserPayload(method: String, params: List[List[String]], jsonrpc: String = "2.0", id: Int = 0)
case class URL(name: String)

object Main extends App {

  implicit val encodePayload: Encoder[ParserPayload] = deriveEncoder
  implicit val encodeIngredient: Encoder[Ingredient] = deriveEncoder
  implicit val decodeIngredient: Decoder[Ingredient] = Decoder.forProduct3("name", "unit", "qty")(Ingredient.apply)

  def stringify(ul: Element): List[String] = ul
    .getElementsByTag("li")
    .asScala.toList
    .map(_.text)

  def callParser(l: List[List[String]]): Future[String] = {
    val jsonString: String = ParserPayload("parse_all", l).asJson.toString()
    val client: Service[Request, Response] = Http.newService("localhost:4000")
    val request: Request = Request(Method.Post, "/jsonrpc")
    request.setContentString(jsonString)
    client(request).map(_.contentString)
  }

  def isIngredientList(l: List[Ingredient]): Boolean =
    if (l.isEmpty) false
    else l.count {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    } / l.length.toFloat > .5 // some arbitrary number

  def callValidator(i: Ingredient): Future[String] = {
    // eventually use word2vec for this
    val baseURL = "https://api.edamam.com/api"
    val endpoint = s"food-database/parser?ingr=${i.name}&app_id=$appID&app_key=$appKey"
    val client: Service[Request, Response] = Http.newService(baseURL)
    val request: Request = Request(Method.Get, endpoint)
    client(request).map(_.contentString)
  }

  def parseJSON(s: String): HCursor = parse(s)
    .getOrElse(throw new Exception("couldn't parse json"))
    .hcursor

  def validateIngredient(r: String): Boolean = parseJSON(r)
    .get[List[String]]("parsed") match {
      case Right(x) => x.nonEmpty
      case Left(_) => false
    }

//  val url = "https://www.allrecipes.com/recipe/9027/kung-pao-chicken/"

  def parseUL(url: String): Future[List[Ingredient]] = {

    val doc = Jsoup.connect(url).timeout(10000).get()
    // begin hard coding stuff to remove
    doc.select("[ng-cloak]").remove()
    // end hard coding stuff to remove

    val unorderedLists: List[List[String]] = doc
      .getElementsByTag("ul")
      .asScala.toList
      .map(stringify)

    for {
      response <- callParser(unorderedLists)
      (normalIngredients: List[Ingredient], sketchyIngredients: List[Ingredient]) = parseJSON(response)
        .get[List[List[Ingredient]]]("result") // convert json to ingredient list
        .getOrElse(throw new Exception("couldn't decode")) // take right side of either
        .filter(isIngredientList) // take out most things that aren't ingredients
        .flatten // combine to one list of ingredients
        .partition { // separate between sketchy and normal ingredients
        case Ingredient(_, None, None) => false
        case _ => true
      }
      validatedIngredients: List[Ingredient] <- Future.collect(sketchyIngredients.map(callValidator)) map { _
        .map(validateIngredient) // make api call to validate
        .zip(sketchyIngredients)
        .filter { case (bool, _) => bool } // filter out ones that are invalid
        .map { case (_, value) => value }
        .toList
      }
    } yield normalIngredients ::: validatedIngredients

  }

  val parseEndpoint: Endpoint[List[Ingredient]] = post("parse" :: jsonBody[URL]) { u: URL =>
    parseUL(u.name).map(Ok)
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