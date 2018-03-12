import AppConfig._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.collection.JavaConverters._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

case class Ingredient(name: Option[String], unit: Option[String], qty: Option[String])
case class ParserPayload(method: String, params: List[List[String]], jsonrpc: String = "2.0", id: Int = 0)

object Main extends App {

  implicit val encodePayload: Encoder[ParserPayload] = deriveEncoder
  implicit val decodeIngredient: Decoder[Ingredient] = Decoder.forProduct3("name", "unit", "qty")(Ingredient.apply)

  def stringify(ul: Element): List[String] = ul
    .getElementsByTag("li")
    .asScala.toList
    .map(_.text)

  def callParser(l: List[List[String]]): Response[String] = {
    implicit val syncBackend: SttpBackend[Id, Nothing] = HttpURLConnectionBackend()
    sttp
      .body( ParserPayload("parse_all", l).asJson.toString )
      .post(uri"http://localhost:4000/jsonrpc")
      .send()
  }

  def isIngredientList(l: List[Ingredient]): Boolean =
    if (l.isEmpty) false
    else l.count {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    } / l.length.toFloat >= .75 // some arbitrary number

  def callValidator(i: Ingredient): Future[Response[String]] = {
    implicit val asyncBackend: SttpBackend[Future, Nothing] = AsyncHttpClientFutureBackend()
    sttp // eventually use word2vec for this
      .get(uri"https://api.edamam.com/api/food-database/parser?ingr=${i.name}&app_id=$appID&app_key=$appKey")
      .send()
  }

  def validateIngredient(r: Response[String]): Boolean = parse(r.unsafeBody)
    .getOrElse(Json.Null)
    .hcursor
    .get[List[String]]("parsed") match {
      case Right(x) => x.isEmpty
      case Left(_) => false
    }

  val url = "http://allrecipes.com/recipe/9027/kung-pao-chicken/"
  val doc = Jsoup.connect(url).timeout(10000).get()

  // begin hard coding stuff to remove
    doc.select("[ng-cloak]").remove()
  // end hard coding stuff to remove

  val unorderedLists: List[List[String]] = doc
    .getElementsByTag("ul")
    .asScala.toList
    .map(stringify)

  val JSONResponse: String = callParser(unorderedLists).unsafeBody
  val parsedIngredients: List[List[Ingredient]] = decode[List[List[Ingredient]]](JSONResponse)
    .getOrElse(throw new Exception("couldn't decode"))

  val (normalIngredients: List[Ingredient], sketchyIngredients: List[Ingredient]) = parsedIngredients
    .filter(isIngredientList) // take out most things that aren't ingredients
    .flatten // combine to one list of ingredients
    .partition { // separate between sketchy and normal ingredients
      case Ingredient(_, None, None) => false
      case _ => true
    }

  for {
    validatedIngredients <- sketchyIngredients.traverse(callValidator) map { _
      .map(validateIngredient) // make api call to validate
      .zip(sketchyIngredients)
      .filter { case (bool, _) => bool } // filter out ones that are invalid
      .map { case (_, value) => value }
    }
  } yield normalIngredients ::: validatedIngredients

}

//  def suspectList(e: Element): Boolean =
//    if (e.children.size < 3) false
//    else e.children.asScala.toList
//      .map { child => (child.tagName, child.className) }
//      .groupBy(identity).mapValues(_.size).values
//      .max >= e.children.size / 2.toFloat

// perform suspectList on ALL nodes