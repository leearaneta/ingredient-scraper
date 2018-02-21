import AppConfig._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.collection.JavaConverters._
import sys.process._
import io.circe._
import io.circe.parser._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

case class Ingredient(name: Option[String], unit: Option[String], qty: Option[String])

object Main extends App {

  implicit val sttpBackend: SttpBackend[Future, Nothing] = AsyncHttpClientFutureBackend()
  implicit val decodeIngredient: Decoder[Ingredient] = Decoder.forProduct3("name", "unit", "qty")(Ingredient.apply)

  def stringify(ul: Element): String = ul
    .getElementsByTag("li")
    .asScala.toList
    .map(_.text)
    .fold("") { _ + "\n" + _ }

  def standardize(s: String): String = {
    val path = "python src/main/ingredient-phrase-tagger/bin/" // put this in separate docker container
    val parserFile = path + "parse-ingredients.py"
    val converterFile = path + "convert-to-json.py"
    Seq("echo", s) #| parserFile #| converterFile !! // wrap this in IO monad or something
  }

  def format(s: String): List[Ingredient] = parse(s)
    .getOrElse(Json.Null)
    .as[List[Ingredient]] match { case Right(x) => x }

  def isIngredientList(l: List[Ingredient]): Boolean =
    if (l.isEmpty) false
    else l.count {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    } / l.length.toFloat >= .75 // some arbitrary number

  def makeRequest(i: Ingredient): Future[Response[String]] = sttp // eventually use word2vec for this
    .get(uri"https://api.edamam.com/api/food-database/parser?ingr=${i.name}&app_id=$appID&app_key=$appKey")
    .send()

  def validateIngredient(r: Response[String]): Boolean = parse(r.unsafeBody)
    .getOrElse(Json.Null)
    .hcursor
    .get[List[String]]("parsed") match {
      case Right(x) => x.isEmpty
      case Left(_) => false
    }

  def suspectList(e: Element): Boolean =
    if (e.children.size < 3) false
    else e.children.asScala.toList
      .map { child => (child.tagName, child.className) }
      .groupBy(identity).mapValues(_.size).values
      .max >= e.children.size / 2.toFloat

  // perform suspectList on ALL nodes

  val url = "http://allrecipes.com/recipe/9027/kung-pao-chicken/"
  val doc = Jsoup.connect(url).timeout(10000).get()

  // begin hard coding stuff to remove
    doc.select("[ng-cloak]").remove()
  // end hard coding stuff to remove

  def method1: Unit = {
    val unorderedLists = doc.getElementsByTag("ul").asScala.toList
    val (normalIngredients: List[Ingredient], sketchyIngredients: List[Ingredient]) = unorderedLists
      .map(stringify) // take all raw text and separate with newlines
      .map(standardize) // standardize using model
      .map(format) // convert to ingredient case class
      .filter(isIngredientList) // take out most things that aren't ingredients
      .flatten // combine to one list of ingredients
      .partition { // separate between sketchy and normal ingredients
      case Ingredient(_, None, None) => false
      case _ => true
    }

    val ingredients: Future[List[Ingredient]] = for {
      validatedIngredients <- sketchyIngredients.traverse(makeRequest) map { _
        .map(validateIngredient) // make api call to validate
        .zip(sketchyIngredients)
        .filter { case (bool, _) => bool } // filter out ones that are invalid
        .map { case (_, value) => value }
      }
    } yield normalIngredients ::: validatedIngredients

    ingredients onComplete {
      case Success(r) => println(r)
      case Failure(e) => println(e) // throw exception probably
    }
  }

  def method2: Unit = {

  }
}