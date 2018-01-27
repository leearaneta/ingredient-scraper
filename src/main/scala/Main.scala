import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.collection.JavaConverters._
import sys.process._
import org.json4s._
import org.json4s.native.JsonMethods._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

case class Ingredient(name: Option[String], unit: Option[String], qty: Option[String])

object Main extends App {

  implicit val formats = DefaultFormats

  def stringify(ul: Element): String = ul
    .getElementsByTag("li")
    .asScala.toList
    .map(_.text)
    .fold("") { _ + "\n" + _ }

  def standardize(s: String): String = {
    val path = "python ../ingredient-phrase-tagger/bin/"
    val parserFile = path + "parse-ingredients.py"
    val converterFile = path + "convert-to-json.py"
    Seq("echo", s) #| parserFile #| converterFile !! // wrap this in IO monad or something
  }

  def format(s: String): List[Ingredient] = parse(s).extract[List[Ingredient]]

  def isIngredientList(l: List[Ingredient]): Boolean = {
    if (l.isEmpty) return false
    val valid: List[Ingredient] = l filter {
      case Ingredient(_, Some(_), Some(_)) => true
      case _ => false
    }
    valid.length / l.length.toFloat >= .75 // some arbitrary number
  }

  def makeRequest(i: Ingredient): Future[Response[String]] = {
    implicit val sttpBackend = AsyncHttpClientFutureBackend()
    val appID = "5c3b30f0" // hide these
    val appKey = "6e4e5e4c702f73d47f0f1cd6937b225b"
    val requestURL = uri"https://api.edamam.com/api/food-database/parser?ingr=${i.name}&app_id=$appID&app_key=$appKey"
    sttp.get(requestURL).send()
  }

  def validateIngredients(r: Response[String]): Boolean = {
    val JSON = r.unsafeBody
    val JArray(x) = parse(JSON) \ "parsed"
    x.isEmpty
  }

  val url = "http://allrecipes.com/recipe/9027/kung-pao-chicken/"
  val doc = Jsoup.connect(url).timeout(10000).get()

  // begin hard coding stuff to remove
    doc.select("[ng-cloak]").remove()
  // end hard coding stuff to remove

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
       .map(validateIngredients)
       .zip(sketchyIngredients)
       .filter { case (bool, _) => bool }
       .map { case (_, value) => value }
     }
   } yield normalIngredients ::: validatedIngredients

  ingredients onComplete {
    case Success(r) => println(r)
    case Failure(e) => "bye" // throw exception probably
  }
}
