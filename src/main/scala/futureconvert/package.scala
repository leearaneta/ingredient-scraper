import com.twitter.util.{Future => TwitterFuture, Promise => TwitterPromise}
import scala.concurrent.{Future => ScalaFuture, ExecutionContext}
import scala.util.{Success, Failure}

package object futureconvert {

  implicit class RichScalaFuture[A](val sf: ScalaFuture[A]) extends AnyVal {
    def asTwitter(implicit e: ExecutionContext): TwitterFuture[A] = {
      val promise: TwitterPromise[A] = new TwitterPromise[A]()
      sf.onComplete {
        case Success(value) => promise.setValue(value)
        case Failure(exception) => promise.setException(exception)
      }
      promise
    }
  }

}