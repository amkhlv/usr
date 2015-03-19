/**
 * Created by andrei on 28/07/14.
 */

import play.api._
import play.api.mvc._
import play.filters.csrf._
import play.api.http.HeaderNames._
import play.api.libs.concurrent.Execution.Implicits._


object Global extends  GlobalSettings {

  val surfFilter = new CSRFFilter()

  val noCacheHeaders = Seq(
    CACHE_CONTROL -> "no-cache, no-store, max-age=0",
    PRAGMA -> "no-cache",
    EXPIRES ->"-1")

  override def doFilter(action: EssentialAction): EssentialAction = EssentialAction { request =>
    surfFilter(action).apply(request).map(_.withHeaders(noCacheHeaders:_*))
  }


}
