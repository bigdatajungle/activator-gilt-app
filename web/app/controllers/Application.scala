package controllers

import play.api.mvc.{Action, Controller}
import apidoc.models.json._
import apidoc.models.Sample

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {
  val serverUrl = "http://localhost:9000"

  val client = new apidoc.Client(serverUrl)

  def index = Action.async {
    for {
      sample <- client.Samples.getByGuid("abc")
    } yield {
      sample match {
        case (code, sample: Sample) =>
          Ok(views.html.index(s"Hello ${sample.name}"))

        case o: play.api.libs.ws.Response =>
          Ok(views.html.index(o.body.toString))
      }
    }
  }
}
