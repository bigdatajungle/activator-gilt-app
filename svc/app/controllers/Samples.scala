package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json._

import apidoc.models.json._
import apidoc.models.sample.SampleImpl

object Samples extends Controller {
  def getGuid(guid: String) = Action {
    val sampleObj = SampleImpl(guid, "Gilt World")
    Ok(Json.toJson(sampleObj))
  }
}
