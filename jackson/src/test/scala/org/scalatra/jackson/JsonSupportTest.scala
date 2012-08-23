package org.scalatra
package jackson

import org.scalatra.{json, ScalatraServlet}
import json.AST._
import DSL.WithJFloat._

class JsonSupportTest extends json.JsonSupportTestBase {
  override protected def expectedXml = """<?xml version='1.0' encoding='UTF-8'?>
                                  |<resp><k1>v1</k1><k2>v2</k2></resp>""".stripMargin
  addServlet(classOf[JsonSupportTestServlet], "/*")
  addServlet(classOf[JsonPTestServlet], "/p/*")
  addServlet(new ScalatraServlet with JacksonOutput {
    override protected lazy val jsonVulnerabilityGuard: Boolean = true
    override val jsonpCallbackParameterNames: Iterable[String] = Some("callback")
    get("/json") {
      ("k1" -> "v1") ~ ("k2" -> "v2")
    }

    get("/jsonp") {
      ("k1" -> "v1") ~ ("k2" -> "v2")
    }

  }, "/g/*")


}


class JsonSupportTestServlet extends ScalatraServlet with JacksonOutput {
  get("/json") {
    ("k1" -> "v1") ~ ("k2" -> "v2")
  }


  get("/nulls") {
    JNull
  }

}

class JsonPTestServlet extends ScalatraServlet with JacksonOutput {
  override def jsonpCallbackParameterNames = Some("callback")

  get("/jsonp") {
    ("k1" -> "v1") ~ ("k2" -> "v2")
  }
}
