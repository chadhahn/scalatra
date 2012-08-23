package org.scalatra
package jackson

import java.io.InputStream
import json.AST._
import json.Xml.toJson

trait JacksonSupport extends json.JsonSupport with JacksonOutput {
  
  protected def readJsonFromStream(stream: InputStream): JsonType =
    jsonMapper.readValue(stream, classOf[JValue])

  protected def readXmlFromStream(stream: InputStream): JsonType = {
    val JObject(JField(_, jv) :: Nil) = toJson(scala.xml.XML.load(stream))
    jv
  }
    

  protected val jsonZero: JsonType = JNothing
}
