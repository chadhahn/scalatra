package org.scalatra
package jackson

import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.ser.Serializers
import java.io.Writer
import com.fasterxml.jackson.dataformat.xml.XmlMapper
import xml.XML
import com.fasterxml.jackson.core.JsonGenerator
import json.AST._
import json.Xml.toXml

private[jackson] trait JacksonOutput extends json.JsonOutput {

  protected def useBigDecimalForFloats: Boolean = false
  protected def useBigIntForInts: Boolean = false

  protected val jsonMapper = new ObjectMapper()
  configureJackson(jsonMapper)

  protected def configureJackson(mapper: ObjectMapper) {
    mapper.registerModule(ScalatraScalaModule)
    mapper.configure(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS, useBigDecimalForFloats)
    mapper.configure(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS, useBigIntForInts)
    mapper.configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false)
  }

  protected type JsonType = JValue

  protected def jsonClass: Class[_] = classOf[JsonType]

  protected def writeJsonAsXml(json: JsonType, writer: Writer) {
    XML.write(response.writer, xmlRootNode.copy(child = toXml(json)), response.characterEncoding.get, xmlDecl = true, null)
  }

  protected def writeJson(json: JsonType, writer: Writer) {
    jsonMapper.writeValue(writer, json)
  }
}
