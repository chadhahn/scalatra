package org.scalatra
package jackson

import org.scalatra.json.AST._
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.core.JsonGenerator
import java.math.BigInteger

class JValueSerializer extends JsonSerializer[JValue]{
  def serialize(value: JValue, json: JsonGenerator, provider: SerializerProvider) {
    value match {
      case JInt(v) => json.writeNumber(new BigInteger(v.toString()))
      case JDouble(v) => json.writeNumber(v)
      case JDecimal(v) => json.writeNumber(v.bigDecimal)
      case JString(v) => json.writeString(v)
      case JBoolean(v) => json.writeBoolean(v)
      case JArray(elements) => json.writeObject(elements)
      case JField(name, value) => {
        json.writeFieldName(name)
        json.writeObject(value)
      }
      case JObject(fields) => {
        json.writeStartObject()
        fields.foreach(json.writeObject)
        json.writeEndObject()
      }
      case JNull => json.writeNull()
      case JNothing => ()
    }
  } 
}