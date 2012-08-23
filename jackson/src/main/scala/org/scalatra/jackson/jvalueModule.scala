package org.scalatra
package jackson

import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import collection.mutable.ArrayBuffer
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.ser.Serializers
import com.fasterxml.jackson.databind.deser.Deserializers
import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.module.scala.deser.UntypedObjectDeserializerModule
import json.AST._
import com.fasterxml.jackson.core.JsonGenerator
import java.math.BigInteger
import com.fasterxml.jackson.databind.`type`.TypeFactory

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

private object JValueSerializerResolver extends Serializers.Base {
  private val JVALUE = classOf[JValue]
  override def findSerializer(config: SerializationConfig, theType: JavaType, beanDesc: BeanDescription) = {
    if (!JVALUE.isAssignableFrom(theType.getRawClass)) null
    else new JValueSerializer
  }
}


class JValueDeserializer(factory: TypeFactory, klass: Class[_]) extends JsonDeserializer[Object] {
  def deserialize(jp: JsonParser, ctxt: DeserializationContext): Object = {
    if (jp.getCurrentToken == null) {
      jp.nextToken()
    }

    val value = jp.getCurrentToken match {
      case JsonToken.VALUE_NULL => JNull
      case JsonToken.VALUE_NUMBER_INT => JInt(BigInt(jp.getText))
      case JsonToken.VALUE_NUMBER_FLOAT =>
        if (ctxt.isEnabled(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS)) JDecimal(jp.getDecimalValue)
        else JDouble(jp.getDoubleValue)
      case JsonToken.VALUE_STRING => JString(jp.getText)
      case JsonToken.VALUE_TRUE => JBoolean(true)
      case JsonToken.VALUE_FALSE => JBoolean(false)
      case JsonToken.START_ARRAY => {
        JArray(jp.getCodec.readValue(jp, Types.build(factory, manifest[List[JValue]])))
      }
      case JsonToken.START_OBJECT => {
        jp.nextToken()
        deserialize(jp, ctxt)
      }
      case JsonToken.FIELD_NAME | JsonToken.END_OBJECT => {
        val fields = new ArrayBuffer[JField]
        while (jp.getCurrentToken != JsonToken.END_OBJECT) {
          val name = jp.getCurrentName
          jp.nextToken()
          fields += JField(name, jp.getCodec.readValue(jp, Types.build(factory, manifest[JValue])))
          jp.nextToken()
        }
        JObject(fields.toList)
      }
      case _ => throw ctxt.mappingException(classOf[JValue])
    }

    if (!klass.isAssignableFrom(value.getClass)) {
      throw ctxt.mappingException(klass)
    }

    value
  }

  override def isCachable = true
}

private object JValueDeserializerResolver extends Deserializers.Base {
  private val JVALUE = classOf[JValue]
  
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig, beanDesc: BeanDescription) = {
    if (!JVALUE.isAssignableFrom(javaType.getRawClass)) null
    else new JValueDeserializer(config.getTypeFactory(), javaType.getRawClass)
  }
}


trait JValueModule extends JacksonModule {
  this += (_ addSerializers JValueSerializerResolver)
  this += JValueDeserializerResolver
}

class ScalatraScalaModule 
            extends JacksonModule 
            with JValueModule 
            with EnumerationModule 
            with OptionModule
            with SeqModule
            with IterableModule
            with TupleModule
            with MapModule
            with CaseClassModule
            with SetModule
            with UntypedObjectDeserializerModule
object ScalatraScalaModule extends ScalatraScalaModule
