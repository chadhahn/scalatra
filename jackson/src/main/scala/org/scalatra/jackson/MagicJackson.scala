package org.scalatra
package jackson

import json.AST._
import com.fasterxml.jackson.databind.util.TokenBuffer
import java.io.IOException

trait MagicJackson extends JacksonSupport {
  override protected def renderPipeline: RenderPipeline = renderToJson orElse super.renderPipeline

  private[this] def isApplicable = format == "json" || format == "xml"

  private def renderToJson: RenderPipeline = {
    case a: JValue => super.renderPipeline(a)
    case null if isApplicable => JNull
    case p if isApplicable => {
      val buf = new TokenBuffer(jsonMapper)
      try {
        jsonMapper.writeValue(buf, p)
        val parser = buf.asParser()
        val result = parser.readValueAs(classOf[JValue])
        parser.close()
        result
      } catch {
        case e: IOException => throw new IllegalArgumentException(e.getMessage, e) 
      }
    }
  }
}
