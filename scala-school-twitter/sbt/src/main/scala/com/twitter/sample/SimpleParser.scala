package com.twitter.sample

import org.codehaus.jackson.{JsonToken => T, _}

case class SimpleParsed(id: Long, text: String)

class SimpleParser {
  val parserFactory = new JsonFactory()

  def parse(str: String): Option[SimpleParsed] = {
    var parsed: Option[SimpleParsed] = None
    val parser = parserFactory.createJsonParser(str)
    var nested = 0

    if (parser.nextToken() == T.START_OBJECT) {
      var token = parser.nextToken()
      var textOpt: Option[String] = None
      var idOpt: Option[Long] = None

      while (token != null) {
        if (token == T.FIELD_NAME && nested == 0) {
          parser.getCurrentName() match {
            case "text" =>
              parser.nextToken()
              textOpt = Some(parser.getText())
            case "id" =>
              parser.nextToken()
              idOpt = Some(parser.getLongValue())
            case _ => // noop
          }
        } else if (token == T.START_OBJECT) {
          nested += 1
        } else if (token == T.END_OBJECT) {
          nested -= 1
        }

        token = parser.nextToken()
      }

      parsed = for (t <- textOpt; i <- idOpt) yield SimpleParsed(i, t)
    }

    parsed
  }
}
