package lmxml
package transforms
package json

import util.parsing.json._

object JSTransform {
  JSON.perThreadNumberParser = { (input: String) =>
    if (input.contains(".")) input.toDouble else input.toInt
  }

  def apply(jsonStr: String) = {
    val complete = (pros: Seq[(String,Processor)]) => Transform(pros: _*)
    val fullyTransform = transform _ andThen complete
   
    JSON.parseRaw(jsonStr).map(fullyTransform)
  }

  def transform(parsed: Any): Seq[(String, Processor)] = parsed match {
    case JSONObject(obj) => obj.toList.map {
      case (key, value) => key -> element(value)
    }
    case _ =>
      Seq("element" -> element(parsed))
  }

  def element(parsed: Any): Processor = parsed match {
    case JSONArray(list) => Foreach(list)(transform)
    case _ => Value(parsed)
  }
}
