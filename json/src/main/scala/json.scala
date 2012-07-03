package lmxml
package transforms
package json

import util.parsing.json._

object JSTransform {
  type Filter = (Any => Boolean)

  JSON.perThreadNumberParser = { (input: String) =>
    if (input.contains(".")) input.toDouble else input.toInt
  }

  object Filters {
    def onJSON(fun: JSONType => Boolean) = fun.asInstanceOf[Filter]

    def onArray(fun: List[Any] => Boolean) =
      onJSON(j => fun(j.asInstanceOf[JSONArray].list))

    def onObject(fun: Map[String, Any] => Boolean) =
      onJSON(j =>
        // required for scala 2.8.x
        fun(j.asInstanceOf[JSONObject].obj.asInstanceOf[Map[String, Any]])
      )

    def onValue(fun: String => Boolean) = fun.asInstanceOf[Filter]
  }

  def apply(filters: (String, Filter)*) = new JSTransform(Map(filters :_*))

  def apply(jsonStr: String): Option[Transform] = apply().get(jsonStr)
}

case class JSTransform(filters: Map[String, JSTransform.Filter]) {

  def parse(jsonStr: String) = get(jsonStr).getOrElse(Transform())

  def get(jsonStr: String) = {
    val complete = (pros: Seq[(String,Processor)]) => Transform(pros: _*)
    val fullyTransform = transform _ andThen complete

    JSON.parseRaw(jsonStr).map(fullyTransform)
  }

  def tupleFilter(key: String, value: Any) = key ->
    filters.get(key).map(applyFilter(_, value)).getOrElse(element(value))

  def applyFilter(filter: JSTransform.Filter, value: Any) =
    if (filter(value)) element(value) else Empty

  def transform(parsed: Any): Seq[(String, Processor)] = parsed match {
    case JSONObject(obj) => obj.toList.flatMap(transform)
    case (key: String, value) => Seq(tupleFilter(key, value))
    case _ => Seq("element" -> element(parsed))
  }

  def element(parsed: Any): Processor = parsed match {
    case obj: JSONObject => Values(transform(obj))
    case JSONArray(list) => Foreach(list)(transform)
    case _ => Value(parsed)
  }
}
