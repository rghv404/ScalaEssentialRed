sealed trait Expression {
  def print: String = {
    def SeqToString(seq: SeqCell): String = seq match {
      // base case
      case SeqCell(h, SeqEnd) => h.print
      // recursive case
      case SeqCell(h, t: SeqCell) => s"${h.print},  ${SeqToString(t)}"
    }

    def ObjToString(obj: ObjectCell): String = obj match {
      case ObjectCell(key, value, ObjectEnd) => s"$key: ${value.print}"
      case ObjectCell(key, value, cell: ObjectCell) => s"$key: ${value.print} , ${ObjToString(cell)}"
    }

    this match {
      case JsString(a) => s"$a"
      case JsNumber(a) => s"$a"
      case JsBoolean(a) => s"$a"
      case o@SeqCell(_, _)=> s"[  ${SeqToString(o)}  ]"
      case SeqEnd => "[]"
      case o @ ObjectCell(_, _, _) => s"{  ${ObjToString(o)}  }"
      case ObjectEnd => "{}"
    }
  }
}
//final case class Pair(key: String, value: SeqCell) extends Expression
final case object SeqEnd extends Expression
final case object ObjectEnd extends Expression
final case class SeqCell(head: Expression, tail: Expression) extends Expression
final case class ObjectCell(key: String, value: Expression, tail: Expression) extends Expression

final case class JsString(value: String) extends Expression
final case class JsNumber(value: Double) extends Expression
final case class JsBoolean(value: Boolean) extends Expression

SeqCell(JsString("a string"), SeqCell(JsNumber(1.0), SeqCell(JsBoolean (true), SeqEnd))).print
// res0: String = ["a string", 1.0, true]
ObjectCell(
  "a", SeqCell(JsNumber(1.0), SeqCell(JsNumber(2.0), SeqCell(JsNumber
  (3.0), SeqEnd))), ObjectCell(
    "b", SeqCell(JsString("a"), SeqCell(JsString("b"), SeqCell( JsString("c"), SeqEnd))),
    ObjectCell(
      "c", ObjectCell("doh", JsBoolean(true),
        ObjectCell("ray", JsBoolean(false), ObjectCell("me", JsNumber(1.0), ObjectEnd))),
      ObjectEnd )
  ) ).print