package zio.morphir.sexpr

sealed abstract class SExprError

object SExprError {
  final case class Message(text: String) extends SExprError

  final case class IndexedAccess(index: Int) extends SExprError

  final case class ObjectAccess(field: String) extends SExprError

  def render(trace: List[SExprError]): String =
    trace.reverse.map {
      case Message(text)        => s"($text)"
      case IndexedAccess(index) => s"[$index]"
      case ObjectAccess(field)  => s".$field"
    }.mkString
}
