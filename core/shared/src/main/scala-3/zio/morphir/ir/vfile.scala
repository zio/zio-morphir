package zio.morphir.ir

import zio.Chunk
import zio.prelude.*
import scala.language.postfixOps

/** This is meant to pretty much be an implementation of this: https://www.npmjs.com/package/vfile#vfiledata
  */
object vfile:
  sealed trait VFile[+Props] { self =>
    import VFile.*
    import VFileCase.*
    def $case: VFileCase[Props, VFile[Props]]
  }
  object VFile:
    final case class File[+Props]($case: VFileCase[Props, VFile[Props]]) extends VFile[Props] {}

    sealed trait VFileCase[+Model, +A]:
      self =>
      import VFileCase.*
      def map[B](f: A => B): VFileCase[Model, B] = self match
        case c @ DataCase(_)       => DataCase(c.data)
        case c @ FileCase(_, _)    => FileCase(c.children.map(f), f(c.content))
        case c @ LineCase(_, text) => LineCase(c.index, text)

    object VFileCase:
      final case class FileCase[+A](children: Chunk[A], content: A) extends VFileCase[Nothing, A]
      final case class DataCase[+Props](data: Props)                extends VFileCase[Props, Nothing]
      final case class LineCase[+Props](index: Int, text: String)   extends VFileCase[Props, Nothing]

  end VFile

  final case class VFileMessage(reason: VFileMessage.Reason)
  object VFileMessage:
    enum Reason:
      case Description(text: String)
      case Error(value: Throwable)
  end VFileMessage

  object LineNumber extends Newtype[Int]:
    import zio.prelude.Assertion.*

    extension (self: LineNumber) def ++ = wrap(unwrap(self) + 1)

    val default: LineNumber       = wrap(1)
    override inline def assertion = greaterThanOrEqualTo(1)

  type LineNumber = LineNumber.Type

  opaque type ColumnNumber = Int

  final case class Offset(line: Option[LineNumber], column: Option[ColumnNumber])
  object Offset:
    def apply(line: LineNumber)                       = new Offset(Some(line), None)
    def apply(line: LineNumber, column: ColumnNumber) = new Offset(Some(line), Some(column))
    object Empty:
      def apply: Offset = Offset(None, None)
      def unapply(offset: Offset): Option[Unit] = offset match
        case Offset(None, None) => Some(())
        case _                  => None

  final case class Position(start: Option[Offset], end: Option[Offset])
  object Position:
    def apply(start: Offset) = new Position(Some(start), None)
