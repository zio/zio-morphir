package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.{Name, NativeFunction}

import scala.annotation.tailrec

sealed trait Value[+TA, +VA] { self =>
  def attributes: VA

  def foldLeft[Z](initial:Z)(f: (Z, Value[TA,VA]) => Z): Z = {
    import Value.{Unit => UnitType, _}
    @tailrec
    def loop(stack: List[Value[TA, VA]], acc: Z): Z =
      stack match {
        case Nil => acc
        case (t @ UnitType(_)) :: tail => loop(tail, f(acc, t))
      }

    loop(List(self), initial)
  }

}

object Value {

  final case class NativeApply[+TA,+VA](attributes:VA, function: NativeFunction, arguments: Chunk[Value[TA,VA]]) extends Value[TA,VA]
  object NativeApply {
    type Raw = NativeApply[scala.Unit, scala.Unit]
  }
  final case class Unit[+VA](attributes:VA) extends Value[Nothing, VA]
  object Unit {
    type Raw = Unit[scala.Unit]
    def apply:Raw = Unit(scala.Unit)
  }
  final case class Variable[+VA](attributes:VA, name: Name) extends Value[Nothing, VA]
  object Variable {
    type Raw = Variable[scala.Unit]
  }
}
