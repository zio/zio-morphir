package zio.morphir.sexpr.ast

import zio.Chunk
import zio.morphir.sexpr.ast.SExprCase.BoolCase
import zio.morphir.sexpr.ast.SExprCase.StrCase
import zio.morphir.sexpr.ast.SExprCase.NumCase
import zio.morphir.sexpr.ast.SExprCase.NilCase
import zio.morphir.sexpr.ast.SExprCase.ConsCase
import zio.morphir.sexpr.ast.SExprCase.QuotedCase
import zio.morphir.sexpr.ast.SExprCase.VectorCase

sealed trait SExpr { self =>
  def $case: SExprCase[SExpr]

  def fold[Z](f: SExprCase[Z] => Z): Z = self.$case match {
    case c @ BoolCase(_)    => f(c)
    case c @ StrCase(_)     => f(c)
    case c @ NumCase(_)     => f(c)
    case NilCase            => f(NilCase)
    case ConsCase(car, cdr) => f(ConsCase(car.fold(f), cdr.fold(f)))
    case QuotedCase(value)  => f(QuotedCase(value.fold(f)))
    case VectorCase(items)  => f(VectorCase(items.map(_.fold(f))))
  }
}

object SExpr {
  import SExprCase.*

  def bool(value: Boolean): Bool     = Bool(value)
  def vector(items: SExpr*): SVector = SVector(Chunk(items: _*))

  final case class Bool private[sexpr] ($case: BoolCase) extends SExpr
  object Bool {
    def apply(value: Boolean): Bool          = Bool(BoolCase(value))
    def unapply(arg: SExpr): Option[Boolean] = arg.$case match {
      case BoolCase(value) => Some(value)
      case _               => None
    }
  }

  final case class SVector private[sexpr] ($case: VectorCase[SExpr]) extends SExpr
  object SVector {
    def apply(items: Chunk[SExpr]): SVector       = SVector(VectorCase(items))
    def unapply(arg: SExpr): Option[Chunk[SExpr]] = arg.$case match {
      case VectorCase(items) => Some(items)
      case _                 => None
    }
  }
}

sealed trait SExprCase[+A] { self =>
  import SExprCase.*
  def map[B](f: A => B): SExprCase[B] = self match {
    case BoolCase(value)    => BoolCase(value)
    case ConsCase(car, cdr) => ConsCase(f(car), f(cdr))
    case StrCase(value)     => StrCase(value)
    case NilCase            => NilCase
    case NumCase(value)     => NumCase(value)
    case QuotedCase(value)  => QuotedCase(f(value))
    case VectorCase(items)  => VectorCase(items.map(f))
  }

}
object SExprCase {
  sealed trait AtomCase[+A]       extends SExprCase[A]
  sealed trait CollectionCase[+A] extends SExprCase[A]
  sealed trait ListCase[+A]       extends CollectionCase[A]
  sealed trait SymbolCase[+A]     extends AtomCase[A]

  // Leaf Cases
  final case class BoolCase(value: Boolean)   extends SymbolCase[Nothing]
  final case class StrCase(value: String)     extends AtomCase[Nothing]
  final case class NumCase(value: BigDecimal) extends AtomCase[Nothing]
  case object NilCase                         extends ListCase[Nothing]

  // Recursive Cases
  final case class ConsCase[+A](car: A, cdr: A)    extends ListCase[A]
  final case class QuotedCase[+A](value: A)        extends SExprCase[A]
  final case class VectorCase[+A](items: Chunk[A]) extends CollectionCase[A]

}
