package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.{Literal => Lit, FQName, Name}
import zio.morphir.ir.Type.UType
import zio.morphir.ir.value.Pattern

final case class Value[+TA, +VA](caseValue: ValueCase[TA, VA, Value[TA, VA]]) { self =>
  def attributes: VA = caseValue.attributes
}

object Value {
  import ValueCase._

  type RawValue = Value[Any, Any]
  val RawValue: Value.type = Value

  type TypedValue = Value[Any, UType]
  val TypedValue: Value.type = Value

  def apply[TA, VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]): Value[TA, VA] =
    Value(ApplyCase(attributes, function, argument))

  object Lambda {
    def apply[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
      Value(LambdaCase(attributes, argumentPattern, body))
  }

  object Literal {
    def apply[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] =
      Value(LiteralCase(attributes, literal))
  }
}
