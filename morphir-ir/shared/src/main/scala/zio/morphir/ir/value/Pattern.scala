package zio.morphir.ir.value

import zio.{Chunk}
import zio.morphir.ir.ZEnvironmentSubset
import zio.prelude.AnyType
import zio.morphir.ir.{FQName, Literal, Name, UType}

sealed trait Pattern[+A] { self =>
  import Pattern._

  def attributes: ZEnvironmentSubset[AnyType, A]
  final def mapAttributes[B](f: ZEnvironmentSubset[AnyType, A] => ZEnvironmentSubset[AnyType, B]): Pattern[B] = self match {
    case AsPattern(pattern, name, attributes) => AsPattern(pattern.mapAttributes(f), name, f(attributes))
    case ConstructorPattern(constructorName, argumentPatterns, attributes) =>
      ConstructorPattern(constructorName, argumentPatterns.map(_.mapAttributes(f)), f(attributes))
    case EmptyListPattern(attributes) => EmptyListPattern(f(attributes))
    case HeadTailPattern(headPattern, tailPattern, attributes) =>
      HeadTailPattern(headPattern.mapAttributes(f), tailPattern.mapAttributes(f), f(attributes))
    case LiteralPattern(literal, attributes) => LiteralPattern(literal, f(attributes))
    case TuplePattern(elementPatterns, attributes) =>
      TuplePattern(elementPatterns.map(_.mapAttributes(f)), f(attributes))
    case UnitPattern(attributes)     => UnitPattern(f(attributes))
    case WildcardPattern(attributes) => WildcardPattern(f(attributes))
  }

  def withAttributes[B](attributes: => ZEnvironmentSubset[AnyType, B]): Pattern[B] =
    self.mapAttributes(_ => attributes)
}

object Pattern {
  type DefaultAttributes = ZEnvironmentSubset[AnyType, Any]
  val DefaultAttributes: DefaultAttributes = ZEnvironmentSubset.empty

  def asPattern[Attributes](
      attributes: ZEnvironmentSubset[AnyType, Attributes],
      pattern: Pattern[Attributes],
      name: Name
  ): AsPattern[Attributes] =
    AsPattern(pattern, name, attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    AsPattern(pattern, name, DefaultAttributes)

  def asPattern(name: Name): UPattern =
    AsPattern(wildcardPattern, name, DefaultAttributes)

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern(DefaultAttributes)

  def wildcardPattern[Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]): WildcardPattern[Attributes] =
    WildcardPattern(attributes)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]): UnitPattern[Attributes] = UnitPattern(attributes)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]): Wildcard[Attributes] = Wildcard(attributes)

  // final case class LiteralPattern[+Attributes, +Value](value: Lit[Value], attributes: ZEnvironmentSubset[AnyType, Attributes])
  //     extends Pattern[Attributes]

  final case class AsPattern[+Attributes](
      pattern: Pattern[Attributes],
      name: Name,
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ) extends Pattern[Attributes]

  final case class ConstructorPattern[+Attributes](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Attributes]],
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ) extends Pattern[Attributes]

  final case class EmptyListPattern[+Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]) extends Pattern[Attributes]

  final case class HeadTailPattern[+Attributes](
      headPattern: Pattern[Attributes],
      tailPattern: Pattern[Attributes],
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ) extends Pattern[Attributes]

  final case class LiteralPattern[+A, +Attributes](
      literal: Literal[A],
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ) extends Pattern[Attributes]

  object LiteralPattern {
    type Typed[+A] = LiteralPattern[A, UType]
    object Typed {
      def apply[A](literal: Literal[A])(ascribedType: UType): LiteralPattern[A, UType] =
        LiteralPattern(literal, ZEnvironmentSubset[AnyType, UType](ascribedType))
    }
  }

  final case class TuplePattern[+Attributes](
      elementPatterns: Chunk[Pattern[Attributes]],
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ) extends Pattern[Attributes]

  final case class UnitPattern[+Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]) extends Pattern[Attributes]

  final case class WildcardPattern[+Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]) extends Pattern[Attributes]

  final implicit class UPatternExtensions(val self: Pattern[Any]) extends AnyVal {
    def :@(ascribedType: ZEnvironmentSubset[AnyType, UType]): Pattern[UType] = self.mapAttributes(_ => ascribedType)
  }
}
