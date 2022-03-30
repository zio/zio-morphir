package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.ZEnvironmentSubset
import zio.prelude.AnyType
import zio.morphir.ir.{FQName, Literal, Name, UType}

sealed trait Pattern[+Caps[_], +A] { self =>
  import Pattern._

  def attributes: ZEnvironmentSubset[Caps, A]

  final def mapAttributes[B](f: ZEnvironmentSubset[Caps, A] => ZEnvironmentSubset[Caps, B]): Pattern[Caps, B] =
    self match {
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

  def withAttributes[B](attributes: => ZEnvironmentSubset[Caps, B]): Pattern[Caps, B] =
    self.mapAttributes(_ => attributes)
}

object Pattern {
  type DefaultAttributes = ZEnvironmentSubset[AnyType, Any]
  val DefaultAttributes: DefaultAttributes = ZEnvironmentSubset.empty

  def asPattern[Caps[_], Attributes](
      attributes: ZEnvironmentSubset[Caps, Attributes],
      pattern: Pattern[Caps, Attributes],
      name: Name
  ): AsPattern[Caps, Attributes] =
    AsPattern(pattern, name, attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    AsPattern(pattern, name, DefaultAttributes)

  def asPattern(name: Name): UPattern =
    AsPattern(wildcardPattern, name, DefaultAttributes)

  lazy val wildcardPattern: WildcardPattern[AnyType, Any] = WildcardPattern(DefaultAttributes)

  def wildcardPattern[Caps[_], Attributes](
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ): WildcardPattern[Caps, Attributes] =
    WildcardPattern(attributes)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]): UnitPattern[Attributes] = UnitPattern(attributes)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Attributes](attributes: ZEnvironmentSubset[AnyType, Attributes]): Wildcard[Attributes] = Wildcard(attributes)

  // final case class LiteralPattern[+Attributes, +Value](value: Lit[Value], attributes: ZEnvironmentSubset[AnyType, Attributes])
  //     extends Pattern[Attributes]

  final case class AsPattern[+Caps[_], +Attributes](
      pattern: Pattern[Caps, Attributes],
      name: Name,
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ) extends Pattern[Caps, Attributes]

  final case class ConstructorPattern[+Caps[_], +Attributes](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Caps, Attributes]],
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ) extends Pattern[Caps, Attributes]

  final case class EmptyListPattern[+Caps[_], +Attributes](attributes: ZEnvironmentSubset[Caps, Attributes])
      extends Pattern[Caps, Attributes]

  final case class HeadTailPattern[+Caps[_], +Attributes](
      headPattern: Pattern[Caps, Attributes],
      tailPattern: Pattern[Caps, Attributes],
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ) extends Pattern[Caps, Attributes]

  final case class LiteralPattern[+Caps[_], +A, +Attributes](
      literal: Literal[A],
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ) extends Pattern[Caps, Attributes]

  object LiteralPattern {
    type Typed[+Caps[_], +A] = LiteralPattern[Caps, A, UType]
    object Typed {
      def apply[Caps[_], A](literal: Literal[A])(ascribedType: UType)(implicit
          capibilities: Caps[UType]
      ): LiteralPattern[Caps, A, UType] =
        LiteralPattern(literal, ZEnvironmentSubset[Caps, UType](ascribedType))

      def apply[Caps[_], A](
          attributes: ZEnvironmentSubset[Caps, UType],
          literal: Literal[A]
      ): LiteralPattern[Caps, A, UType] =
        LiteralPattern(literal, attributes)
    }
  }

  final case class TuplePattern[+Caps[_], +Attributes](
      elementPatterns: Chunk[Pattern[Caps, Attributes]],
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ) extends Pattern[Caps, Attributes]

  final case class UnitPattern[+Caps[_], +Attributes](attributes: ZEnvironmentSubset[Caps, Attributes])
      extends Pattern[Caps, Attributes]

  final case class WildcardPattern[+Caps[_], +Attributes](attributes: ZEnvironmentSubset[Caps, Attributes])
      extends Pattern[Caps, Attributes]

  final implicit class UPatternExtensions[+Caps[_]](val self: Pattern[Caps, Any]) extends AnyVal {
    def :@(ascribedType: ZEnvironmentSubset[Caps, UType]): Pattern[Caps, UType] = self.mapAttributes(_ => ascribedType)
  }
}
