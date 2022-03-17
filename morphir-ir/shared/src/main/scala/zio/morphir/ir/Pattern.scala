package zio.morphir.ir

import zio.Chunk

sealed trait Pattern[+Annotations] { self =>
  import Pattern._

  def annotations: Annotations
  final def mapAttributes[B](f: Annotations => B): Pattern[B] = self match {
    case AsPattern(pattern, name, annotations) => AsPattern(pattern.mapAttributes(f), name, f(annotations))
    case ConstructorPattern(constructorName, argumentPatterns, annotations) => ConstructorPattern(constructorName, argumentPatterns.map(_.mapAttributes(f)), f(annotations))
    case EmptyListPattern(annotations) => EmptyListPattern(f(annotations))
    case HeadTailPattern(headPattern, tailPattern, annotations) => HeadTailPattern(headPattern.mapAttributes(f), tailPattern.mapAttributes(f), f(annotations))
    case LiteralPattern(literal, annotations) => LiteralPattern(literal, f(annotations))
    case TuplePattern(elementPatterns, annotations) => TuplePattern(elementPatterns.map(_.mapAttributes(f)), f(annotations))
    case UnitPattern(annotations) => UnitPattern(f(annotations))
    case WildcardPattern(annotations) => WildcardPattern(f(annotations))
  }
}

object Pattern {

  def asPattern[Annotations](
      annotations: Annotations,
      pattern: Pattern[Annotations],
      name: Name
  ): AsPattern[Annotations] =
    AsPattern(pattern, name, annotations)

  def asPattern(pattern: Pattern[Any], name: Name): AsPattern[Any] =
    AsPattern(pattern, name, ())

  def asPattern(name: Name): AsPattern[Any] =
    AsPattern(wildcardPattern, name, ())

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern[Any](())

  def wildcardPattern[Annotations](annotations: Annotations): WildcardPattern[Annotations] =
    WildcardPattern(annotations)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Annotations](annotations: ZEnvironment[Annotations]): UnitPattern[Annotations] = UnitPattern(annotations)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

  // final case class LiteralPattern[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
  //     extends Pattern[Annotations]

  final case class AsPattern[+Annotations](
      pattern: Pattern[Annotations],
      name: Name,
      annotations: Annotations
  ) extends Pattern[Annotations]

  final case class ConstructorPattern[+Annotations](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Annotations]],
      annotations: Annotations
  ) extends Pattern[Annotations]

  final case class EmptyListPattern[+Annotations](annotations: Annotations) extends Pattern[Annotations]

  final case class HeadTailPattern[+Annotations](
      headPattern: Pattern[Annotations],
      tailPattern: Pattern[Annotations],
      annotations: Annotations
  ) extends Pattern[Annotations]

  final case class LiteralPattern[+A, +Annotations](
      literal: Literal[A],
      annotations: Annotations
  ) extends Pattern[Annotations]

  final case class TuplePattern[+Annotations](
      elementPatterns: Chunk[Pattern[Annotations]],
      annotations: Annotations
  ) extends Pattern[Annotations]

  final case class UnitPattern[+Annotations](annotations: Annotations) extends Pattern[Annotations]

  final case class WildcardPattern[+Annotations](annotations: Annotations) extends Pattern[Annotations]

}
