package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.value.Pattern.DefaultAttributes
import zio.morphir.Not

trait PatternConstructors { self =>
  final def asPattern[A](attributes: A, pattern: Pattern[A], name: Name): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = name)

  final def asPattern[A](attributes: A, pattern: Pattern[A], name: String): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = Name.fromString(name))

  final def asPattern(pattern: UPattern, name: Name): UPattern =
    Pattern.AsPattern(attributes = DefaultAttributes, pattern = pattern, name = name)

  final def asPattern(pattern: UPattern, name: String): UPattern =
    Pattern.AsPattern(attributes = DefaultAttributes, pattern = pattern, name = Name.fromString(name))

  final def tuplePattern[A](attributes: A, patterns: Chunk[Pattern[A]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = patterns)

  final def tuplePattern[A](attributes: A, patterns: Pattern[A]*)(implicit ev: Not[A <:< Pattern[_]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = Chunk.fromIterable(patterns))

  final def tuplePattern(patterns: Chunk[UPattern]): UPattern =
    Pattern.TuplePattern(attributes = DefaultAttributes, elementPatterns = patterns)

  final def wildcardPattern[A](attributes: A): Pattern[A] = Pattern.WildcardPattern(attributes)

  final val wildcardPattern: UPattern = Pattern.WildcardPattern(Pattern.DefaultAttributes)

}
