package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.ZEnvironment

object Value {

  sealed trait Pattern[+Annotations] {
    def annotations: ZEnvironment[Annotations]
  }
  object Pattern {
    val unit: Unit[Any]                                                              = Unit(ZEnvironment.empty)
    def unit[Annotations](annotations: ZEnvironment[Annotations]): Unit[Annotations] = Unit(annotations)
    val wildcard: Wildcard[Any]                                                      = Wildcard(ZEnvironment.empty)
    def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

    final case class Literal[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
        extends Pattern[Annotations]

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations])     extends Pattern[Annotations]
    final case class Wildcard[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]
  }
}
