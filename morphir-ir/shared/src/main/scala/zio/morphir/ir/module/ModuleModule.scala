package zio.morphir.ir.module

trait ModuleModule {
  final type Specification[+TA] = zio.morphir.ir.module.Specification[TA]
  val Specification: zio.morphir.ir.module.Specification.type

  final def emptySpecifiction: Specification[Nothing] = Specification.empty
}
