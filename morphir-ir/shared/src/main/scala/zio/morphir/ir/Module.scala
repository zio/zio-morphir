package zio.morphir.ir

import zio.morphir.ir.IR.ModuleDefinition

object Module {

  type Definition[+Annotations] = IR.ModuleDefinition[Annotations]
  val Definition = IR.ModuleDefinition

  type Specification[+Annotations] = IR.ModuleSpecification[Annotations]
  val Specification = IR.ModuleSpecification

  lazy val emptyDefinition: ModuleDefinition[Any] = Definition.empty

  final case class ModuleName(namespace: Path, localName: Name) {
    lazy val toPath = namespace / localName
  }

  final case class QualifiedModuleName(packageName: Path, module: Path) {
    lazy val toPath = packageName / module
  }

}
