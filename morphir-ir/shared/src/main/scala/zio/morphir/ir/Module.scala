package zio.morphir.ir

object Module {

  type Definition[+Annotations] = IR.ModuleDefinition[Annotations]
  val Definition = IR.ModuleDefinition

  type Specification[+Annotations] = IR.ModuleSpecification[Annotations]
  val Specification = IR.ModuleSpecification

  lazy val emptyDefinition: Definition[Any] = Definition.empty

  lazy val emptySpecification: Specification[Any] = Specification.empty

  final case class ModuleName(namespace: Path, localName: Name) {
    lazy val toPath = namespace / localName
  }

  final case class QualifiedModuleName(packageName: Path, module: Path) {
    lazy val toPath = packageName / module
  }

}
