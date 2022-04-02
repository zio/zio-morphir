package zio.morphir.ir.packages
import zio.morphir.ir.{Name, Path}
import zio.morphir.ir.module.{ModuleName, Specification => ModuleSpec}

final case class Specification[+Annotations](modules: Map[ModuleName, ModuleSpec[Annotations]]) {
  self =>

  def lookupModuleSpecification(path: Path): Option[ModuleSpec[Annotations]] =
    lookupModuleSpecification(ModuleName.fromPath(path))

  def lookupModuleSpecification(moduleName: ModuleName): Option[ModuleSpec[Annotations]] =
    modules.get(moduleName)

  def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[Annotations]] =
    lookupTypeSpecification(ModuleName(path, name))

  def lookupTypeSpecification(moduleName: ModuleName): Option[ModuleSpec[Annotations]] =
    modules.get(moduleName)

  def mapSpecificationAttributes[B](func: Annotations => B): Specification[B] = ???

}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty)
}
