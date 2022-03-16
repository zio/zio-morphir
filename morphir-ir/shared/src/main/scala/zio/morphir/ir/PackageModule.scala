package zio.morphir.ir

import zio.morphir.ir.ModuleModule.{Specification => ModuleSpec, Definition => ModuleDef}

object PackageModule {

  val emptySpecification: Specification[Any] = Specification.empty

  final case class Definition[+Annotations](
      modules: Map[ModuleModule.ModuleName, AccessControlled[ModuleDefinition[Annotations]]]
  ) { self =>
    def toSpecification: Specification[Annotations] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPublicAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def toSpecificationWithPrivate: Specification[Annotations] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPrivateAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    // lookupModuleDefinition : Path -> Definition ta va -> Maybe (Module.Definition ta va)
    def lookupModuleDefinition(path: Path): List[Option[ModuleDef[Annotations]]] = {
      modules.map { case (key, value) =>
        if (key.namespace == path) Some(value.withPrivateAccess)
        else None
      }.toList
    }

    def lookupTypeDefinition(path: Path, name: Name): Option[ModuleDef[Annotations]] =
      modules.get(ModuleName(path, name)).map(_.withPrivateAccess)

    def lookupTypeDefinition(moduleName: ModuleName): Option[ModuleDef[Annotations]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def mapDefinitionAttributes[B](func: Annotations => B): Definition[B] = ???

  }

  object Definition {
    def empty[Annotations]: Definition[Annotations] = Definition(Map.empty)
  }

  final case class Specification[+Annotations](modules: Map[ModuleName, ModuleSpec[Annotations]]) {
    self =>

    // lookupValueDefinition : Path -> Name -> Definition ta va -> Maybe (Value.Definition ta va)
    def lookupModuleSpecification(path: Path): List[Option[ModuleSpec[Annotations]]] = {
      modules.map { case (key, value) =>
        if (key.namespace == path) Some(value)
        else None
      }.toList
    }

    def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[Annotations]] =
      modules.get(ModuleName(path, name))

    def lookupTypeSpecification(moduleName: ModuleName): Option[ModuleSpec[Annotations]] =
      modules.get(moduleName)

    def mapSpecificationAttributes[B](func: Annotations => B): Specification[B] = ???

  }

  object Specification {
    val empty: Specification[Any] = Specification(Map.empty)
  }

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName =
      FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
}

trait PackageSpecFor[A] {
  import PackageModule.*

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
