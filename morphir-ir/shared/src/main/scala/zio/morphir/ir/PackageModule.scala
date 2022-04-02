package zio.morphir.ir

import zio.morphir.ir.Module.{ModuleName, ModulePath, Specification => ModuleSpec, Definition => ModuleDef}

object PackageModule {

  val emptySpecification: Specification[Any] = Specification.empty

  final case class Definition[+TA, +VA](
      modules: Map[ModuleName, AccessControlled[ModuleDef[TA, VA]]]
  ) { self =>
    def toSpecification: Specification[TA] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPublicAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def toSpecificationWithPrivate: Specification[TA] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPrivateAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def lookupModuleDefinition(path: Path): Option[ModuleDef[TA, VA]] = lookupModuleDefinition(
      ModuleName.fromPath(path)
    )

    def lookupModuleDefinition(moduleName: ModuleName): Option[ModuleDef[TA, VA]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def lookupTypeDefinition(path: Path, name: Name): Option[ModuleDef[TA, VA]] =
      lookupTypeDefinition(ModuleName(path, name))

    def lookupTypeDefinition(moduleName: ModuleName): Option[ModuleDef[TA, VA]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def mapDefinitionAttributes[TB, VB](tf: TA => VB, vf: VA => VB): Definition[TB, VB] = ???

  }

  object Definition {
    val empty: Definition[Nothing, Nothing] = Definition(Map.empty)
  }

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
    val empty: Specification[Any] = Specification(Map.empty)
  }

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName =
      FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }

  object PackageName {
    def fromString(input: String): PackageName = PackageName(Path.fromString(input))
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
}

trait PackageSpecFor[A] {
  import PackageModule._

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
