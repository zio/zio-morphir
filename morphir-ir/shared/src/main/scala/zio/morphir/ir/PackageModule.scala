package zio.morphir.ir

import zio.morphir.ir.ModuleModule.{Specification => ModuleSpec, Definition => ModuleDef}

object PackageModule {

  val emptySpecification: Specification[Any] = Specification.empty

  final case class Definition[+Attributes](
      modules: Map[ModuleModule.ModuleName, AccessControlled[ModuleDefinition[Attributes]]]
  ) { self =>
    def toSpecification: Specification[Attributes] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPublicAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def toSpecificationWithPrivate: Specification[Attributes] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPrivateAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def lookupModuleDefinition(path: Path): Option[ModuleDef[Attributes]] = lookupModuleDefinition(
      ModuleName.fromPath(path)
    )

    def lookupModuleDefinition(moduleName: ModuleName): Option[ModuleDef[Attributes]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def lookupTypeDefinition(path: Path, name: Name): Option[ModuleDef[Attributes]] =
      lookupTypeDefinition(ModuleName(path, name))

    def lookupTypeDefinition(moduleName: ModuleName): Option[ModuleDef[Attributes]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def mapDefinitionAttributes[A, B](f1: Attributes => A, f2: Attributes => B): Definition[B] = Definition(
      modules.map { case (_, AccessControlled(_, moduleSpec)) =>
        (_, moduleSpec.mapAttributes(f1, f2))
      }.toMap
    )

  }

  object Definition {
    def empty[Attributes]: Definition[Attributes] = Definition(Map.empty)
  }

  final case class Specification[+Attributes](modules: Map[ModuleName, ModuleSpec[Attributes]]) {
    self =>

    def lookupModuleSpecification(path: Path): Option[ModuleSpec[Attributes]] =
      lookupModuleSpecification(ModuleName.fromPath(path))

    def lookupModuleSpecification(moduleName: ModuleName): Option[ModuleSpec[Attributes]] =
      modules.get(moduleName)

    def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[Attributes]] =
      lookupTypeSpecification(ModuleName(path, name))

    def lookupTypeSpecification(moduleName: ModuleName): Option[ModuleSpec[Attributes]] =
      modules.get(moduleName)

    def mapSpecificationAttributes[B](f: Attributes => B): Specification[B] = Specification(
      modules.map { case (_, moduleSpec) => (_, moduleSpec.mapAttributes(f)) }.toMap
    )

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
