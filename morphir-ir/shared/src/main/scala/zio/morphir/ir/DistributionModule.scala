package zio.morphir.ir

import zio.morphir.ir.TypeModule.Specification.TypeAliasSpecification
import zio.morphir.ir.TypeModule.TypeCase.ReferenceCase
import zio.morphir.ir.TypeModule.{Type, USpecification}
import zio.morphir.ir.ValueModule.ValueDefinition

object DistributionModule {
  sealed trait Distribution
  object Distribution {
    final case class Library(
        packageName: PackageName,
        dependencies: Map[PackageName, UPackageSpecification],
        packageDef: PackageDefinition[UType]
    ) extends Distribution { self =>

      def lookupModuleSpecification(packageName: PackageName, module: ModuleName): Option[UModuleSpecification] =
        self match {
          case Library(`packageName`, _, packageDef) =>
            packageDef.toSpecification.lookupModuleSpecification(module)
          case Library(_, dependencies, _) =>
            dependencies.get(packageName).flatMap(_.lookupModuleSpecification(module))
        }

      def lookupModuleSpecification(packageName: PackageName, path: Path): Option[UModuleSpecification] =
        lookupModuleSpecification(packageName, ModuleName.fromPath(path))

      def lookupTypeSpecification(packageName: PackageName, module: ModuleName): Option[USpecification] =
        lookupModuleSpecification(packageName, module).flatMap(_.lookupType(module.localName))

      def lookupBaseTypeName(fqName: FQName): Option[FQName] = {
        for {
          spec     <- lookupModuleSpecification(fqName.packagePath, fqName.modulePath.toPath)
          typeSpec <- spec.lookupType(fqName.localName)
          result <- typeSpec match {
            case TypeAliasSpecification(_, Type(ReferenceCase(aliasFQName, _), _)) =>
              lookupBaseTypeName(aliasFQName)
            case _ => Some(fqName)
          }
        } yield result
      }

      def lookupValueSpecification(
          packageName: PackageName,
          moduleName: ModuleName
      ): Option[ValueModule.USpecification] =
        lookupModuleSpecification(packageName, moduleName).flatMap(_.lookupValue(moduleName.localName))

      def lookupValueDefinition(qName: QName): Option[ValueDefinition[UType]] =
        self match {
          case Library(_, _, packageDef) =>
            packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupValue(qName.localName))
        }

      def lookupPackageSpecification: UPackageSpecification = self match {
        case Library(_, _, packageDef) =>
          packageDef.toSpecificationWithPrivate.mapSpecificationAttributes(_ => ())
      }

      def lookupPackageName: PackageName = self match {
        case Library(packageName, _, _) =>
          packageName
      }
    }
  }

}
