package zio.morphir

import zio.morphir.sdk.ResultModule

package object ir {

  type LiteralValue = Literal[Any]
  val LiteralValue: Literal.type = Literal

  type PackageDefinition[+Annotations] = PackageModule.Definition[Annotations]
  val PackageDefinition: PackageModule.Definition.type = PackageModule.Definition

  type PackageName = PackageModule.PackageName
  val PackageName: PackageModule.PackageName.type = PackageModule.PackageName

  type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  val PackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type Result[+E, +A] = zio.morphir.sdk.ResultModule.Result[E, A]
  val Result: ResultModule.Result.type = zio.morphir.sdk.ResultModule.Result

  type UPackageDefinition = PackageModule.Definition[Any]
  val UPackageDefinition: PackageModule.Definition.type = PackageModule.Definition

  type UPackageSpecification = PackageModule.Specification[Any]
  val UPackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type ??? = Nothing
}
