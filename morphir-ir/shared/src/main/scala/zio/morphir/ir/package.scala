package zio.morphir

import zio.morphir.sdk.ResultModule

package object ir {

  type LiteralValue = Literal[Any]
  val LiteralValue: Literal.type = Literal

  type PackageName = PackageModule.PackageName
  val PackageName: PackageModule.PackageName.type = PackageModule.PackageName

  type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  val PackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type Result[+E, +A] = zio.morphir.sdk.ResultModule.Result[E, A]
  val Result: ResultModule.Result.type = zio.morphir.sdk.ResultModule.Result

  type UPackageSpecification = PackageModule.Specification[Any]
  val UPackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type ??? = Nothing

  final implicit class StringToFieldOps(private val self: String) extends AnyVal {
    import zio.morphir.ir.types.nonrecursive
    import zio.morphir.ir.types.recursive

    def as[A](tpe: recursive.Type[A]): recursive.Field[recursive.Type[A]] =
      recursive.Field(Name.fromString(self), tpe)

    def as[A](tpe: nonrecursive.Type[A]): nonrecursive.Field[nonrecursive.Type[A]] =
      nonrecursive.Field(Name.fromString(self), tpe)
  }
}
