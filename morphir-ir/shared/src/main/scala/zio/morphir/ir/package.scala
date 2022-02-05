package zio.morphir

package object ir {

  type LiteralValue = Literal[Nothing]
  val LiteralValue = Literal

  type ModuleName = Module.ModuleName
  val ModuleName = Module.ModuleName

  type ModulePath = naming.ModulePath
  val ModulePath = naming.ModulePath

  type PackageName = PackageModule.PackageName
  val PackageName = PackageModule.PackageName

  type ??? = Nothing
}
