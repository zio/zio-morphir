package zio.morphir

package object ir {

  type LiteralValue = Literal[Any]
  val LiteralValue = Literal

  type ModuleName = Module.ModuleName
  val ModuleName = Module.ModuleName

  type ModulePath = Module.ModulePath
  val ModulePath = Module.ModulePath

  type PackageName = PackageModule.PackageName
  val PackageName = PackageModule.PackageName

  type ??? = Nothing
}
