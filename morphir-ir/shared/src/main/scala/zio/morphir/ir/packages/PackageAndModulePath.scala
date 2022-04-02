package zio.morphir.ir.packages

import zio.morphir.ir.{FQName, Name, Path}
import zio.morphir.ir.module._

final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
  def %(name: Name): FQName = FQName(packageName, modulePath, name)
}