package zio.morphir.ir.distribution
import zio.morphir.ir.PackageModule.PackageName

sealed trait Distribution
object Distribution {
  final case class Library(packageName: PackageName)
}
