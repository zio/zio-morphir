package zio.morphir.ir
import zio.morphir.ir.PackageModule.PackageName

object DistributionModule {
  sealed trait Distribution
  object Distribution {
    final case class Library(packageName: PackageName)
  }
}
