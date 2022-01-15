package zio.morphir.io
import zio.prelude.*

object predef {
  object VFilePath extends Newtype[String] {
    implicit class VFilePathExtensions(private val self: VFilePath) extends AnyVal {
      def /(child: VFilePath): VFilePath = wrap(unwrap(self) + "/" + unwrap(child))
    }
  }
  type VFilePath = VFilePath.Type
}
