package zio.morphir.ir.codec

import zio.Chunk
import zio.morphir.ir.{MorphirIRVersion, PackageDefinition}

trait MorphirEncoder[A] {
  def encode(value: A)(implicit options:CodecOptions): CharSequence
}

trait MorphirDecoder[A] {
  def decode(input: CharSequence)(implicit options:CodecOptions): Either[DecodeError, A]
}

trait MorphirCodec[A] extends MorphirEncoder[A] with MorphirDecoder[A]
trait DecodeError

trait MorphirIREncoder {
  def encodePackageDef[Annotations](value: PackageDefinition[Annotations]): CharSequence
  def enc
}

final case class CodecOptions(morphirIRVersion:MorphirIRVersion)
object CodecOptions {
  implicit val default: CodecOptions = CodecOptions(MorphirIRVersion.Default)

}


