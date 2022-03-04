package zio.morphir.json
import zio.Chunk
import zio.json._
import zio.morphir.ir.{MorphirIRVersion, Name}
import zio.morphir.ir.ValueModule.Value
import zio.morphir.ir.codec._


trait JsonSupport {
  implicit val unitEncoder:JsonEncoder[Unit] = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder:JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)

  implicit def valueEncoder[Annotations](implicit annotationsEncoder:JsonEncoder[Annotations]):MorphirEncoder[Value[Annotations]] = new MorphirEncoder[Value[Annotations]] {
    import zio.morphir.ir.ValueModule.ValueCase._
    override def encode(value: Value[Annotations])(implicit options: CodecOptions): CharSequence = options.morphirIRVersion match {
      case MorphirIRVersion.V1_0 => value.fold[CharSequence]{
        case UnitCase => ("unit", ).toJson
        case _ => ???
      }
    }
  }
}

object JsonSupport extends JsonSupport

