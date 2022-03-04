package zio.morphir.json
import zio.ZEnvironment
import zio.json._
import zio.json.ast.Json
import zio.json.internal.Write
import zio.morphir.ir.{FQName}
import zio.morphir.ir.TypeModule.{Field, Type, TypeCase}
import zio.morphir.ir.ValueModule.ValueCase.UnitCase
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.{ModuleDefinition, Name, PackageDefinition}

trait MorphirJsonCodecV1 {

  // NOTE: We will want to create JSON encoders which follow the format in the morphir-elm project
  implicit val unitEncoder: JsonEncoder[Unit]     = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder: JsonEncoder[Name]     = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val fqNameEncoder: JsonEncoder[FQName] = ???

  implicit def fieldEncoder[A](implicit
      annotationsEncoder: JsonEncoder[A]
  ): JsonEncoder[Field[A]] = ???
  // JsonEncoder.list[(String, Json)]
  //   .contramap(field => field.name.toList.map(name => (name, field.value.toJson)))

  implicit def valueEncoder[Annotations](implicit
      annotationsEncoder: JsonEncoder[Annotations]
  ): JsonEncoder[Value[Annotations]] = ???

  implicit def typeEncoder[Annotations](implicit
      annotationsEncoder: JsonEncoder[ZEnvironment[Annotations]]
  ): JsonEncoder[Type[Annotations]] =
    Json.encoder.contramap[Type[Annotations]] { tpe =>
      tpe.foldAnnotated[Json] {
        case (TypeCase.ExtensibleRecordCase(name, fields), annotations) =>
          Json.Arr(
            Json.Str("extensible_record"),
            toJsonAstOrThrow(annotations),
            toJsonAstOrThrow(name),
            Json.Arr(fields.map(_.toJsonAST.right.get))
          )
        case (TypeCase.FunctionCase(paramTypes, returnType), annotations) =>
          Json.Arr(Json.Str("function"), toJsonAstOrThrow(annotations), Json.Arr(paramTypes), returnType, ???)
        case (TypeCase.RecordCase(fields), annotations) =>
          Json.Arr(Json.Str("record"), toJsonAstOrThrow(annotations), Json.Arr(fields.map(_.toJsonAST.right.get)))
        case (TypeCase.ReferenceCase(typeName, typeParameters), annotations) =>
          Json.Arr(
            Json.Str("reference"),
            toJsonAstOrThrow(annotations),
            typeName.toJsonAST.right.get,
            Json.Arr(typeParameters)
          )
        case (TypeCase.TupleCase(items), annotations) =>
          Json.Arr(Json.Str("tuple"), toJsonAstOrThrow(annotations), Json.Arr(items))
        case (TypeCase.UnitCase, annotations) => Json.Arr(Json.Str("unit"), annotations.toJsonAST.right.get)
        case (TypeCase.VariableCase(name), annotations) =>
          Json.Arr(Json.Str("variable"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
      }
    }

  def encodePackageDefinition[Annotations](
      packageDefinition: PackageDefinition[Annotations]
  )(implicit annotationEncoder: JsonEncoder[Annotations]): CharSequence = {
    // TODO: Look here for hints at implementing: https://github.com/finos/morphir-elm/blob/9122b99a6f500af389bfa26c2f7c604ea64485c2/src/Morphir/IR/Package/CodecV1.elm
    ???
  }

  def encodeModuleDefinition[Annotations](moduleDef: ModuleDefinition[Annotations])(implicit
      annotationEncoder: JsonEncoder[Annotations]
  ): CharSequence = ???

  private def toJsonAstOrThrow[A](a: A)(implicit encoder: JsonEncoder[A]): Json =
    a.toJsonAST.right.get
}

object MorphirJsonCodecV1 extends MorphirJsonCodecV1
