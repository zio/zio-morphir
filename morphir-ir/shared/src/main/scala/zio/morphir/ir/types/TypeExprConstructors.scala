package zio.morphir.ir.types

import zio.{=!=, Chunk}
import zio.morphir.ir._

trait TypeExprConstructors { self =>
  import TypeCase._
  import TypeExpr.{FieldT, Type}
  // Extensible record constructors

  final def extensibleRecord[A](attributes: A, name: Name, fields: Chunk[FieldT[A]]): TypeExpr[A] =
    TypeExpr(ExtensibleRecordCase(attributes, name, fields))

  final def extensibleRecord[A](attributes: A, name: String, fields: Chunk[FieldT[A]]): TypeExpr[A] =
    extensibleRecord(attributes, Name.fromString(name), fields)

  final def extensibleRecord[A](attributes: A, name: String, field: FieldT[A], fields: FieldT[A]*): TypeExpr[A] =
    extensibleRecord(attributes, Name.fromString(name), field +: Chunk.fromIterable(fields))

  final def extensibleRecord[A](attributes: A, name: Name, fields: (String, TypeExpr[A])*): TypeExpr[A] = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) =>
      Field(Name.fromString(name), typeExpr)
    })
    TypeExpr(ExtensibleRecordCase(attributes, name, fieldsChunk))
  }

  final def extensibleRecord[A](attributes: A, name: String, fields: (String, TypeExpr[A])*): TypeExpr[A] =
    extensibleRecord(attributes, Name.fromString(name), fields: _*)

  // Function constructors
  final def function[A](attributes: A, paramTypes: Chunk[TypeExpr[A]], returnType: TypeExpr[A]): TypeExpr[A] =
    TypeExpr(FunctionCase(attributes, paramTypes, returnType))

  final def record[A](attributes: A, fields: Chunk[Field[TypeExpr[A]]]): TypeExpr[A] =
    TypeExpr(RecordCase(attributes, fields))

  final def reference[A](attributes: A, typeName: FQName, typeParams: Chunk[TypeExpr[A]]): TypeExpr[A] =
    TypeExpr(ReferenceCase(attributes, typeName, typeParams))

  // Tuple constructors
  final def emptyTuple[A](attributes: A): TypeExpr[A] =
    TypeExpr(TupleCase(attributes, Chunk.empty))

  final def tuple[A](attributes: A, elements: Chunk[TypeExpr[A]]): TypeExpr[A] =
    TypeExpr(TupleCase(attributes, elements))

  final def tuple[A](attributes: A, elements: TypeExpr[A]*)(implicit ev: A =!= TypeExpr[A]): TypeExpr[A] =
    tuple(attributes, Chunk.fromIterable(elements))

  final def unit[A](attributes: A): TypeExpr[A] = TypeExpr(UnitCase(attributes))

  // Variable constructors
  final def variable[A](attributes: A, name: Name): TypeExpr[A] =
    TypeExpr(VariableCase(attributes, name))

  final def variable[A](attributes: A, name: String): TypeExpr[A] =
    TypeExpr(VariableCase(attributes, Name.fromString(name)))

}
