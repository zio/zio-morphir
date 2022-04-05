package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir._

trait UnattributedTypeExprConstructors { self =>

  import TypeCase._
  import TypeExpr.Type
  final def extensibleRecord(name: Name, fields: Chunk[Field[Type]]): Type =
    TypeExpr(ExtensibleRecordCase((), name, fields))

  final def extensibleRecord(name: String, fields: Chunk[Field[Type]]): Type =
    TypeExpr(ExtensibleRecordCase((), Name.fromString(name), fields))

  final def extensibleRecord(name: Name, fields: (String, Type)*): Type = {
    val fieldsChunk = Chunk.fromIterable(fields.map { case (name, typeExpr) => Field(Name.fromString(name), typeExpr) })
    TypeExpr(ExtensibleRecordCase((), name, fieldsChunk))
  }

  final def extensibleRecord(name: String, fields: (String, Type)*): Type =
    self.extensibleRecord(Name.fromString(name), fields: _*)

  final def extensibleRecordWithFields(name: Name, fields: Field[Type]*): Type =
    TypeExpr(ExtensibleRecordCase((), name, Chunk.fromIterable(fields)))

  final def extensibleRecordWithFields(name: String, fields: Field[Type]*): Type =
    TypeExpr(ExtensibleRecordCase((), Name.fromString(name), Chunk.fromIterable(fields)))

  final def tuple(elements: Type*): Type =
    TypeExpr(TupleCase((), Chunk.fromIterable(elements)))

  final def tuple(elements: Chunk[Type]): Type =
    TypeExpr(TupleCase((), elements))

  final def variable(name: Name): Type =
    TypeExpr(VariableCase((), name))

  final def variable(name: String): Type =
    TypeExpr(VariableCase((), Name.fromString(name)))
}
