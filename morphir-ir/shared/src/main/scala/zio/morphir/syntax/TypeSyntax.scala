package zio.morphir.syntax

import zio.morphir.ir.TypeModule.TypeCase.*
import zio.morphir.ir.TypeModule.{Field, Type, TypeCase}
import zio.morphir.ir.{FQName, Name}
import zio.{Chunk, ZEnvironment}

trait TypeSyntax {
  def defineVariable(name: String): Type[Any] = Type(VariableCase(Name.fromString(name)), ZEnvironment.empty)
  def defineVariable(name: Name): Type[Any]   = Type(VariableCase(name), ZEnvironment.empty)
  def defineField(name: Name, fieldType: Type[Any]): Field[Type[Any]] = Field(name, fieldType)
  def defineRecord(fields: Chunk[Field[Type[Any]]]): Type[Any]        = Type(RecordCase(fields), ZEnvironment.empty)
  def defineRecord(fields: Field[Type[Any]]*): Type[Any] =
    Type(RecordCase(Chunk.fromIterable(fields)), ZEnvironment.empty)
  def defineTuple(elementTypes: Chunk[Type[Any]]): Type[Any] = Type(TupleCase(elementTypes), ZEnvironment.empty)
  def defineFunction(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Type[Any] =
    Type(FunctionCase(paramTypes, returnType), ZEnvironment.empty)
  def defineExtensibleRecord(name: Name, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(name, fields), ZEnvironment.empty)
  def defineReference(name: FQName, typeParams: Chunk[Type[Any]]): Type[Any] =
    Type(ReferenceCase(name, typeParams), ZEnvironment.empty)
}

trait TypeModuleSyntax {
  val unit: Type[Any] = Type(UnitCase, ZEnvironment.empty)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Type[Annotations] = Type(UnitCase, annotations)

  /**
   * Creates a type variable with the given `name`.
   */
  final def variable(name: String): Type[Any] = Type(VariableCase(Name.fromString(name)), ZEnvironment.empty)
  final def variable(name: Name): Type[Any]   = Type(VariableCase(name), ZEnvironment.empty)
  final def field(name: Name, fieldType: Type[Any]): Field[Type[Any]] = Field(name, fieldType)
  final def record(fields: Chunk[Field[Type[Any]]]): Type[Any]        = Type(RecordCase(fields), ZEnvironment.empty)
  final def tuple(elementTypes: Chunk[Type[Any]]): Type[Any] = Type(TupleCase(elementTypes), ZEnvironment.empty)
  final def function(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Type[Any] =
    Type(FunctionCase(paramTypes, returnType), ZEnvironment.empty)
  final def extensibleRecord(name: Name, fields: Chunk[Field[Type[Any]]]): Type[Any] =
    Type(ExtensibleRecordCase(name, fields), ZEnvironment.empty)
  final def reference(name: FQName, typeParams: Chunk[Type[Any]]): Type[Any] =
    Type(ReferenceCase(name, typeParams), ZEnvironment.empty)

  @inline final def ref(name: FQName): Type[Any] = reference(name, Chunk.empty)
}
