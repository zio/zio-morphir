package zio.morphir.ir.types

package object nonrecursive {

  final type UConstructors = Constructors[Unit]
  final val UConstructors: Constructors.type = nonrecursive.Constructors

  final type UDefinition = Definition[Unit]
  final val UDefinition: Definition.type = nonrecursive.Definition

  final type UField = Field[Unit]
  final val UField: Field.type = nonrecursive.Field

  final type USpecification = Specification[Unit]
  final val USpecification: Specification.type = nonrecursive.Specification

  /** Represents an un-annotated/un-attributed type. */
  type UType = Type[Unit]
  val UType: Type.type = Type
}
