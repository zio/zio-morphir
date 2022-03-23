package zio.morphir.ir

import zio.morphir.syntax.TypeModuleSyntax

object TypeModule extends TypeModuleSyntax {

  final type Constructors[+Attributes] = types.Constructors[Attributes]
  final val Constructors: types.Constructors.type = types.Constructors

  final type UConstructors = types.UConstructors
  final val UConstructors: types.Constructors.type = types.UConstructors

  final type Definition[+Attributes] = types.Definition[Attributes]
  final val Definition: types.Definition.type = types.Definition

  final type UDefinition = types.UDefinition
  final val UDefinition: types.Definition.type = types.UDefinition

  final type Type[+Attributes] = types.Type[Attributes]
  final val Type = types.Type

  /** Represents an un-annotated type. */
  final type UType = types.UType
  final val UType: types.Type.type = types.UType

  final type Specification[+Attributes] = types.Specification[Attributes]
  final val Specification: types.Specification.type = types.Specification

  final type USpecification = types.USpecification
  final val USpecification: types.Specification.type = types.USpecification

}
