package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.ZEnvironmentSubset
import zio.prelude.AnyType
import zio.morphir.ir.Name
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.value.Value.Lambda
import Pattern.{AsPattern, WildcardPattern}
import zio.morphir.ir.types.UType

final case class Definition[+Caps[_], +TA, +VA](
    inputTypes: Chunk[(Name, ZEnvironmentSubset[Caps, VA], Type[TA])],
    outputType: Type[TA],
    body: Value[Caps, TA, VA]
) { self =>

  import Definition._

  def mapAttributes[TB, VB](f: TA => TB, g: ZEnvironmentSubset[Caps, VA] => ZEnvironmentSubset[Caps, VB]): Definition[Caps, TB, VB] =
    Definition(
      inputTypes.map { case (n, va, t) => (n, g(va), t.mapAttributes(f)) },
      outputType.mapAttributes(f),
      body.mapAttributes(f, g)
    )

  def toValue: Value[Caps, TA, VA] = self.inputTypes.toList match {
    case Nil => self.body
    case (firstArgName, va, _) :: restOfArgs =>
      val definition = self.copy(inputTypes = Chunk.fromIterable(restOfArgs))
      Lambda(
        attributes = va,
        argumentPattern = AsPattern(WildcardPattern(va), firstArgName, va),
        body = definition.toValue
      )
  }

  def toCase: Case[Caps, TA, VA, Value[Caps, TA, VA]] = Case(self.inputTypes, self.outputType, self.body)

  def toSpecification: Specification[TA] = {
    Specification(
      inputTypes.map { case (n, _, t) => (n, t) },
      output = self.outputType
    )
  }

}

object Definition {
  def make[Caps[_], TA, VA](
      inputTypes: (String, ZEnvironmentSubset[Caps, VA], Type[TA])*
  )(outputType: Type[TA])(body: Value[Caps, TA, VA]): Definition[Caps, TA, VA] = {
    val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
    Definition(args, outputType, body)
  }

  final case class Case[+Caps[_], +TA, +VA, +Z](
      inputTypes: Chunk[(Name, ZEnvironmentSubset[Caps, VA], Type[TA])],
      outputType: Type[TA],
      body: Z
  ) { self =>
    def mapAttributes[TB, VB](f: TA => TB, g: ZEnvironmentSubset[Caps, VA] => ZEnvironmentSubset[Caps, VB]): Case[Caps, TB, VB, Z] =
      Case(
        inputTypes.map { case (n, va, t) => (n, g(va), t.mapAttributes(f)) },
        outputType.mapAttributes(f),
        body
      )

    def map[Z2](f: Z => Z2): Case[Caps, TA, VA, Z2] =
      Case(inputTypes, outputType, f(body))
  }

  object Case {
    implicit class CaseExtension[+Caps[_], +TA, +VA](val self: Case[Caps, TA, VA, Value[Caps, TA, VA]]) extends AnyVal {
      def toDefinition: Definition[Caps, TA, VA] = Definition(self.inputTypes, self.outputType, self.body)
    }
  }

  // type Raw = Definition[scala.Unit, Any]
  // object Raw {
  //   def apply(inputTypes: (String, ZEnvironmentSubset[AnyType, Any], UType)*)(outputType: UType)(body: RawValue): Raw = {
  //     val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
  //     Definition(args, outputType, body)
  //   }
  // }

  // type Typed = Definition[scala.Unit, UType]
  // object Typed {
  //   def apply(inputTypes: (String, UType)*)(outputType: UType)(body: TypedValue): Typed = {
  //     val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), t, t) })
  //     Definition(args, outputType, body)
  //   }
  // }
}
