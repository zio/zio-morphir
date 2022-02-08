package zio.morphir.ir.value

import zio.morphir.ir.MorphirIR
import zio.morphir.ir.recursive.MorphirIRCase
import zio.morphir.ir.recursive.ValueCase
import zio.morphir.ir.Literal
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.FQName
import zio.morphir.ir.MorphirIR.Value
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.NativeFunction.Addition
import zio.prelude._

object Interperter {
  def eval[Annotations](ir: MorphirIR[Annotations]): Either[InterpretationError, Any] = {

    def loop(
        ir: MorphirIR[Annotations],
        variables: Map[Name, Any],
        references: Map[FQName, Value[Annotations]]
    ): Either[InterpretationError, Any] = {
      ir.caseValue match {

        case ValueCase.ApplyCase(function, args) =>
          ???

        case ValueCase.LetDefinitionCase(name, value, body) =>
          loop(body, variables + (name -> loop(value, variables, references)), references)

        case ValueCase.LiteralCase(value) =>
          Right(evalLiteralValue(value))

        case ValueCase.NativeApplyCase(function, args) =>
          args.forEach(loop(_, variables, references)).flatMap(evalNativeFunction(function, _))

        case ValueCase.UnitCase =>
          Right(())

        case ValueCase.VariableCase(name) =>
          variables.get(name) match {
            case Some(value) => Right(value)
            case None        => Left(new InterpretationError.VariableNotFound(name, s"Variable $name not found"))
          }

        case ValueCase.ReferenceCase(fqName) =>
          references.get(fqName) match {
            case Some(value) => Right(value)
            case None        => Left(new InterpretationError.ReferenceNotFound(fqName, s"Reference $fqName not found"))
          }

        case _ => ???
      }
    }

    loop(ir, Map.empty, Map.empty)
  }

  // val x = 1
  // val y = 2
  // x + y

  private def evalLiteralValue(literalValue: LiteralValue): Any =
    literalValue match {
      case Literal.Bool(value)        => value
      case Literal.Char(value)        => value
      case Literal.String(value)      => value
      case Literal.WholeNumber(value) => value
      case Literal.Float(value)       => value
    }

  def evalNativeFunction(function: NativeFunction, args: Chunk[Any]): Either[InterpretationError, Any] =
    function match {
      case Addition => evalAddition(args)
    }

  def evalAddition(args: Chunk[Any]): Either[InterpretationError, Any] = {
    if (args.length == 0)
      Left(InterpretationError.InvalidArguments(args, s"Addition expected at least two argument but got none."))
    else if (args(0).isInstanceOf[Either[_, _]])
      Right(args.asInstanceOf[Chunk[Either[Nothing, java.math.BigInteger]]].collect { case Right(value) => value }.reduce(_ add _))
    else
      Right(args.asInstanceOf[Chunk[Either[Nothing, java.math.BigDecimal]]].collect { case Right(value) => value }.reduce(_ add _))
  }
}

sealed trait InterpretationError
object InterpretationError {
  final case class Message(message: String)                            extends InterpretationError
  final case class VariableNotFound(name: Name, message: String)       extends InterpretationError
  final case class ReferenceNotFound(name: FQName, message: String)    extends InterpretationError
  final case class InvalidArguments(args: Chunk[Any], message: String) extends InterpretationError
}

// type alias IR =
//     { valueSpecifications : Dict FQName (Value.Specification ())
//     , valueDefinitions : Dict FQName (Value.Definition () (Type ()))
//     , typeSpecifications : Dict FQName (Type.Specification ())
//     , typeConstructors : Dict FQName ( FQName, List Name, List ( Name, Type () ) )
//     }

object Example extends scala.App {

  def add(x: Int, y: Int): Int = x + y

  println("A")

  // val x = 1
  // val y = 2
  // x + y
  val myIRCase: MorphirIRCase[MorphirIR[Any]] =
    ValueCase.LetDefinitionCase(
      Name("x"),
      MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("1")))),
      MorphirIR(
        ValueCase.LetDefinitionCase(
          Name("y"),
          MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("2")))),
          MorphirIR(
            ValueCase.NativeApplyCase(
              NativeFunction.Addition,
              Chunk(MorphirIR(ValueCase.VariableCase(Name("x"))), MorphirIR(ValueCase.VariableCase(Name("y"))))
            )
          )
        )
      )
    )

  println("B")

  val myIR =
    MorphirIR(myIRCase)

  println("C")

  val result = Interperter.eval(myIR)

  println(result)
}
