package zio.morphir.ir.value

import zio.morphir.ir.MorphirIR
import zio.morphir.ir.recursive.MorphirIRCase
import zio.morphir.ir.recursive.ValueCase
import zio.morphir.ir.Literal
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.Chunk
import zio.morphir.ir.FQName
import zio.morphir.ir.MorphirIR.Value
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.NativeFunction.Addition

object Interperter {
  def eval[Annotations](ir: MorphirIR[Annotations]): Either[InterpretationError, Any] = {

    def loop(
        ir: MorphirIR[Annotations],
        variables: Map[Name, Any],
        references: Map[FQName, Value[Annotations]]
    ): Any = {
      ir.caseValue match {

        case ValueCase.ApplyCase(function, args) =>
          ???

        case ValueCase.LetDefinitionCase(name, value, body) =>
          loop(body, variables + (name -> loop(value, variables, references)), references)

        case ValueCase.LiteralCase(value) =>
          evalLiteralValue(value)

        case ValueCase.NativeApplyCase(function, args) =>
          evalNativeFunction(function, args.map(loop(_, variables, references)))

        case ValueCase.UnitCase =>
          ()

        case ValueCase.VariableCase(name) =>
          variables.get(name) match {
            case Some(value) => value
            case None        => throw new InterpretationError.VariableNotFound(name, s"Variable $name not found")
          }

        case ValueCase.ReferenceCase(fqName) =>
          references.get(fqName) match {
            case Some(value) => value
            case None        => throw new InterpretationError.ReferenceNotFound(fqName, s"Reference $fqName not found")
          }

        case _ => ???
      }
    }

    try {
      Right(loop(ir, Map.empty, Map.empty))
    } catch {
      case interpretationError: InterpretationError => Left(interpretationError)
    }
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

  def evalNativeFunction(function: NativeFunction, args: Chunk[Any]): Any =
    function match {
      case Addition => evalAddition(args)
    }

  def evalAddition(args: Chunk[Any]): Any =
    if (args.length == 0)
      throw new InterpretationError.InvalidArguments(args, s"Addition expected at least two argument but got none.")
    else if (args(0).isInstanceOf[java.math.BigInteger])
      args.asInstanceOf[Chunk[java.math.BigInteger]].reduce(_ add _)
    else
      args.asInstanceOf[Chunk[java.math.BigDecimal]].reduce(_ add _)
}

sealed trait InterpretationError extends Throwable
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
