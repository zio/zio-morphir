package zio.morphir.ir.value
import zio.prelude._
import zio.morphir.ir.Name
import zio.morphir.ir.ValueModule.Value
import zio.morphir.ir.ValueModule.RawValue
import zio.morphir.ir.ValueModule.Value.*
import zio.morphir.ir.TypeModule
import zio.morphir.IRModule.IR
import zio.morphir.Dsl
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.ValueModule.ValueCase
import zio.morphir.ir.ValueModule.ValueCase.*
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.FQName
import zio.morphir.ir.NativeFunction.*
import zio.Chunk
import scala.collection.immutable.ListMap

object ElmBasedInterpreter {

    type Variables = Map[Name, RawValue]
    type Error = Any
    case class PatternMismatch(pattern : RawValue, value : RawValue)
    type MatchResult = Either[PatternMismatch, Variables]

    def evaluateFunctionValue(
        nativeFunctions : Map[FQName, NativeFunction], 
        ir : IR, 
        name : FQName, 
        variables : Chunk[RawValue]) : Either[Error, RawValue] = ???

    def evaluate(
        nativeFunctions : Map[FQName, NativeFunction], 
        ir : IR, 
        value : RawValue) : Either[Error, RawValue] = ???

    def evaluateValue(
        nativeFunctions : Map[FQName, NativeFunction], 
        ir : IR, 
        variables : Variables, 
        arguments : Chunk[RawValue], 
        value : RawValue) : Either[Error, RawValue] = {
            def recurse(inner_value : RawValue) = evaluateValue(nativeFunctions, ir, variables, Chunk.empty, inner_value)
            value.caseValue match {
                case _ : LiteralCase => Right(value)
                case _ : ConstructorCase => Right(value) //TODO : Revisit?
                case TupleCase(elems : Chunk[RawValue]) => 
                    elems
                        .forEach(recurse)
                        .map(evaluatedElems => Value(TupleCase(evaluatedElems)))
                case ListCase(elems : Chunk[RawValue]) => 
                    elems
                        .forEach(recurse)
                        .map(evaluatedElems => Value(TupleCase(evaluatedElems)))
                case RecordCase(fields : Chunk[FieldCase[RawValue]]) =>
                    fields
                        .forEach(field => recurse(field.target).map((value : RawValue) => (field.name, value)))
                        .map(evaluatedFields => Value(RecordCase(evaluatedFields)))
                case VariableCase(varName) =>
                    variables.get(varName) match {
                        case Some(value) => Right(value)
                        case None => Left(s"Variable $varName not found.")
                    }
                case ReferenceCase(fqName) => 
                    nativeFunctions.get(fqName) match {
                        // case Some(nativeFunction) =>
                        //     nativeFunction(recurse, arguments)
                        //         .mapLeft(_ => s"Error while evaluating function : $fqName")
                        case None =>
                            arguments
                                .forEach(recurse)
                                .flatMap((args : Chunk[RawValue]) => evaluateFunctionValue(nativeFunctions, ir, fqName, args))
                    }
                case FieldCase(subject, fieldName) =>
                    recurse(subject).flatMap(_.caseValue match {
                        case RecordCase(fields : Chunk[(Name, RawValue)]) => 
                            fields.find(_._1 == fieldName) match {
                                case Some(value) => Right(value._2)
                                case None => Left(s"Field $fieldName referenced but not found")
                            }
                        case _ => Left("Expected record")
                    })
                case FieldFunctionCase(fieldName) => 
                    if (arguments.length == 1) recurse(arguments.head).flatMap(_.caseValue match {
                        case RecordCase(fields : Chunk[(Name, RawValue)]) => 
                            fields.find(_._1 == fieldName) match {
                                case Some(value) => Right(value._2)
                                case None => Left(s"Field $fieldName referenced but not found")
                            }
                        case _ => Left("Expected record")
                    }) else Left("Field Function takes exactly one argument")
                case ApplyCase(function, arguments) =>
                    evaluateValue(nativeFunctions, ir, variables, arguments, function)
                case LambdaCase(argumentPattern, body) =>
                    if (arguments.length == 1) then {
                        matchPattern(argumentPattern, arguments.head)
                            .flatMap(matchedArguments => 
                                evaluateValue(nativeFunctions, ir, variables ++ matchedArguments, Chunk.empty, body))
                    } else Left("Lambda expects single argument")
                case LetDefinitionCase(letName, letValue, letBody) =>
                    for {
                        evaluatedValue <- recurse(letValue)
                        result <- evaluateValue(nativeFunctions, ir, variables + (letName -> evaluatedValue), Chunk.empty, letBody)
                    } yield result
                case LetRecursionCase(definitions, letBody) =>
                    evaluateValue(nativeFunctions, ir, variables ++ definitions, Chunk.empty, letBody)
                case DestructureCase(pattern, valueToDestruct, inValue) =>
                    for {
                        evaluatedValue <- recurse(valueToDestruct)
                        matchedArgs <- matchPattern(pattern, evaluatedValue)
                        result <- evaluateValue(nativeFunctions, ir, variables ++ matchedArgs, Chunk.empty, inValue)
                    } yield result
                case IfThenElseCase(cond, thenBranch, elseBranch) =>
                    for {
                        evaluatedCondition <- recurse(cond)
                        result <- {
                            evaluatedCondition match {
                                // case LiteralCase(true) => recurse(thenBranch)
                                // case LiteralCase(false) => recurse(elseBranch)
                                case _ => Left("Condition of If/Then/Else should evaluate to a boolean literal")
                            }
                        } 
                     }yield result
                case PatternMatchCase(subjectValue, cases) =>
                    for {
                        evaluatedSubject <- recurse(subjectValue)
                        matchedCase <- cases.forEach(caseClause => 
                            matchPattern(caseClause._1, evaluatedSubject)
                                .map(foundArgs => (foundArgs, caseClause._2))
                                .swap
                        ).swap
                        result <- evaluateValue(nativeFunctions, ir, variables ++ matchedCase._1, Chunk.empty, matchedCase._2)
                    } yield result
                //Note: Elm interpreter had safety checks here that I'm skipping
                case UpdateRecordCase(valueToUpdate, fieldsToUpdate) => 
                    for {
                        evaluatedRecord <- recurse(valueToUpdate)
                        evaluatedFields <- fieldsToUpdate.forEach({case(name, value) => recurse(value).map((name -> _))})
                        result <- {
                            evaluatedRecord.caseValue match {
                                case RecordCase(oldFields: Chunk[(Name, RawValue)]) => Right(RecordCase[RawValue](oldFields ++ evaluatedFields))
                                case _ => Left("Tried to update something which is not a record")
                            }
                        }
                    } yield Value(result)
                case _ : UnitCase => Right(Value(UnitCase))
            }
        }

    def matchPattern(
        pattern : RawValue,
        value : RawValue
    ) : Either[PatternMismatch, Variables] = {
        import PatternCase.*
        //val pattern : PatternCase[Any] = pattern_1.asInstanceOf[PatternCase]
        val noMatch  = Left(PatternMismatch(pattern, value))

        // def forEach[F[+_], G[+_], A](collection: F[A])(f: A => G[B]): G[F[B]] = ???

        // def forEach[E, A, B](collection: Chunk[A])(f: A => Either[E, B]): Either[E, Chunk[B]] = ???

        


        val as = Chunk(1, 2, 3)
        def validateEven(n: Int): Either[String, Int] =
            if (n % 2 == 0) Right(n)
            else Left(s"$n is not even")

        val result = as.forEach(validateEven)

        def helper(values: Chunk[RawValue], patterns: Chunk[RawValue]): MatchResult = {
            if (values.length != patterns.length) then noMatch else {
                val zipped = values.zip(patterns)

                zipped
                    .forEach((value, pattern) => matchPattern(pattern, value))
                    .map(_.foldLeft(Map.empty[Name, RawValue])(_ ++ _))
            }
        }


        pattern.caseValue match {
            case WildcardPatternCase => Right(Map.empty)
            case AsPatternCase(pattern, name) => 
                matchPattern(pattern, value).map(_ + (name -> value))
            case TuplePatternCase(patterns) => 
                value.caseValue match {
                    case TupleCase(values) => helper(values.asInstanceOf[Chunk[RawValue]], patterns)
                    case _ => noMatch
                }
            case ConstructorPatternCase(patternName, patternArgs) =>
                value.caseValue match {
                    case ApplyCase(constructor, args: Chunk[RawValue]) => 
                        constructor.caseValue match {
                            case ConstructorCase(fqName) => 
                                if (patternName == fqName) helper(args, patternArgs)
                                else noMatch
                            case _ => noMatch
                        }
                    case _ => noMatch
                }
            case EmptyListPatternCase =>
                value.caseValue match {
                    case ListCase(Chunk.empty) => Right(Map.empty)
                    case _ => noMatch
                }
            case HeadTailPatternCase(headPattern, tailPattern) =>
                value.caseValue match {
                    case ListCase(values : Chunk[RawValue]) =>
                        if (!values.isEmpty) 
                            for {
                                headVars <- matchPattern(headPattern, values.head)
                                tailVars <- matchPattern(tailPattern, Value(ListCase(values.tail)))
                            } yield headVars ++ tailVars
                        else noMatch
                    case _ => noMatch
                }
            case LiteralPatternCase(patternLiteral) => 
                value.caseValue match {
                    case LiteralCase(valueLiteral) => 
                        if (patternLiteral == valueLiteral) then Right(Map.empty) else noMatch
                    case _ => noMatch
                }
            case UnitPatternCase =>
                value.caseValue match {
                    case _ : UnitCase => Right(Map.empty)
                    case _ => noMatch
                }
            case _ => noMatch
            
        }
    }

    
}
