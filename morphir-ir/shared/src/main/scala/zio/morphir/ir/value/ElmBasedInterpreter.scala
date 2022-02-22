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
import zio.morphir.ir.ValueModule.ValueCase.*
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.FQName
import zio.morphir.ir.NativeFunction.*
import zio.Chunk
import scala.collection.immutable.ListMap

object ElmBasedInterpreter {

    type Variables = Map[Name, RawValue]
    type Error
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
        ir : IR, variables : Variables, 
        arguments : Chunk[RawValue], 
        value : RawValue) : Either[Error, RawValue] = ???

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
