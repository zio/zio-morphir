package zio.morphir.ir.testing

import java.math.BigInteger
import zio.test.*
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.Chunk
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.testing.MorphirBaseSpec
import zio.morphir.Dsl
import zio.morphir.ir.ValueModule.Value.*

import java.math.BigInteger

object CaseExample {
  // /x = if (foo) y else 0
  // y = if (!foo) x else 0
  val letIntroduceMultipleExample: Value[Any] = letRecursion(
    Map(
      Name.fromString("x") -> literal(20), // lit(20)
      Name.fromString("y") -> literal(22)
    ),
    nativeApply(
      NativeFunction.Addition,
      Chunk(variable("x"), variable("y"))
    )
  )

  val letIntroduceOutOfOrderExample: Value[Any] = letRecursion(
    Map(
      Name.fromString("x") ->
        nativeApply(
          NativeFunction.Addition,
          Chunk(
            variable("y"),
            Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("22"))))
          )
        ),
      Name.fromString("y") -> Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("22"))))
    ),
    variable("x")
  )

  val applyFieldFunction: Value[Any] =
    Dsl.apply(fieldFunction(Name.fromString("fieldA")), recordCaseExample)

  val additionExample: Value[Any] =
    letDefinition(
      Name("x"),
      literal(1),
      letDefinition(
        Name("y"),
        literal(2),
        nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
      )
    )

  val subtractionExample: Value[Any] =
    letDefinition(
      Name("x"),
      literal(1),
      letDefinition(
        Name("y"),
        literal(2),
        nativeApply(
          NativeFunction.Subtraction,
          Chunk(variable(Name("x")), variable(Name("y")))
        )
      )
    )

  val tupleCaseExample: Value[Any] =
    tuple(
      literal(1),
      literal(2)
    )

  val listCaseExample: Value[Any] =
    list(
      Chunk(
        literal("hello"),
        literal("world")
      )
    )
  val ifThenElseCaseExample: Value[Any] =
    ifThenElse(
      condition = literal(false),
      thenBranch = literal("yes"),
      elseBranch = literal("no")
    )

  lazy val recordCaseExample = {
    val fieldA = Name.fromString("fieldA")
    val fieldB = Name.fromString("fieldB")

    val value1 = Dsl.string("hello")
    val value2 = Dsl.wholeNumber(new java.math.BigInteger("2"))

    val element1 = fieldA -> value1
    val element2 = fieldB -> value2
    Dsl.record(element1, element2)
  }

  val recordCaseUpdateExample = {
    updateRecord(
      recordCaseExample,
      Chunk(
        Name("fieldB") -> Dsl.wholeNumber(new java.math.BigInteger("3"))
      )
    )
  }

  val patternMatchWildcardCaseExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      wildcardPattern -> Dsl.wholeNumber(
        new java.math.BigInteger("100")
      )
    )

  val patternMatchAsCaseExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      asPattern(wildcardPattern, Name.fromString("x")) -> Dsl.variable(Name.fromString("x"))
    )

  val patternMatchEmptyListCaseExample =
    Dsl.patternMatch(
      list(Chunk()),
      emptyListPattern -> literal("empty list")
    )

  val patternHeadTailCaseExample =
    Dsl.patternMatch(
      listCaseExample,
      headTailPattern(
        literalPattern("hello"),
        asPattern(wildcardPattern, Name("tail"))
      ) -> variable("tail")
    )

  val patternTupleOneCaseExample =
    Dsl.patternMatch(
      tuple(literal("singleton tuple")),
      tuplePattern(asPattern(wildcardPattern, Name("x"))) -> variable(Name("x"))
    )

  val patternTupleOneCaseCounterExample =
    Dsl.patternMatch(
      literal("singleton tuple"),
      tuplePattern(wildcardPattern) -> literal("wrong"),
      wildcardPattern               -> literal("right")
    )

  val patternTupleCaseExample =
    Dsl.patternMatch(
      tupleCaseExample,
      tuplePattern(
        wildcardPattern,
        wildcardPattern
      ) -> Dsl.wholeNumber(new java.math.BigInteger("107"))
    )

  val patternUnitCaseExample =
    Dsl.patternMatch(
      unit,
      emptyListPattern -> literal("wrong"),
      unitPattern      -> literal("right")
    )

  val letDestructExample =
    destructure(
      tuplePattern(asPattern(wildcardPattern, Name("x")), asPattern(wildcardPattern, Name("y"))),
      tuple(Value.literal("red"), Value.literal("blue")),
      variable("x")
    )
  val letRecExample =
    letRecursion(
      Map(
        Name.fromString("x") -> ifThenElse(
          condition = literal(false),
          thenBranch = variable("y"),
          elseBranch = literal(3)
        ),
        Name.fromString("y") ->
          ifThenElse(
            condition = literal(false),
            thenBranch = literal(2),
            elseBranch = variable("x")
          )
      ),
      nativeApply(
        NativeFunction.Addition,
        Chunk(
          variable("x"),
          variable("y")
        )
      )
    )

  // (valueDefinitions: Map[Name, Self], inValue: Self)

  // example : letrec (x, y) = if (cond) then (0, x) else (y, 0)

  /**
   * letRec x -> 3 y -> 4 x + y
   */

  val patternMatchAsCaseComplexExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("7")),
      Value.asPattern(
        literalPattern(8),
        Name.fromString("x")
      ) ->
        nativeApply(
          NativeFunction.Subtraction,
          Chunk(
            variable(Name.fromString("x")),
            variable(Name.fromString("x"))
          )
        ),
      asPattern(
        literalPattern(7),
        Name.fromString("x")
      ) ->
        nativeApply(
          NativeFunction.Addition,
          Chunk(
            variable(Name.fromString("x")),
            variable(Name.fromString("x"))
          )
        )
    )

  // { case _ => 42}()

  val applyWithWildCard =
    Dsl.apply(Value.lambda(Dsl.wildcard, Dsl.wholeNumber(new java.math.BigInteger("42"))), Dsl.unit)

  val lambdaExample = letDefinition(
    Name("foo"),
    Value.lambda(
      Value.asPattern(Dsl.wildcard, Name("x")),
      nativeApply(
        NativeFunction.Addition,
        Chunk(
          variable("x"),
          variable("x")
        )
      )
    ),
    Dsl.apply(variable("foo"), literal(33))
  )

  val personName = zio.morphir.ir.FQName(zio.morphir.ir.Path(Name("")), zio.morphir.ir.Path(Name("")), Name("Person"))

  val constructorExample =
    apply(
      constructor(personName),
      Chunk(literal("Adam"), literal(42))
    )
}
