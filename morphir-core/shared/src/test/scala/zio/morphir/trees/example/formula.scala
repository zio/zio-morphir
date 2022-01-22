package zio.morphir.trees.example

import zio.*
import zio.prelude.*
import zio.morphir.trees.*

object formula {

  type Formula[+Annotations] = Recursive[FormulaCase, Annotations with Context]

  object Formula {
    import FormulaCase.*

    def int(value: Int): Formula[Any] =
      Recursive(IntLiteralCase(value), ZEnvironment.empty.add(Context.empty))

    def let(name: String, value: Formula[Any])(body: Formula[Any]): Formula[Any] =
      Recursive(LetCase(name, value, body), ZEnvironment.empty.add(Context.empty))

    def minus(left: Formula[Any], right: Formula[Any]): Formula[Any] =
      Recursive(MinusCase(left, right), ZEnvironment.empty.add(Context.empty))

    def plus(left: Formula[Any], right: Formula[Any]): Formula[Any] =
      Recursive(PlusCase(left, right), ZEnvironment.empty.add(Context.empty))

    def variable(name: String): Formula[Any] =
      Recursive(VariableCase(name), ZEnvironment.empty.add(Context.empty))

    def assignVariables[Annotations](formula: Formula[Annotations]): Formula[Annotations] = {

      def loop(formula: Formula[Annotations], currentAnnotations: ZEnvironment[Context]): Formula[Annotations] =
        formula.caseValue match {
          case LetCase(variable, value, body) =>
            val evaluatedValue     = evaluate(loop(value, currentAnnotations))
            val currentContext     = currentAnnotations.get[Context]
            val updatedContext     = currentContext.set(variable, evaluatedValue)
            val updatedAnnotations = currentAnnotations.add(updatedContext)
            loop(body, updatedAnnotations)
          case IntLiteralCase(value) =>
            Recursive(IntLiteralCase(value), formula.annotations)
          case PlusCase(left, right) =>
            Recursive(PlusCase(loop(left, currentAnnotations), loop(right, currentAnnotations)), formula.annotations)
          case MinusCase(left, right) =>
            Recursive(MinusCase(loop(left, currentAnnotations), loop(right, currentAnnotations)), formula.annotations)
          case VariableCase(name) =>
            currentAnnotations.get[Context].get(name) match {
              case Some(value) => Recursive(IntLiteralCase(value), formula.annotations)
              case None        => throw new Exception("variable was never assigned")
            }
        }

      loop(formula, formula.annotations)
    }

    def evaluate[Annotations](formula: Formula[Annotations]): Int =
      formula.caseValue match {
        case IntLiteralCase(value)  => value
        case PlusCase(left, right)  => evaluate(left) + evaluate(right)
        case MinusCase(left, right) => evaluate(left) - evaluate(right)
        case VariableCase(name)     => throw new Exception("variable was never assigned")
        case LetCase(variable, value, body) =>
          evaluate(assignVariables(formula))
      }
  }

  implicit class FormulaExtensions[Annotations](val self: Formula[Annotations]) extends AnyVal {
    def assignVariables: Formula[Annotations] =
      Formula.assignVariables(self)

    def evaluate: Int = Formula.evaluate(self)
  }

  sealed trait FormulaCase[+Self] { self =>
    import FormulaCase._
    def map[B](f: Self => B): FormulaCase[B] = self match {
      case LetCase(variable, value, body) => LetCase(variable, f(value), f(body))
      case IntLiteralCase(value)          => IntLiteralCase(value)
      case PlusCase(left, right)          => PlusCase(f(left), f(right))
      case MinusCase(left, right)         => MinusCase(f(left), f(right))
      case VariableCase(name)             => VariableCase(name)
    }
  }

  object FormulaCase {
    case class IntLiteralCase(value: Int)                                extends FormulaCase[Nothing]
    case class PlusCase[+Self](left: Self, right: Self)                  extends FormulaCase[Self]
    case class MinusCase[+Self](left: Self, right: Self)                 extends FormulaCase[Self]
    case class VariableCase(name: String)                                extends FormulaCase[Nothing]
    case class LetCase[+Self](variable: String, value: Self, body: Self) extends FormulaCase[Self]

    implicit val FormulaCaseCovariant: Covariant[FormulaCase] = new Covariant[FormulaCase] {
      override def map[A, B](f: A => B): FormulaCase[A] => FormulaCase[B] = _.map(f)
    }
  }

  final case class Context private (private val data: NonEmptyList[Map[String, Int]]) { self =>
    import zio.prelude.NonEmptyList.*

    def ++(that: Context): Context = Context(self.data ++ that.data)
    def set(name: String, value: Int): Context = self.data match {
      case Cons(head, tail) => Context(Cons(head.updated(name, value), tail))
      case Single(head)     => Context(Single(head.updated(name, value)))
    }

    def get(name: String): Option[Int] = {
      // @tailrec
      def loop(name: String, scope: NonEmptyList[Map[String, Int]]): Option[Int] = scope match {
        case NonEmptyList.Single(map) => map.get(name)
        case NonEmptyList.Cons(map, tail) =>
          map.get(name).orElse(loop(name, tail))
      }
      loop(name, self.data)
    }
    def beginScope(): Context = Context(NonEmptyList.cons(Map.empty[String, Int], self.data))
    def endScope(): Context = self.data match {
      case top @ NonEmptyList.Single(_) => Context(top)
      case NonEmptyList.Cons(_, tail)   => Context(tail)
    }

    // def toAnnotation: Annotation[Context] = Annotation("context", self, _ ++ _)
  }

  object Context {
    val empty: Context = Context(NonEmptyList(Map.empty[String, Int]))
  }
}

object FormulaExample extends zio.ZIOAppDefault {
  import formula.Formula.*
  def run = {
    val theFormula =
      let("x", int(5))(
        let("y", int(10))(
          let("z", int(20))(
            plus(minus(plus(plus(variable("x"), variable("y")), variable("z")), int(3)), int(10))
          )
        )
      )
    for {
      result <- Task(theFormula.evaluate)
      _      <- Console.printLine(s"The result is $result")
    } yield ()

  }
}
