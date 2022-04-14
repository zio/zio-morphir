package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.Type.{Type, UType}

trait DefinitionConstructors {
  import DefinitionConstructors._
  def functionDef(firstArg: (String, UType), otherArgs: (String, UType)*): FunctionDefInputs[Any, UType] = {
    val args = (firstArg +: Chunk.fromIterable(otherArgs)).map { case (name, tpe) =>
      (Name.fromString(name), tpe, tpe)
    }
    new FunctionDefInputs(args)
  }
}

object DefinitionConstructors {
  final class FunctionDefInputs[TA, VA](val args: Chunk[(Name, VA, Type[TA])]) extends AnyVal {

    def apply(returnType: Type[TA]) = returning(returnType)

    def apply(returnType: Type[TA], body: Value[TA, VA]): Definition[TA, VA] =
      Definition(inputTypes = args, outputType = returnType, body = body)

    def returning(returnType: Type[TA]): FunctionSignature[TA, VA] = new FunctionSignature(() => (args, returnType))
  }

  final class FunctionSignature[TA, VA](val input: () => (Chunk[(Name, VA, Type[TA])], Type[TA])) extends AnyVal {
    def apply(body: => Value[TA, VA]): Definition[TA, VA] = {
      val (args, returnType) = input()
      Definition(inputTypes = args, outputType = returnType, body = body)
    }

    def withBody(body: => Value[TA, VA]): Definition[TA, VA] = {
      val (args, returnType) = input()
      Definition(inputTypes = args, outputType = returnType, body = body)
    }
  }
}
