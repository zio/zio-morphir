package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.PackageModule.PackageName
import zio.morphir.ir._

object Common {
  val packageName: PackageName = PackageName.fromString("Morphir.SDK")

  def toFQName(moduleName: ModuleName, localName: String): FQName =
    FQName(packageName, moduleName.toModulePath, Name.fromString(localName))

  def tFun(argTypes: UType*)(returnType: UType): UType      = tFun(argTypes.toList, returnType)
  def tFun(argTypes: List[UType], returnType: UType): UType = UType.curriedFunction(argTypes, returnType)

  def tVar(varName: String): UType = UType.variable(varName)

  def vSpec(name: String, inputs: (String, UType)*) = new VSpec(() => (name, Chunk.fromIterable(inputs)))

  final class VSpec(private val data: () => (String, Chunk[(String, UType)])) extends AnyVal {
    def apply(outputType: UType): (Name, ValueModule.Specification[Any]) = {
      val (name, inputs) = data()
      (
        Name.fromString(name),
        ValueModule.Specification(inputs.map { case (name, tpe) => (Name.fromString(name), tpe) }, outputType)
      )
    }

    @inline def returning(outputType: UType): (Name, ValueModule.Specification[Any]) = apply(outputType)
  }

}
