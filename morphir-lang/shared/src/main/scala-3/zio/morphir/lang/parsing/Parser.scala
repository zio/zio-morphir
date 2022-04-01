package zio.morphir.lang
import zio.morphir.ir.Value.TypedValue

object Parser {
    inline def valueExpr[T]:TypedValue = ???

}

object MorphirModule {
    val pi = 3.14
    def add(a: Int, b: Int): Int =  a + b
}