package zio.morphir.lang.metaprog.parsing
import scala.quoted._
import zio.morphir.ir.Module
import zio.morphir.ir.Type.UType

object ModuleParser {
    type ModuleDef = Module.Definition[Unit, UType]
    inline def ModuleDef = Module.Definition
    type ModuleSpec = Module.Specification[Unit]
    inline def ModuleSpec = Module.Specification

    inline def apply[T](inline body:T) = ${parseSpecAndDef('body)}

    def parseSpecAndDef[T:Type](body:Expr[T])(using Quotes): Expr[(ModuleSpec, ModuleDef, T)] = {
        val spec = '{ModuleSpec.empty}
        val defn = '{ModuleDef.empty}
        '{ ($spec, $defn, $body)}
    }


    //def parseModule[] 
}


// val packageSpec = {
//     val c1ModuleSpec = ModuleParser.parseSpec[ModuleContract] {
//         new ModuleContract 
//     }
//     val c2ModuleSpec = ModuleParser.parseSpec[ModuleContract] {
//         new ModuleContract 
//     }
//     (c1ModuleSpec, c2ModuleSpec)
    
// }

// parseModule {
//     object Module {
//         def method = ....
//     }
// }

// trait ModuleContract {
//     def method(value:Int):Int
// }

// trait Module2Contract {

// }

// object ModuleContractV1 extends ModuleContract {
//     inline def method(inline value:Int) = value + 1
// }

// val (module, morphirModule) = 
//     parseModule {
//         new ModuleContract {
//             def method(value:Int) = ModuleContractV1.method(value)
//         }
//     }
// val (module, morphirModule) = 
//     parseModule {
//         new ModuleContract {
//             def method(value:Int) = value + 1
//         }
//     }
