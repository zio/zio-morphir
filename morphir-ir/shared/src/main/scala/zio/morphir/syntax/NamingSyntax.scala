package zio.morphir.syntax

import zio.morphir.ir.Name
import zio.morphir.ir.PackageModule.PackageName
import zio.morphir.ir.FQName

trait NamingSyntax {
  def fqn(packageName: String, module: String, localName: String): FQName = FQName.fqn(packageName, module, localName)
  def name(name: String): Name                                            = Name.fromString(name)
  def pkg(name: String): PackageName                                      = PackageName.fromString(name)
}

object NamingSyntax extends NamingSyntax
