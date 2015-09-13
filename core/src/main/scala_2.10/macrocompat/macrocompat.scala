/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package macrocompat

import scala.language.experimental.macros

import scala.reflect.macros.Context

trait MacroCompat {
  val c: Context
  import c.universe._

  def TypeName(s: String) = newTypeName(s)
  def TermName(s: String) = newTermName(s)

  val termNames = nme
  val typeNames = tpnme

  def freshName = c.fresh
  def freshName(name: String) = c.fresh(name)
  def freshName[NameType <: Name](name: NameType) = c.fresh(name)

  implicit def mkContextOps(c0: c.type): this.type = this

  sealed trait TypecheckMode
  case object TERMmode extends TypecheckMode
  case object TYPEmode extends TypecheckMode

  def typecheck(
    tree: Tree,
    mode: TypecheckMode = TERMmode,
    pt: Type = WildcardType,
    silent: Boolean = false,
    withImplicitViewsDisabled: Boolean = false,
    withMacrosDisabled: Boolean = false
  ): Tree =
    // Spurious non exhaustive match warning ... see,
    //   https://issues.scala-lang.org/browse/SI-8068
    (mode: @unchecked) match {
      case TERMmode =>
        c.typeCheck(tree, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
      case TYPEmode =>
        val term = q"null.asInstanceOf[$tree[Any]]"
        c.typeCheck(term, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
    }

  implicit def mkTypeOps(tpe: Type): TypeOps = new TypeOps(tpe)
  class TypeOps(tpe: Type) {
    def typeParams = tpe.typeSymbol.asType.typeParams
    def companion = {
      val typSym = tpe.typeSymbol
      if (typSym.isModuleClass) tpe.termSymbol.companionSymbol.asType.toType
      else if (typSym.isClass) typSym.companionSymbol.asModule.moduleClass.asType.toType
      else NoType
    }
    def decls = tpe.declarations
  }

  implicit def mkMethodSymbolOps(sym: MethodSymbol): MethodSymbolOps = new MethodSymbolOps(sym)
  class MethodSymbolOps(sym: MethodSymbol) {
    def paramLists = sym.paramss
  }

  def appliedType(tc: Type, ts: List[Type]): Type = c.universe.appliedType(tc, ts)
  def appliedType(tc: Type, ts: Type*): Type = c.universe.appliedType(tc, ts.toList)

  def showCode(t: Tree): String = show(t)
}

