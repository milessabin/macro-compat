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

  object GlobalConversions {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

    implicit def globalType(tpe: Type): global.Type = tpe.asInstanceOf[global.Type]
    implicit def globalSymbol(sym: Symbol): global.Symbol = sym.asInstanceOf[global.Symbol]
    implicit def globalTypeSymbol(sym: TypeSymbol): global.TypeSymbol = sym.asInstanceOf[global.TypeSymbol]
    implicit def globalTree(tree: Tree): global.Tree = tree.asInstanceOf[global.Tree]

    implicit def macroType(tpe: global.Type): Type = tpe.asInstanceOf[Type]
    implicit def macroSymbol(sym: global.Symbol): Symbol = sym.asInstanceOf[Symbol]
    implicit def macroTypeSymbol(sym: global.TypeSymbol): TypeSymbol = sym.asInstanceOf[TypeSymbol]
    implicit def macroTree(tree: global.Tree): Tree = tree.asInstanceOf[Tree]
  }

  import GlobalConversions._

  def symbolOf[T: WeakTypeTag]: TypeSymbol =
    weakTypeOf[T].typeSymbolDirect.asType

  object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }

  object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

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

  def untypecheck(tree: Tree): Tree = c.resetLocalAttrs(tree)

  implicit class TypeOps(tpe: Type) {
    def typeParams = tpe.typeSymbol.asType.typeParams

    def companion: Type = {
      val sym = tpe.typeSymbolDirect
      if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol.tpe
      else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol.tpe
      else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol.info
      else NoType
    }

    def decl(nme: Name): Symbol = tpe.declaration(nme)

    def decls = tpe.declarations
  }

  implicit class MethodSymbolOps(sym: MethodSymbol) {
    def paramLists = sym.paramss
  }

  implicit class SymbolOps(sym: Symbol) {
    def companion: Symbol = {
      if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol
      else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol
      else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol
      else NoSymbol
    }
  }

  def appliedType(tc: Type, ts: List[Type]): Type = c.universe.appliedType(tc, ts)
  def appliedType(tc: Type, ts: Type*): Type = c.universe.appliedType(tc, ts.toList)

  def showCode(t: Tree): String = show(t)

  object internal {
    def constantType(c: Constant): ConstantType = ConstantType(c)

    def enclosingOwner: Symbol = {
      val internalContext = c.asInstanceOf[scala.reflect.macros.runtime.Context]
      val internalOwner = internalContext.callsiteTyper.context.owner
      internalOwner.asInstanceOf[Symbol]
    }

    object gen {
      def mkAttributedRef(sym: Symbol): Tree =
        global.gen.mkAttributedRef(sym)

      def mkAttributedRef(pre: Type, sym: Symbol): Tree =
        global.gen.mkAttributedRef(pre, sym)
    }
  }
}
