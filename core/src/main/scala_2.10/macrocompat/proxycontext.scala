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

import scala.reflect.macros.Context

abstract class ProxyContext extends Context {
  val c: Context
  lazy val universe: c.universe.type = c.universe
  import universe._

  def Expr[T: WeakTypeTag](tree: Tree) = c.Expr[T](tree)
  override val Expr = c.Expr
  def TypeTag[T](tpe: Type) = c.TypeTag(tpe)
  def WeakTypeTag[T](tpe: Type) = c.WeakTypeTag(tpe)

  val mirror = c.mirror
  type PrefixType = c.PrefixType
  val prefix = c.prefix

  val enclosingClass = c.enclosingClass
  val enclosingImplicits = c.enclosingImplicits
  val enclosingMacros = c.enclosingMacros
  val enclosingMethod = c.enclosingMethod
  val enclosingPosition = c.enclosingPosition
  val enclosingRun = c.enclosingRun
  val enclosingUnit = c.enclosingUnit
  val macroApplication = c.macroApplication

  def eval[T](expr: Expr[T]) = c.eval(expr)

  def literalNull = c.literalNull
  def literalUnit = c.literalUnit
  def literalTrue = c.literalTrue
  def literalFalse = c.literalFalse
  def literal(x: Boolean) = c.literal(x)
  def literal(x: Byte) = c.literal(x)
  def literal(x: Short) = c.literal(x)
  def literal(x: Int) = c.literal(x)
  def literal(x: Long) = c.literal(x)
  def literal(x: Float) = c.literal(x)
  def literal(x: Double) = c.literal(x)
  def literal(x: String) = c.literal(x)
  def literal(x: Char) = c.literal(x)

  def abort(pos: Position, msg: String) = c.abort(pos, msg)
  def echo(pos: Position, msg: String) = c.echo(pos, msg)
  def error(pos: Position, msg: String) = c.error(pos, msg)
  def hasErrors = c.hasErrors
  def hasWarnings = c.hasWarnings
  def info(pos: Position, msg: String, force: Boolean) = c.info(pos, msg, force)
  def warning(pos: Position, msg: String) = c.warning(pos, msg)

  def classPath = c.classPath
  def compilerSettings = c.compilerSettings
  def settings = c.settings

  def fresh[NameType <: Name](name: NameType) = c.fresh(name)
  def fresh(name: String) = c.fresh(name)
  def fresh() = c.fresh()

  def parse(code: String) = c.parse(code)

  def reifyEnclosingRuntimeClass = c.reifyEnclosingRuntimeClass
  def reifyRuntimeClass(tpe: Type, concrete: Boolean = false) = c.reifyRuntimeClass(tpe, concrete)
  def reifyTree(universe: Tree, mirror: Tree, tree: Tree) = c.reifyTree(universe, mirror, tree)
  def reifyType(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean = false) = c.reifyType(universe, mirror, tpe, concrete)
  def unreifyTree(tree: Tree) = c.unreifyTree(tree)

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition) = c.inferImplicitValue(pt, silent, withMacrosDisabled, pos)
  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition) = c.inferImplicitView(tree, from, to, silent, withMacrosDisabled, pos)
  def openImplicits = c.openImplicits
  def openMacros = c.openMacros
  def resetAllAttrs(tree: Tree) = c.resetAllAttrs(tree)
  def resetLocalAttrs(tree: Tree) = c.resetLocalAttrs(tree)
  def typeCheck(tree: Tree, pt: Type = WildcardType, silent: Boolean = true, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false) = c.typeCheck(tree, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
}
