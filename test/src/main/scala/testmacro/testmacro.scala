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

package testmacro

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

import macrocompat.bundle

object TestCtx {
  def foo: Int = macro TestMacroCtx.fooImpl
  def bar(i: Int): String = macro TestMacroCtx.barImpl
  def baz(is: Int*): Int = macro TestMacroCtx.bazImpl

  def fooE: Int = macro TestMacroCtx.fooEImpl
  def barE(i: Int): String = macro TestMacroCtx.barEImpl
  def bazE(is: Int*): Int = macro TestMacroCtx.bazEImpl

  def quux[T](t: T): T = macro TestMacroCtx.quuxImpl[T]
}

object TestC {
  def foo: Int = macro TestMacroC.fooImpl
  def bar(i: Int): String = macro TestMacroC.barImpl
  def baz(is: Int*): Int = macro TestMacroC.bazImpl

  def fooE: Int = macro TestMacroC.fooEImpl
  def barE(i: Int): String = macro TestMacroC.barEImpl
  def bazE(is: Int*): Int = macro TestMacroC.bazEImpl

  def quux[T](t: T): T = macro TestMacroC.quuxImpl[T]
}

@bundle
class TestMacroCtx(val ctx: whitebox.Context) {
  import ctx.universe._

  def fooImpl: Tree = {
    val nme = TermName(ctx.freshName)
    val i = Ident(nme)

    q""" 23 """
  }

  def barImpl(i: Tree): Tree = q""" "bar" """

  def bazImpl(is: Tree*): Tree = q""" 13 """

  def quuxImpl[T: WeakTypeTag](t: Tree): Tree = {
    val foo = ctx.typecheck(q""" 1+1 """, ctx.TYPEmode).tpe
    t
  }

  def fooEImpl: ctx.Expr[Int] = ctx.Expr[Int](q""" 23 """)

  def barEImpl(i: ctx.Expr[Int]): ctx.Expr[String] = ctx.Expr[String](q""" "bar" """)

  def bazEImpl(is: ctx.Expr[Int]*): ctx.Expr[Int] = ctx.Expr[Int](q""" 13 """)

  def quuxEImpl[T: ctx.WeakTypeTag](t: ctx.Expr[T]): ctx.Expr[T] = {
    val foo = ctx.typecheck(q""" 1+1 """, ctx.TYPEmode).tpe
    ctx.Expr[T](t.tree)
  }
}

@bundle
class TestMacroC(val c: whitebox.Context) {
  import c.universe._

  def fooImpl: Tree = {
    val nme = TermName(c.freshName)
    val i = Ident(nme)

    q""" 23 """
  }

  def barImpl(i: Tree): Tree = q""" "bar" """

  def bazImpl(is: Tree*): Tree = q""" 13 """

  def quuxImpl[T: WeakTypeTag](t: Tree): Tree = {
    val foo = c.typecheck(q""" 1+1 """, c.TYPEmode).tpe
    t
  }

  def fooEImpl: c.Expr[Int] = c.Expr[Int](q""" 23 """)

  def barEImpl(i: c.Expr[Int]): c.Expr[String] = c.Expr[String](q""" "bar" """)

  def bazEImpl(is: c.Expr[Int]*): c.Expr[Int] = c.Expr[Int](q""" 13 """)

  def quuxEImpl[T: c.WeakTypeTag](t: c.Expr[T]): c.Expr[T] = {
    val foo = c.typecheck(q""" 1+1 """, c.TYPEmode).tpe
    c.Expr[T](t.tree)
  }
}
