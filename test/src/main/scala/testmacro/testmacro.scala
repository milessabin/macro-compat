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

object Test {
  def foo: Int = macro TestMacro.fooImpl
  def bar(i: Int): String = macro TestMacro.barImpl
  def baz(is: Int*): Int = macro TestMacro.bazImpl

  def fooE: Int = macro TestMacro.fooEImpl
  def barE(i: Int): String = macro TestMacro.barEImpl
  def bazE(is: Int*): Int = macro TestMacro.bazEImpl

  def quux[T](t: T): T = macro TestMacro.quuxImpl[T]

  def comp[T](t: T): String = macro TestMacro.compImpl[T]
}

@bundle
trait TestUtil {
  val c: whitebox.Context
  import c.universe._

  def util(t: Type): Tree = {
    q""" () """
  }
}

@bundle
class TestMacro(val c: whitebox.Context) extends TestUtil {
  import c.universe._

  def fooImpl: Tree = {
    val nme = TermName(c.freshName)
    val i = Ident(nme)

    val nme2 = TermName(c.freshName("pfx"))
    val nme3 = c.freshName(TermName("pfx"))

    val nme4 = termNames.CONSTRUCTOR
    val nme5 = typeNames.WILDCARD

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

  def compImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val typ = weakTypeOf[T]
    q""" ${typ.companion.toString } """
  }

  def useUtil: Tree =
    util(typeOf[Int])
}
