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

package overrides

import scala.language.experimental.macros

import scala.reflect.macros.{ blackbox, whitebox }

import macrocompat.bundle

object Override0 {
  def bar(i: Int): String = macro Sub0.barImpl
}

@bundle
trait Super0 {
  val c: blackbox.Context

  import c.universe._

  def fooImpl(i: Tree): Tree = q""" "foo" """
}

@bundle
class Sub0(val c: whitebox.Context) extends Super0 {
  import c.universe._

  def barImpl(i: Tree): Tree = q""" "bar" """
}

object Override1 {
  def foo(i: Int): String = macro Super1.fooImpl
  def bar(i: Int): String = macro Sub1.barImpl
}

@bundle
class Super1(val c: blackbox.Context) {
  import c.universe._

  def fooImpl(i: Tree): Tree = q""" "foo" """
}

@bundle
class Sub1(override val c: whitebox.Context) extends Super1(c) {
  import c.universe._

  def barImpl(i: Tree): Tree = q""" "bar" """
}
