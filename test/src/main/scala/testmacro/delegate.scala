/*
 * Copyright (c) 2016 Miles Sabin
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

package delegate

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

import macrocompat.bundle

class Delegate {
  def delegate: Unit = macro DelegateMacro.delegate
}

@bundle
class DelegateMacro(val c: whitebox.Context) { outer =>
  import c.universe._

  class D(override val c: outer.c.type) extends Delegatee(c)
  def delegate: Tree = (new D(c)).delegate
}

@bundle
class Delegatee(val c: whitebox.Context) {
  import c.universe._

  def delegate: Tree = {
    q"()"
  }
}
