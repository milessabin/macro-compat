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

package testannotations

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import macrocompat.bundle

import scala.annotation.{compileTimeOnly, StaticAnnotation}

/**
 * Test is that this should compile
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class Testbundle extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TestbundleMacros.impl
}

@bundle
class TestbundleMacros(val c: whitebox.Context) {
  import c.universe._

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = c.Expr[Any](q"")
}
