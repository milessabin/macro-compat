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

import org.scalatest.FunSuite

class MacroCompatTests extends FunSuite {
  test("No arg method, Tree") {
    val res = Test.foo
    assert(res == 23)
  }

  test("One arg method, Tree") {
    val res = Test.bar(23)
    assert(res == "bar")
  }

  test("Variadic method, Tree") {
    val res = Test.baz(1, 2, 3)
    assert(res == 13)
  }

  test("No arg method, Expr") {
    val res = Test.fooE
    assert(res == 23)
  }

  test("One arg method, Expr") {
    val res = Test.barE(23)
    assert(res == "bar")
  }

  test("Variadic method, Expr") {
    val res = Test.bazE(1, 2, 3)
    assert(res == 13)
  }

  test("Accessing companion") {
    val res = Test.comp[Foo]
    assert(res == "testmacro.Foo.type")
  }
}

class Foo
object Foo