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

class MacroCompatCTests extends FunSuite {
  test("No arg method, Tree") {
    val res = TestC.foo
    assert(res == 23)
  }

  test("One arg method, Tree") {
    val res = TestC.bar(23)
    assert(res == "bar")
  }

  test("Variadic method, Tree") {
    val res = TestC.baz(1, 2, 3)
    assert(res == 13)
  }

  test("No arg method, Expr") {
    val res = TestC.fooE
    assert(res == 23)
  }

  test("One arg method, Expr") {
    val res = TestC.barE(23)
    assert(res == "bar")
  }

  test("Variadic method, Expr") {
    val res = TestC.bazE(1, 2, 3)
    assert(res == 13)
  }
}

class MacroCompatCtxTests extends FunSuite {
  test("No arg method, Tree") {
    val res = TestCtx.foo
    assert(res == 23)
  }

  test("One arg method, Tree") {
    val res = TestCtx.bar(23)
    assert(res == "bar")
  }

  test("Variadic method, Tree") {
    val res = TestCtx.baz(1, 2, 3)
    assert(res == 13)
  }

  test("No arg method, Expr") {
    val res = TestCtx.fooE
    assert(res == 23)
  }

  test("One arg method, Expr") {
    val res = TestCtx.barE(23)
    assert(res == "bar")
  }

  test("Variadic method, Expr") {
    val res = TestCtx.bazE(1, 2, 3)
    assert(res == 13)
  }
}
