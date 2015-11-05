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

  test("symbolOf") {
    val res = Test.symbolOf(new Foo)
    assert(res == "class Foo")

    val res2 = Test.symbolOf(Foo)
    assert(res2 == "object Foo")

    val res3 = Test.symbolOf(new Bar)
    assert(res3 == "class Bar")

    val res4 = Test.symbolOf(Baz)
    assert(res4 == "object Baz")
  }

  test("Accessing companion") {
    val res = Test.comp(new Foo)
    assert(res == "testmacro.Foo.type")

    val res2 = Test.comp(Foo)
    assert(res2 == "testmacro.Foo")

    val res3 = Test.comp(new Bar)
    assert(res3 == "<notype>")

    val res4 = Test.comp(Baz)
    assert(res4 == "<notype>")
  }

  test("Accessing companion symbol") {
    val res = Test.compSym(new Foo)
    assert(res == "object Foo")

    val res2 = Test.compSym(Foo)
    assert(res2 == "class Foo")

    val res3 = Test.compSym(new Bar)
    assert(res3 == "<none>")

    val res4 = Test.compSym(Baz)
    assert(res4 == "<none>")
  }

  test("Companion reference") {
    val res: Foo.type = Test.ref(new Foo)
    assert((res: Foo.type) eq Foo)

  }

  test("Typecheck") {
    Test.typecheck
  }

  test("Modifiers") {
    Test.modifiers
  }

  test("Untypecheck") {
    val res = Test.untypecheck(23)
    assert((res: Int) == 23)
  }

  test("Dealias types") {
    type T = List[Int]
    Test.ensureOneTypeArg[T]
  }

  test("Fresh name") {
    val nme = Test.freshName
  }

  test("Annotation") {
    class Annotation extends scala.annotation.StaticAnnotation
    @Annotation trait T

    val annTpe = Test.AnnotationType[T]
    val a = new Annotation
    val a0: annTpe.Ann = a
  }
}

class Foo
object Foo

class Bar

object Baz
