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

object MacroCompatTests extends App {
  implicit class SymMinus(s: Symbol) {
    def -(f: => Unit): Unit = f
  }

  'noArgMethodTree - {
    val res = TestObj.foo
    assert(res == 23)
  }

  'oneArgMethodTree - {
    val res = TestObj.bar(23)
    assert(res == "bar")
  }

  'variadicMethodTree - {
    val res = TestObj.baz(1, 2, 3)
    assert(res == 13)
  }

  'genericMethodImplicitweakTypeTag - {
    val res = TestObj.quux( "foo" )
    assert(res == "foo")
  }

  'genericMethodExplicitweakTypeTag - {
    val res = TestObj.quuxExplicit( "foo" )
    assert(res == "foo")
  }

  'noArgMethodExpr - {
    val res = TestObj.fooE
    assert(res == 23)
  }

  'oneArgMethodExpr - {
    val res = TestObj.barE(23)
    assert(res == "bar")
  }

  'variadicMethodExpr - {
    val res = TestObj.bazE(1, 2, 3)
    assert(res == 13)
  }

  'noArgMethodCTree - {
    val res = TestObj.fooCT
    assert(res == 23)
  }

  'oneArgMethodCTree - {
    val res = TestObj.barCT(23)
    assert(res == "bar")
  }

  'variadicMethodCTree - {
    val res = TestObj.bazCT(1, 2, 3)
    assert(res == 13)
  }

  'symbolOf - {
    val res = TestObj.symbolOf(new Foo)
    assert(res == "class Foo")

    val res2 = TestObj.symbolOf(Foo)
    assert(res2 == "object Foo")

    val res3 = TestObj.symbolOf(new Bar)
    assert(res3 == "class Bar")

    val res4 = TestObj.symbolOf(Baz)
    assert(res4 == "object Baz")
  }

  'accessingCompanion - {
    val res = TestObj.comp(new Foo)
    assert(res == "testmacro.Foo.type")

    val res2 = TestObj.comp(Foo)
    assert(res2 == "testmacro.Foo")

    val res3 = TestObj.comp(new Bar)
    assert(res3 == "<notype>")

    val res4 = TestObj.comp(Baz)
    assert(res4 == "<notype>")
  }

  'accessingCompanionSymbol - {
    val res = TestObj.compSym(new Foo)
    assert(res == "object Foo")

    val res2 = TestObj.compSym(Foo)
    assert(res2 == "class Foo")

    val res3 = TestObj.compSym(new Bar)
    assert(res3 == "<none>")

    val res4 = TestObj.compSym(Baz)
    assert(res4 == "<none>")
  }

  'companionReference - {
    val res: Foo.type = TestObj.ref(new Foo)
    assert((res: Foo.type) eq Foo)

  }

  def typecheck: Unit = {
    TestObj.typecheck
  }

  def modifiers: Unit = {
    TestObj.modifiers
  }

  'untypecheck - {
    val res = TestObj.untypecheck(23)
    assert((res: Int) == 23)
  }

  def dealiasTypes: Unit = {
    type T = List[Int]
    TestObj.ensureOneTypeArg[T]
  }

  def freshName: Unit = {
    val nme = TestObj.freshName
  }

  def annotation: Unit = {
    class Annotation extends scala.annotation.StaticAnnotation
    @Annotation trait T

    val annTpe = TestObj.AnnotationType[T]
    val a = new Annotation
    val a0: annTpe.Ann = a
  }

  'implicitCandidate - {
    import TestObj.materialize
    val res = implicitly[List[String]]
    assert(res == List("materialize"))
  }

  def finalResultType: Unit = {
    TestObj.finalResultType
  }

  def dealiasTypeArgs: Unit = {
    TestObj.dealiasTypeArgs
  }
}

class Foo
object Foo

class Bar

object Baz
