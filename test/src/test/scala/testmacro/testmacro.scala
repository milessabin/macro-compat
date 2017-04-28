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

import org.junit.Test
import org.junit.Assert._

class MacroCompatTests {
  @Test
  def noArgMethodTree: Unit = {
    val res = TestObj.foo
    assert(res == 23)
  }

  @Test
  def oneArgMethodTree: Unit = {
    val res = TestObj.bar(23)
    assert(res == "bar")
  }

  @Test
  def variadicMethodTree: Unit = {
    val res = TestObj.baz(1, 2, 3)
    assert(res == 13)
  }

  @Test
  def genericMethodImplicitweakTypeTag: Unit = {
    val res = TestObj.quux( "foo" )
    assert(res == "foo")
  }

  @Test
  def genericMethodExplicitweakTypeTag: Unit = {
    val res = TestObj.quuxExplicit( "foo" )
    assert(res == "foo")
  }

  @Test
  def noArgMethodExpr: Unit = {
    val res = TestObj.fooE
    assert(res == 23)
  }

  @Test
  def oneArgMethodExpr: Unit = {
    val res = TestObj.barE(23)
    assert(res == "bar")
  }

  @Test
  def variadicMethodExpr: Unit = {
    val res = TestObj.bazE(1, 2, 3)
    assert(res == 13)
  }

  @Test
  def noArgMethodCTree: Unit = {
    val res = TestObj.fooCT
    assert(res == 23)
  }

  @Test
  def oneArgMethodCTree: Unit = {
    val res = TestObj.barCT(23)
    assert(res == "bar")
  }

  @Test
  def variadicMethodCTree: Unit = {
    val res = TestObj.bazCT(1, 2, 3)
    assert(res == 13)
  }

  @Test
  def symbolOf: Unit = {
    val res = TestObj.symbolOf(new Foo)
    assert(res == "class Foo")

    val res2 = TestObj.symbolOf(Foo)
    assert(res2 == "object Foo")

    val res3 = TestObj.symbolOf(new Bar)
    assert(res3 == "class Bar")

    val res4 = TestObj.symbolOf(Baz)
    assert(res4 == "object Baz")
  }

  @Test
  def accessingCompanion: Unit = {
    val res = TestObj.comp(new Foo)
    assert(res == "testmacro.Foo.type")

    val res2 = TestObj.comp(Foo)
    assert(res2 == "testmacro.Foo")

    val res3 = TestObj.comp(new Bar)
    assert(res3 == "<notype>")

    val res4 = TestObj.comp(Baz)
    assert(res4 == "<notype>")
  }

  @Test
  def accessingCompanionSymbol: Unit = {
    val res = TestObj.compSym(new Foo)
    assert(res == "object Foo")

    val res2 = TestObj.compSym(Foo)
    assert(res2 == "class Foo")

    val res3 = TestObj.compSym(new Bar)
    assert(res3 == "<none>")

    val res4 = TestObj.compSym(Baz)
    assert(res4 == "<none>")
  }

  @Test
  def companionReference: Unit = {
    val res: Foo.type = TestObj.ref(new Foo)
    assert((res: Foo.type) eq Foo)

  }

  @Test
  def typecheck: Unit = {
    TestObj.typecheck
  }

  @Test
  def modifiers: Unit = {
    TestObj.modifiers
  }

  @Test
  def untypecheck: Unit = {
    val res = TestObj.untypecheck(23)
    assert((res: Int) == 23)
  }

  @Test
  def dealiasTypes: Unit = {
    type T = List[Int]
    TestObj.ensureOneTypeArg[T]
  }

  @Test
  def freshName: Unit = {
    val nme = TestObj.freshName
  }

  @Test
  def annotation: Unit = {
    class Annotation extends scala.annotation.StaticAnnotation
    @Annotation trait T

    val annTpe = TestObj.AnnotationType[T]
    val a = new Annotation
    val a0: annTpe.Ann = a
  }

  @Test
  def implicitCandidate: Unit = {
    import TestObj.materialize
    val res = implicitly[List[String]]
    assert(res == List("materialize"))
  }

  @Test
  def finalResultType: Unit = {
    TestObj.finalResultType
  }

  @Test
  def dealiasTypeArgs: Unit = {
    TestObj.dealiasTypeArgs
  }
}

class Foo
object Foo

class Bar

object Baz
