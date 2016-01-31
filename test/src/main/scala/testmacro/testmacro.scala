/*
 * Copyright (c) 2015-6 Miles Sabin
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

  def symbolOf[T](t: T): String = macro TestMacro.symbolOfImpl[T]

  def compSym[T](t: T): String = macro TestMacro.compSymImpl[T]

  def ref[T](t: T): AnyRef = macro TestMacro.refImpl[T]

  def typecheck: Unit = macro TestMacro.typecheckImpl

  def untypecheck[T](t: T): T = macro TestMacro.untypecheckImpl[T]

  def modifiers: Unit = macro TestMacro.modifiersImpl

  def ensureOneTypeArg[T]: Unit = macro TestMacro.ensureOneTypeArgImpl[T]

  def freshName: String = macro TestMacro.freshNameImpl

  def finalResultType: Unit = macro TestMacro.finalResultType

  def dealiasTypeArgs: Unit = macro TestMacro.dealiasTypeArgs

  implicit def materialize: List[String] = macro TestMacro.materialize

  trait AnnotationType[T] {
    type Ann
  }
  object AnnotationType {
    def apply[T](implicit annTpe: AnnotationType[T]): Aux[T, annTpe.Ann] = annTpe
    type Aux[T, Ann0] = AnnotationType[T] { type Ann = Ann0 }
    implicit def materialize[T, Ann]: Aux[T, Ann] = macro TestMacro.annotationTypeImpl[T, Ann]
    def instance[T, Ann0]: Aux[T, Ann0] = new AnnotationType[T] { type Ann = Ann0 }
  }

  implicit def convertImpl[T](t: T): String = macro TestMacro.convertImpl[T]

  def implicitProperties: (String, String, String, String) = macro TestMacro.implicitProperties
}

@bundle
abstract class TestMacroBase {
  val c: whitebox.Context
  import c.universe._

  def abstractClassAbstractMethod: Tree

  def abstractClassMethodUsingCompanionObject: Tree = q"${TestMacroBase.foo}"
}

object TestMacroBase {
  def foo = 1
}

@bundle
trait TestUtil {
  val c: whitebox.Context
  import c.universe._

  def util(t: Type): Tree = {
    q""" () """
  }

  def traitAbstractMethod: Tree

  def traitMethodUsingCompanionObject: Tree = q"${TestUtil.foo}"
}

object TestUtil {
  def foo = 1
}

@bundle
class TestMacro(val c: whitebox.Context) extends TestMacroBase with TestUtil {
  import c.universe._

  // Test for early use of context
  val intTpe = typeOf[Int]
  val bounds = internal.typeBounds(typeOf[Any], typeOf[Nothing])

  def fooImpl: Tree = {
    val nme0 = TermName(c.freshName)
    val TermName(str0) = nme0
    val nme1 = TypeName(c.freshName)
    val TypeName(str1) = nme1
    val i = Ident(nme0)

    val nme2 = TermName(c.freshName("pfx"))
    val nme3 = c.freshName(TermName("pfx"))

    val nme4 = termNames.CONSTRUCTOR
    val nme5 = typeNames.WILDCARD

    q""" 23 """
  }

  def barImpl(i: Tree): Tree = q""" "bar" """

  def bazImpl(is: Tree*): Tree = q""" 13 """

  def quuxImpl[T: WeakTypeTag](t: Tree): Tree = t

  def fooEImpl: c.Expr[Int] = c.Expr[Int](q""" 23 """)

  def barEImpl(i: c.Expr[Int]): c.Expr[String] = c.Expr[String](q""" "bar" """)

  def bazEImpl(is: c.Expr[Int]*): c.Expr[Int] = c.Expr[Int](q""" 13 """)

  def quuxEImpl[T: c.WeakTypeTag](t: c.Expr[T]): c.Expr[T] = c.Expr[T](t.tree)

  def compImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val typ = weakTypeOf[T]
    q""" ${typ.companion.toString } """
  }

  def symbolOfImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val sym = symbolOf[T]
    q""" ${sym.toString } """
  }

  def compSymImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val sym = symbolOf[T]
    q""" ${sym.companion.toString } """
  }

  def refImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val sym = symbolOf[T]
    val ref = c.internal.gen.mkAttributedRef(sym.companion)
    q""" $ref """
  }

  def typecheckImpl: Tree = {
    assert(c.typecheck(q""" 1+1 """, c.TERMmode).tpe <:< typeOf[Int])
    assert(c.typecheck(tq""" Int """, c.TYPEmode).tpe =:= typeOf[Int])
    assert(c.typecheck(tq""" List """, c.TYPEmode).tpe =:= typeOf[List[Any]].typeConstructor)
    assert(c.typecheck(tq""" List[Int] """, c.TYPEmode).tpe =:= typeOf[List[Int]])
    assert(c.typecheck(tq""" Map """, c.TYPEmode).tpe =:= typeOf[Map[Any, Any]].typeConstructor)
    assert(c.typecheck(tq""" Map[Int, String] """, c.TYPEmode).tpe =:= typeOf[Map[Int, String]])
    q""" () """
  }

  def untypecheckImpl[T: c.WeakTypeTag](t: c.Expr[T]): Tree = {
    val tree = c.untypecheck(t.tree)
    q""" $tree """
  }

  def modifiersImpl: Tree = {
    val mods = Modifiers(Flag.IMPLICIT, TypeName("Pw"), List(EmptyTree))
    val Modifiers(flags, pw, annots) = mods
    q""" () """
  }

  def ensureOneTypeArgImpl[T: c.WeakTypeTag]: Tree =
    weakTypeOf[T].dealias match {
      case TypeRef(_, _, List(_)) => q" () "
      case _ => c.abort(
        c.enclosingPosition,
        s"Cannot dealias ${weakTypeOf[T]} to a type with one type argument"
      )
    }

  def freshNameImpl: Tree = {
    val name = c.freshName().toString
    q" $name "
  }

  def annotationTypeImpl[T: c.WeakTypeTag, R: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val annotations = tpe.dealias.typeSymbol.annotations

    annotations.map { ann =>
      // Ensuring we can call this one
      ann.tree.children.tail
    }

    // Test 2.11.x apply
    val tree = annotations.head.tree
    val ann2 = Annotation(tree)

    annotations match {
      case Nil => c.abort(c.enclosingPosition, s"No annotation found on $tpe")
      case ann :: _ => q" _root_.testmacro.Test.AnnotationType.instance[$tpe, ${ann.tree.tpe}] "
    }
  }

  def useUtil: Tree =
    util(typeOf[Int])

  def const: Tree =
    TypeTree(internal.constantType(Constant(23)))

  def materialize: Tree = {
    import c.ImplicitCandidate

    val oi = c.openImplicits
    val ImplicitCandidate(pre, sym, pt, tree) = oi.head
    val s = sym.name.toString
    q"List($s)"
  }

  def convertImpl[T](t: Expr[T]): Tree = ???

  def implicitProperties: Tree = {
    val i = c.openImplicits.head
    val pre = i.pre.toString
    val sym = i.sym.toString
    val pt = i.pt.toString
    val tree = i.tree.toString
    q"($pre, $sym, $pt, $tree)"
  }

  def traitAbstractMethod: Tree = q"()"

  def abstractClassAbstractMethod: Tree = q"()"

  def finalResultType: Tree = {
    trait Foo {
      def foo: Unit
      def bar(): Unit
      def baz(i: Int): Unit
      def quux(i: Int)(s: String): Unit
    }

    def check(nme: String, tpe: Type): Unit =
      assert(weakTypeOf[Foo].decl(TermName(nme)).typeSignature.finalResultType =:= tpe)

    check("foo", typeOf[Unit])
    check("bar", typeOf[Unit])
    check("baz", typeOf[Unit])
    check("quux", typeOf[Unit])

    q"()"
  }

  def dealiasTypeArgs: Tree = {
    val tpe = weakTypeOf[Either[Int, _]]
    val dealiased = tpe.dealias
    val tArgs = dealiased.typeArgs
    val normalized = appliedType(dealiased.typeConstructor, tArgs)
    q"()"
  }
}
