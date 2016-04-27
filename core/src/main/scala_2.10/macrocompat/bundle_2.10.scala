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

package macrocompat

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class bundle extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BundleMacro.bundleImpl
}

class BundleMacro[C <: Context](val c: C) {
  import c.universe._
  import Flag._

  object TreeE {
    val TreeNme = newTypeName("Tree")
    val CNme = newTermName("c")
    def unapply(t: Tree): Option[Tree] = t match {
      case Ident(TreeNme) => Some(tq"Any")
      case Select(Ident(CNme), TreeNme) => Some(tq"Any")
      case x => None
    }
  }

  object ExprE {
    val ExprNme = newTypeName("Expr")
    def unapply(t: Tree): Option[Tree] = t match {
      case AppliedTypeTree(ExprNme, List(arg)) => Some(arg)
      case AppliedTypeTree(Select(_, ExprNme), List(arg)) => Some(arg)
      case AppliedTypeTree(Ident(ExprNme), List(arg)) => Some(arg)
      case _ => None
    }
  }

  object WeakTypeTagE {
    val WTTNme = newTypeName("WeakTypeTag")
    def unapply(t: Tree): Option[Tree] = t match {
      case AppliedTypeTree(Ident(WTTNme), List(arg)) => Some(arg)
      case AppliedTypeTree(Select(_, WTTNme), List(arg)) => Some(arg)
      case _ => None
    }
  }

  object Repeat {
    val Scala = newTermName("scala")
    val Repeated = newTypeName("<repeated>")

    def apply(t: Tree): Tree =
      AppliedTypeTree(Select(Select(Ident(nme.ROOTPKG), Scala), Repeated), List(t))

    def unapply(t: Tree): Option[Tree] = t match {
      case AppliedTypeTree(Select(Select(Ident(nme.ROOTPKG), Scala), Repeated), List(tpt)) => Some(tpt)
      case _ => None
    }
  }

  def mkForwarder(d: DefDef, macroClassNme: TypeName): DefDef = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d
    val ctxNme = newTermName(c.fresh)
    val ctxParam = q""" val $ctxNme: _root_.scala.reflect.macros.Context """

    val targs = tparams.map(_.name)

    val cvparamss = vparamss.map(_.map { param =>
      val ValDef(mods, nme, tpt, rhs) = param
      val ctpt = tpt match {
        case TreeE(t) => tq""" $ctxNme.Expr[$t] """
        case ExprE(t) => tq""" $ctxNme.Expr[$t] """
        case Repeat(TreeE(t)) => Repeat(tq""" $ctxNme.Expr[$t] """)
        case Repeat(ExprE(t)) => Repeat(tq""" $ctxNme.Expr[$t] """)
        case WeakTypeTagE(t) => tq""" $ctxNme.WeakTypeTag[$t] """
      }
      ValDef(mods, nme, ctpt, rhs)
    })
    val mi = newTermName(c.fresh("inst"))
    val cargss = vparamss.map(_.map { param =>
      val ValDef(mods, nme, tpt, rhs) = param
      tpt match {
        case TreeE(_) => q""" $nme.tree.asInstanceOf[$mi.c.universe.Tree] """
        case ExprE(t) => q""" $nme.asInstanceOf[$mi.c.Expr[$t]] """
        case Repeat(TreeE(_)) => q""" $nme.map(_.tree.asInstanceOf[$mi.c.universe.Tree]): _* """
        case Repeat(ExprE(t)) => q""" $nme.map(_.asInstanceOf[$mi.c.Expr[$t]]): _* """
        case WeakTypeTagE(t) => q""" $nme.asInstanceOf[$mi.c.universe.WeakTypeTag[$t]] """
      }
    })

    val call =
      q"""
        val $mi =
          new $macroClassNme(
            new _root_.macrocompat.RuntimeCompatContext(
              $ctxNme.asInstanceOf[_root_.scala.reflect.macros.runtime.Context]
            )
          )
        $mi.${name.toTermName}[..$targs](...$cargss)
      """

    val (ctpt, crhs) =
      tpt match {
        case ExprE(tpt) => (
          tq""" $ctxNme.Expr[$tpt] """,
           q""" $ctxNme.Expr[$tpt](_root_.macrocompat.BundleMacro.fixPositions[$ctxNme.type]($ctxNme)($call.tree.asInstanceOf[$ctxNme.universe.Tree])) """
        )
        case TreeE(tpt) => (
          tq""" $ctxNme.Expr[Nothing] """,
           q""" $ctxNme.Expr[Nothing](_root_.macrocompat.BundleMacro.fixPositions[$ctxNme.type]($ctxNme)($call.asInstanceOf[$ctxNme.universe.Tree])) """
        )
      }

    DefDef(mods, name, tparams, List(ctxParam) :: cvparamss, ctpt, crhs)
  }

  object MacroImpl {
    def unapply(d: DefDef): Option[DefDef] = {
      val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d

      tpt match {
        case TreeE(_)|ExprE(_)
          if vparamss.forall(_.forall {
            case ValDef(mods, _, _, _) if mods hasFlag IMPLICIT => true
            case ValDef(_, _, TreeE(_)|ExprE(_)|Repeat(TreeE(_))|Repeat(ExprE(_))|WeakTypeTagE(_), _) => true
            case _ => false
          }) => Some(d)
        case _ => None
      }
    }
  }

  def stripPositions(tree: Tree): Tree = {
    if(!tree.isEmpty) {
      tree.setPos(NoPosition)
      tree.children.foreach(stripPositions)
    }
    tree
  }

  def fixPositions(tree: Tree): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    if(global.settings.Yrangepos.value)
      stripPositions(tree)
    else
      tree
  }

  def bundleImpl(annottees: Tree*): Tree = {
    annottees match {
      case List(clsDef: ClassDef) => mkMacroClsAndObjTree(clsDef, None)

      case List(clsDef: ClassDef, objDef: ModuleDef) => mkMacroClsAndObjTree(clsDef, Some(objDef))

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }

  def mkMacroClsAndObjTree(clsDef: ClassDef, objDef: Option[ModuleDef]) = {
    val ClassDef(mods, macroClassNme, tparams, Template(parents, self, body)) = clsDef

    // The private forwarding defintions (appliedType, Modifiers) below are needed because they need
    // to appear to be accessible as a result of import c.universe._. They can't be added to
    // c.compatUniverse because that results in import ambiguities. Note that the definitions are
    // private to avoid override conflicts in stacks of inherited bundles.
    val Block(stats, _) =
      q"""
        import c.compatUniverse._
        import _root_.macrocompat.TypecheckerContextExtensions._

        private def appliedType(tc: c.universe.Type, ts: _root_.scala.collection.immutable.List[c.universe.Type]): c.universe.Type = c.universe.appliedType(tc, ts)

        private def appliedType(tc: c.universe.Type, ts: c.universe.Type*): c.universe.Type = c.universe.appliedType(tc, ts.toList)

        private val Annotation = c.compatUniverse.CompatAnnotation

        private val Modifiers = c.compatUniverse.CompatModifiers
      """

    // For now all macro bundles must have a Context constructor argument named "c". See,
    //   https://gitter.im/scala/scala?at=55ef0ffe24362d5253fe3a51
    val ctxNme = newTermName("c")
    val mixinCtorNme = newTermName("$init$")

    val (prefix0, ctor :: suffix0) = body.span {
      case d: DefDef if d.name == nme.CONSTRUCTOR || d.name == mixinCtorNme => false
      case _ => true
    }

    val (prefix1, suffix1) = suffix0.span {
      case ValDef(_, nme, _, _) if nme == ctxNme => false
      case _ => true
    }

    val suffix2 =
      suffix1 match {
        case decl :: rest => prefix1 ++ (decl :: stats) ++ rest
        case _ => stats ++ prefix1 ++ suffix1
      }

    val newBody = prefix0 ++ List(ctor) ++ suffix2

    val macroClass = ClassDef(mods, macroClassNme, tparams, Template(parents, self, newBody))

    val res =
      if(mods.hasFlag(ABSTRACT))
        objDef match {
          case Some(obj) =>
            q"""
              $macroClass

              $obj
            """
          case _ =>
            macroClass
        }
      else {
        val (objEarlydefns, objParents, objBody) = objDef match {
          case Some(q"$objMods object $objTname extends { ..$objEarlydefns } with ..$objParents { $objSelf => ..$objBody }") => (objEarlydefns, objParents, objBody)
          case None => (Nil, List(tq"_root_.scala.AnyRef"), Nil)
        }

        val defns = body collect {
          case MacroImpl(d: DefDef) => d
        }

        val forwarders = defns.map { d => mkForwarder(d, macroClassNme) }
        val macroObjectNme = macroClassNme.toTermName

        q"""
          $macroClass

          object $macroObjectNme extends { ..$objEarlydefns } with ..$objParents {
            ..$forwarders

            ..$objBody
          }
        """
      }

    fixPositions(res)
  }
}

object BundleMacro {
  def inst[C <: Context](c: C) = new BundleMacro[c.type](c)

  def bundleImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](inst(c).bundleImpl(annottees.map(_.tree): _*))

  def fixPositions[C <: Context](c: C)(tree: c.Tree): c.Tree =
    inst(c).fixPositions(tree)
}
