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
    def unapply(t: Tree): Option[Tree] = t match {
      case Ident(TreeNme) => Some(tq"Any")
      case _ => None
    }
  }

  object ExprE {
    val ExprNme = newTypeName("Expr")
    def unapply(t: Tree): Option[Tree] = t match {
      case AppliedTypeTree(ExprNme, List(arg)) => Some(arg)
      case AppliedTypeTree(Select(_, ExprNme), List(arg)) => Some(arg)
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

  def mkForwarder(d: DefDef, typeNme: TypeName, rename: TermName): DefDef = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d
    val ctxNme = newTermName(c.fresh("forwarderCtxNme"))
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
    val cargss = vparamss.map(_.map { param =>
      val ValDef(mods, nme, tpt, rhs) = param
      tpt match {
        case TreeE(_) => q""" $nme.tree """
        case ExprE(_) => q""" $nme """
        case Repeat(TreeE(_)) => q""" $nme.map(_.tree): _* """
        case Repeat(ExprE(_)) => q""" $nme: _* """
        case WeakTypeTagE(_) => q""" $nme """
      }
    })

    val instNme = newTermName(c.fresh("forwarderInstNme"))
    val call = q""" $instNme.${name.toTermName}[..$targs](...$cargss) """
    val (ctpt, wrap) =
      tpt match {
        case ExprE(tpt) => (
          tq""" $ctxNme.Expr[$tpt] """,
           q""" $ctxNme.Expr[$tpt](_root_.macrocompat.BundleMacro.fixPositions[$ctxNme.type]($ctxNme)($call.tree)) """
        )
        case TreeE(tpt) => (
          tq""" $ctxNme.Expr[Nothing] """,
           q""" $ctxNme.Expr[Nothing](_root_.macrocompat.BundleMacro.fixPositions[$ctxNme.type]($ctxNme)($call)) """
         )
      }

    val crhs =
      q"""
        {
          val $instNme = new $typeNme[$ctxNme.type]($ctxNme)
          $wrap
        }
      """

    DefDef(mods, name, tparams, List(ctxParam) :: cvparamss, ctpt, crhs)
  }

  object MacroImpl {
    def unapply(d: DefDef): Option[DefDef] = {
      val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d

      tpt match {
        case TreeE(_)|ExprE(_)
          if vparamss.forall(_.forall {
            case ValDef(mods, _, _, _) if mods hasFlag IMPLICIT => true
            case ValDef(_, _, TreeE(_)|ExprE(_)|Repeat(TreeE(_))|Repeat(ExprE(_))|WeakTypeTag(_), _) => true
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
      case List(ClassDef(mods, macroClassNme, tparams, Template(parents, self, body)))
        if mods.hasFlag(ABSTRACT) =>
        val newParents =
          tq"_root_.macrocompat.MacroCompat" ::
          parents.filter {
            case tq"scala.AnyRef" => false
            case _ => true
          }
        val res =
        ClassDef(mods, macroClassNme, tparams,
          Template(newParents, self, body))
        fixPositions(res)

      case List(clsDef: ClassDef) => mkMacroClsAndObjTree(clsDef, Nil)

      case List(clsDef: ClassDef, q"object $objName { ..$objBody }") => mkMacroClsAndObjTree(clsDef, objBody)

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }

  def mkMacroClsAndObjTree(clsDef: ClassDef, objBody: List[Tree]) = {
    val ClassDef(_, macroClassNme, _, Template(parents, _, body)) = clsDef

    val defns = body collect {
      case MacroImpl(d: DefDef) => d
    }

    val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]
    val macroDefns = body filter {
      case d: DefDef if d.name == nme.CONSTRUCTOR => false
      case ValDef(mods, _, _, _) if mods.hasFlag(PARAMACCESSOR) => false
      case _ => true
    }

    val clientCtxNme = (body collectFirst {
      case ValDef(mods, nme, _, _) if mods.hasFlag(PARAMACCESSOR) => nme
    }).getOrElse(c.abort(c.enclosingPosition, "Missing Context parameter"))

    val compatNme = newTermName("c")
    val compatTypeNme = newTypeName(c.fresh("compatTypeNme"))
    val forwarders = defns.map { d => mkForwarder(d, macroClassNme, clientCtxNme) }
    // For now all macro bundles must have a Context constructor argument named "c". See,
    //   https://gitter.im/scala/scala?at=55ef0ffe24362d5253fe3a51
    // We support renaming on the offchance that this will get fixed.
//  val alias =
//    if(clientCtxNme == compatNme) List()
//    else List(q""" val $clientCtxNme: $compatNme.type = $compatNme """)

    val alias = List(q""" val c0: $compatNme.type = $compatNme """)

    val macroObjectNme = macroClassNme.toTermName

    val res =
    q"""
      class $macroClassNme[$compatTypeNme <: scala.reflect.macros.Context]
        (val $compatNme: $compatTypeNme) extends $macroObjectNme.Stub

      object $macroObjectNme {
        trait Stub extends ..$parents with _root_.macrocompat.MacroCompat {
          ..$alias
          ..$macroDefns
        }

        ..$forwarders

        ..$objBody
      }
    """
    val t = fixPositions(res)
    println(show(t))
    t
  }
}

object BundleMacro {
  def inst[C <: Context](c: C) = new BundleMacro[c.type](c)

  def bundleImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](inst(c).bundleImpl(annottees.map(_.tree): _*))

  def fixPositions[C <: Context](c: C)(tree: c.Tree): c.Tree =
    inst(c).fixPositions(tree)
}
