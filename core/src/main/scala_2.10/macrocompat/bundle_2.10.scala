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

  def convertImpl(d: DefDef, ctxNme: TermName): DefDef = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d
    val ctxParam = q""" val $ctxNme: _root_.scala.reflect.macros.Context """
    val cvparamss = vparamss.map(_.map { param =>
      val ValDef(mods, nme, tpt, rhs) = param
      val ctpt = tpt match {
        case TreeE(t) => tq""" $ctxNme.Expr[$t] """
        case ExprE(t) => tq""" $ctxNme.Expr[$t] """
        case Repeat(TreeE(t)) => Repeat(tq""" $ctxNme.Expr[$t] """)
        case Repeat(ExprE(t)) => Repeat(tq""" $ctxNme.Expr[$t] """)
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
      }
    })

    val call = q""" inst($ctxNme).${name.toTermName}(...$cargss) """
    val (ctpt, crhs) =
      tpt match {
        case ExprE(tpt) => (tq""" $ctxNme.Expr[$tpt] """, call)
        case TreeE(tpt) => (tq""" $ctxNme.Expr[Nothing] """, q""" $ctxNme.Expr($call) """)
      }

    DefDef(mods, name, tparams, List(ctxParam) :: cvparamss, ctpt, crhs)
  }

  object MacroImpl {
    def unapply(d: DefDef): Option[DefDef] = {
      val DefDef(mods, name, tparams, vparamss, tpt, rhs) = d

      tpt match {
        case TreeE(_)|ExprE(_)
          if vparamss.forall(_.forall {
            case ValDef(_, _, TreeE(_)|ExprE(_)|Repeat(TreeE(_))|Repeat(ExprE(_)), _) => true
            case _ => false
          }) => Some(d)
        case _ => None
      }
    }
  }

  def bundleImpl(annottees: Tree*): Tree = {
    annottees match {
      case List(ClassDef(mods, typeNme, tparams, Template(parents, self, body))) =>
        val defns = body collect {
          case MacroImpl(d: DefDef) => d
        }

        val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]
        val fdefns = body filter {
          case d: DefDef if d.name == nme.CONSTRUCTOR => false
          case ValDef(mods, _, _, _) if mods.hasFlag(PARAMACCESSOR) => false
          case _ => true
        }

        val ctxNme = (body collectFirst { 
          case ValDef(mods, nme, _, _) if mods.hasFlag(PARAMACCESSOR) => nme
        }).getOrElse(c.abort(c.enclosingPosition, "Missing Context parameter"))
        val ctxTypeNme = newTypeName(c.fresh)

        val termNme = typeNme.toTermName
        val cdefns = defns.map { d => convertImpl(d, ctxNme) }

        q"""
          class $typeNme[$ctxTypeNme <: _root_.scala.reflect.macros.Context]
            (val $ctxNme: $ctxTypeNme) extends _root_.macrocompat.MacroCompat {
            object bundleContext extends _root_.macrocompat.BundleContext {
              val c: $ctxTypeNme = $typeNme.this.$ctxNme
            }
            ..$fdefns
          }
          object $termNme {
            def inst[$ctxTypeNme <: _root_.scala.reflect.macros.Context]
              ($ctxNme: $ctxTypeNme) = new $typeNme[$ctxNme.type]($ctxNme)
            ..$cdefns
          }
        """

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }
}

object BundleMacro {
  def inst[C <: Context](c: C) = new BundleMacro[c.type](c)

  def bundleImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](inst(c).bundleImpl(annottees.map(_.tree): _*))
}
