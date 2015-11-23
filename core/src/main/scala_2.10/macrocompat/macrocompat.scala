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

import scala.reflect.macros.{ Context, TypecheckException }

trait MacroCompat {
  val c: Context
  import c.universe._

  object GlobalConversions {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

    implicit def globalType(tpe: Type): global.Type = tpe.asInstanceOf[global.Type]
    implicit def globalSymbol(sym: Symbol): global.Symbol = sym.asInstanceOf[global.Symbol]
    implicit def globalTypeSymbol(sym: TypeSymbol): global.TypeSymbol = sym.asInstanceOf[global.TypeSymbol]
    implicit def globalTree(tree: Tree): global.Tree = tree.asInstanceOf[global.Tree]
    implicit def globalAnnotation(ann: Annotation): global.Annotation = ann.asInstanceOf[global.Annotation]

    implicit def macroType(tpe: global.Type): Type = tpe.asInstanceOf[Type]
    implicit def macroSymbol(sym: global.Symbol): Symbol = sym.asInstanceOf[Symbol]
    implicit def macroTypeSymbol(sym: global.TypeSymbol): TypeSymbol = sym.asInstanceOf[TypeSymbol]
    implicit def macroTree(tree: global.Tree): Tree = tree.asInstanceOf[Tree]
    implicit def macroAnnotation(ann: global.Annotation): Annotation = ann.asInstanceOf[Annotation]
  }

  import GlobalConversions._

  def symbolOf[T: WeakTypeTag]: TypeSymbol =
    weakTypeOf[T].typeSymbolDirect.asType

  object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }

  object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  object Modifiers extends ModifiersCreator {
    def apply(flags: FlagSet, privateWithin: Name = typeNames.EMPTY, annots: List[Tree] = Nil): Modifiers =
      c.universe.Modifiers(flags, privateWithin, annots)

    def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])] =
      Some((mods.flags, mods.privateWithin, mods.annotations))
  }

  lazy val termNames = nme
  lazy val typeNames = tpnme

  def freshName() = c.fresh
  def freshName(name: String) = c.fresh(name)
  def freshName[NameType <: Name](name: NameType) = c.fresh(name)

  implicit def mkContextOps(c0: c.type): this.type = this

  type TypecheckMode = Int
  val TERMmode = global.analyzer.EXPRmode
  val TYPEmode = global.analyzer.HKmode

  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    val universe: global.type = global
    val callsiteTyper = c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val context = callsiteTyper.context
    val withImplicitFlag = if (!withImplicitViewsDisabled) (context.withImplicitsEnabled[Tree] _) else (context.withImplicitsDisabled[Tree] _)
    val withMacroFlag = if (!withMacrosDisabled) (context.withMacrosEnabled[Tree] _) else (context.withMacrosDisabled[Tree] _)
    def withContext(tree: => Tree) = withImplicitFlag(withMacroFlag(tree))
    def withWrapping(tree: Tree)(op: Tree => Tree) = if (mode == TERMmode) universe.wrappingIntoTerm(tree)(op.asInstanceOf[universe.Tree => universe.Tree]) else op(tree)
    def typecheckInternal(tree: Tree) = callsiteTyper.silent(_.typed(universe.duplicateAndKeepPositions(tree), mode, pt), reportAmbiguousErrors = false)
    withWrapping(tree)(wrappedTree => withContext(typecheckInternal(wrappedTree) match {
      case universe.analyzer.SilentResultValue(result) =>
        result
      case error @ universe.analyzer.SilentTypeError(_) =>
        if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
        universe.EmptyTree
    })).asInstanceOf[Tree]
  }

  def untypecheck(tree: Tree): Tree = c.resetLocalAttrs(tree)

  implicit class TypeOps(tpe: Type) {
    def typeParams = tpe.typeSymbol.asType.typeParams

    def typeArgs: List[Type] = tpe match {
      case TypeRef(_, _, args) => args
      case _ => Nil
    }

    def companion: Type = {
      val sym = tpe.typeSymbolDirect
      if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol.tpe
      else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol.tpe
      else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol.info
      else NoType
    }

    def decl(nme: Name): Symbol = tpe.declaration(nme)

    def decls = tpe.declarations

    def dealias: Type = tpe.normalize

    def finalResultType: Type = (tpe: global.Type).finalResultType
  }

  implicit class MethodSymbolOps(sym: MethodSymbol) {
    def paramLists = sym.paramss
  }

  implicit class SymbolOps(sym: Symbol) {
    def companion: Symbol = {
      if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol
      else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol
      else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol
      else NoSymbol
    }

    def info: Type = sym.typeSignature
    def infoIn(site: Type): Type = sym.typeSignatureIn(site)

    def isConstructor: Boolean = sym.isMethod &&sym.asMethod.isConstructor

    def isAbstract: Boolean = sym.isAbstractClass

    def overrides: List[Symbol] = sym.allOverriddenSymbols
  }

  implicit class TreeOps(tree: Tree) {
    def nonEmpty = !tree.isEmpty
  }

  implicit class AnnotationOps(ann: Annotation) {
    // cut-n-pasted (with the comments) from
    // https://github.com/scala/scala/blob/v2.11.7/src/reflect/scala/reflect/internal/AnnotationInfos.scala#L348-L382
    private def annotationToTree(ann: global.Annotation): Tree = {
      import global._, definitions._

      def reverseEngineerArgs(): List[Tree] = {
        def reverseEngineerArg(jarg: ClassfileAnnotArg): Tree = jarg match {
          case LiteralAnnotArg(const) =>
            val tpe = if (const.tag == UnitTag) UnitTpe else ConstantType(const)
            Literal(const) setType tpe
          case ArrayAnnotArg(jargs) =>
            val args = jargs map reverseEngineerArg
            // TODO: I think it would be a good idea to typecheck Java annotations using a more traditional algorithm
            // sure, we can't typecheck them as is using the `new jann(foo = bar)` syntax (because jann is going to be an @interface)
            // however we can do better than `typedAnnotation` by desugaring the aforementioned expression to
            // something like `new jann() { override def annotatedType() = ...; override def foo = bar }`
            // and then using the results of that typecheck to produce a Java-compatible classfile entry
            // in that case we're going to have correctly typed Array.apply calls, however that's 2.12 territory
            // and for 2.11 exposing an untyped call to ArrayModule should suffice
            Apply(Ident(ArrayModule), args.toList)
          case NestedAnnotArg(ann: Annotation) =>
            annotationToTree(ann)
          case _ =>
            EmptyTree
        }
        def reverseEngineerArgs(jargs: List[(Name, ClassfileAnnotArg)]): List[Tree] = jargs match {
          case (name, jarg) :: rest => AssignOrNamedArg(Ident(name), reverseEngineerArg(jarg)) :: reverseEngineerArgs(rest)
          case Nil => Nil
        }
        if (ann.javaArgs.isEmpty) ann.scalaArgs
        else reverseEngineerArgs(ann.javaArgs.toList)
      }

      // TODO: at the moment, constructor selection is unattributed, because AnnotationInfos lack necessary information
      // later on, in 2.12, for every annotation we could save an entire tree instead of just bits and pieces
      // but for 2.11 the current situation will have to do
      val ctorSelection = Select(New(TypeTree(ann.atp)), nme.CONSTRUCTOR)
      Apply(ctorSelection, reverseEngineerArgs()) setType ann.atp
    }

    def tree: Tree = annotationToTree(ann)
  }

  case class ImplicitCandidate(pre: Type, sym: Symbol, pt: Type, tree: Tree)

  object ImplicitCandidate {
    def unapply(t: (Type, Tree)): Option[(Type, Symbol, Type, Tree)] = tryUnapply(t).right.toOption

    def tryUnapply(t: (Type, Tree)): Either[String, (Type, Symbol, Type, Tree)] = {
      val (pt, tree) = t
      import scala.language.existentials
      val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper
      typer.context.openImplicits.filter(oi => oi.pt == pt && oi.tree == tree) match {
        case List(oi) => Right((
          oi.info.pre.asInstanceOf[Type],
          oi.info.sym.asInstanceOf[Symbol],
          oi.pt.asInstanceOf[Type],
          oi.tree.asInstanceOf[Tree]
        ))
        case Nil => Left(s"Failed to identify ImplicitCandidate for $t, none match")
        case xs => Left(s"Failed to identify ImplicitCandidate for $t, ${xs.size} match")
      }
    }
  }

  implicit def tupleToImplicitCandidate(t: (Type, Tree)): ImplicitCandidate = {
    ImplicitCandidate.tryUnapply(t) match {
      case Left(s) => c.abort(c.enclosingPosition, s)
      case Right((pre, sym, pt, tree)) => ImplicitCandidate(pre, sym, pt, tree)
    }
  }

  def appliedType(tc: Type, ts: List[Type]): Type = c.universe.appliedType(tc, ts)
  def appliedType(tc: Type, ts: Type*): Type = c.universe.appliedType(tc, ts.toList)

  def showCode(t: Tree): String = show(t)

  object internal {
    def constantType(c: Constant): ConstantType = ConstantType(c)

    def polyType(tparams: List[Symbol], tpe: Type): Type = c.universe.polyType(tparams, tpe)

    def enclosingOwner: Symbol = {
      val internalContext = c.asInstanceOf[scala.reflect.macros.runtime.Context]
      val internalOwner = internalContext.callsiteTyper.context.owner
      internalOwner.asInstanceOf[Symbol]
    }

    object gen {
      def mkAttributedRef(sym: Symbol): Tree =
        global.gen.mkAttributedRef(sym)

      def mkAttributedRef(pre: Type, sym: Symbol): Tree =
        global.gen.mkAttributedRef(pre, sym)
    }

    object decorators

    def thisType(sym: Symbol): Type = ThisType(sym)

    def singleType(pre: Type, sym: Symbol): Type = SingleType(pre, sym)

    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = c.universe.typeRef(pre, sym, args)

    def setInfo(sym: Symbol, tpe: Type): Symbol = sym.setTypeSignature(tpe)

    def newTermSymbol(owner: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol =
      owner.newTermSymbol(name, pos, flags)
  }
}
