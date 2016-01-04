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

import scala.reflect.macros.{ Context, TypecheckException, Universe }

class ProxyUniverse[U <: Universe] extends Universe

trait ProxyContext[C <: Context] extends Context {
  val c: C
  lazy val universe: c.universe.type = c.universe
  import universe._

  def Expr[T: WeakTypeTag](tree: Tree) = c.Expr[T](tree)
  override val Expr = c.Expr
  def TypeTag[T](tpe: Type) = c.TypeTag(tpe)
  def WeakTypeTag[T](tpe: Type) = c.WeakTypeTag(tpe)

  val mirror = c.mirror
  type PrefixType = c.PrefixType
  val prefix = c.prefix

  val enclosingClass = c.enclosingClass
  val enclosingImplicits = c.enclosingImplicits
  val enclosingMacros = c.enclosingMacros
  val enclosingMethod = c.enclosingMethod
  val enclosingPosition = c.enclosingPosition
  val enclosingRun = c.enclosingRun
  val enclosingUnit = c.enclosingUnit
  val macroApplication = c.macroApplication

  def eval[T](expr: Expr[T]) = c.eval(expr)

  def literalNull = c.literalNull
  def literalUnit = c.literalUnit
  def literalTrue = c.literalTrue
  def literalFalse = c.literalFalse
  def literal(x: Boolean) = c.literal(x)
  def literal(x: Byte) = c.literal(x)
  def literal(x: Short) = c.literal(x)
  def literal(x: Int) = c.literal(x)
  def literal(x: Long) = c.literal(x)
  def literal(x: Float) = c.literal(x)
  def literal(x: Double) = c.literal(x)
  def literal(x: String) = c.literal(x)
  def literal(x: Char) = c.literal(x)

  def abort(pos: Position, msg: String) = c.abort(pos, msg)
  def echo(pos: Position, msg: String) = c.echo(pos, msg)
  def error(pos: Position, msg: String) = c.error(pos, msg)
  def hasErrors = c.hasErrors
  def hasWarnings = c.hasWarnings
  def info(pos: Position, msg: String, force: Boolean) = c.info(pos, msg, force)
  def warning(pos: Position, msg: String) = c.warning(pos, msg)

  def classPath = c.classPath
  def compilerSettings = c.compilerSettings
  def settings = c.settings

  def fresh[NameType <: Name](name: NameType) = c.fresh(name)
  def fresh(name: String) = c.fresh(name)
  def fresh() = c.fresh()

  def parse(code: String) = c.parse(code)

  def reifyEnclosingRuntimeClass = c.reifyEnclosingRuntimeClass
  def reifyRuntimeClass(tpe: Type, concrete: Boolean = false) = c.reifyRuntimeClass(tpe, concrete)
  def reifyTree(universe: Tree, mirror: Tree, tree: Tree) = c.reifyTree(universe, mirror, tree)
  def reifyType(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean = false) = c.reifyType(universe, mirror, tpe, concrete)
  def unreifyTree(tree: Tree) = c.unreifyTree(tree)

  def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition) = c.inferImplicitValue(pt, silent, withMacrosDisabled, pos)
  def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, pos: Position = enclosingPosition) = c.inferImplicitView(tree, from, to, silent, withMacrosDisabled, pos)
  def openImplicits = c.openImplicits
  def openMacros = c.openMacros
  def resetAllAttrs(tree: Tree) = c.resetAllAttrs(tree)
  def resetLocalAttrs(tree: Tree) = c.resetLocalAttrs(tree)
  def typeCheck(tree: Tree, pt: Type = WildcardType, silent: Boolean = true, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false) = c.typeCheck(tree, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
}

class CompatContext[C <: Context](val c: C) extends ProxyContext[C] {
  import universe._

  object GlobalConversions {

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

    val callsiteTyper = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val globalContext = callsiteTyper.context

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

  def freshName() = c.fresh
  def freshName(name: String) = c.fresh(name)
  def freshName[NameType <: Name](name: NameType) = c.fresh(name)

  case class ImplicitCandidate(pre: Type, sym: Symbol, pt: Type, tree: Tree)

  object ImplicitCandidate {
    def unapply(t: (Type, Tree)): Option[(Type, Symbol, Type, Tree)] = tryUnapply(t).right.toOption

    def tryUnapply(t: (Type, Tree)): Either[String, (Type, Symbol, Type, Tree)] = {
      val (pt, tree) = t
      globalContext.openImplicits.filter(oi => oi.pt == pt && oi.tree == tree) match {
        case List(oi) => Right((oi.info.pre, oi.info.sym, oi.pt, oi.tree))
        case Nil => Left(s"Failed to identify ImplicitCandidate for $t, none match")
        case xs => Left(s"Failed to identify ImplicitCandidate for $t, ${xs.size} match")
      }
    }
  }

  type TypecheckMode = Int
  val TERMmode = global.analyzer.EXPRmode
  val TYPEmode = global.analyzer.HKmode

  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    val universe: global.type = global
    type Tree = universe.Tree
    type Type = universe.Type
    val context = callsiteTyper.context
    val withImplicitFlag = if (!withImplicitViewsDisabled) (context.withImplicitsEnabled[Tree] _) else (context.withImplicitsDisabled[Tree] _)
    val withMacroFlag = if (!withMacrosDisabled) (context.withMacrosEnabled[Tree] _) else (context.withMacrosDisabled[Tree] _)
    def withContext(tree: => Tree) = withImplicitFlag(withMacroFlag(tree))
    def withWrapping(tree: Tree)(op: Tree => Tree) = if (mode == TERMmode) universe.wrappingIntoTerm(tree)(op) else op(tree)
    def typecheckInternal(tree: Tree): universe.analyzer.SilentResult[Tree] =
      callsiteTyper.silent(_.typed(universe.duplicateAndKeepPositions(tree), mode, pt), reportAmbiguousErrors = false)
    withWrapping(tree)(wrappedTree => withContext(typecheckInternal(wrappedTree) match {
      case universe.analyzer.SilentResultValue(result) =>
        result
      case error @ universe.analyzer.SilentTypeError(_) =>
        if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
        universe.EmptyTree
    }))
  }

  def untypecheck(tree: Tree): Tree = c.resetLocalAttrs(tree)

  object internal {
    def constantType(c: Constant): ConstantType = ConstantType(c)

    def polyType(tparams: List[Symbol], tpe: Type): Type = c.universe.polyType(tparams, tpe)

    def enclosingOwner: Symbol = callsiteTyper.context.owner

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

    def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree =
      tree.substituteSymbols(from, to)

    def typeBounds(lo: Type, hi: Type): TypeBounds = TypeBounds(lo, hi)
  }
}

trait MacroCompat { outer =>
  type C <: Context
  val c: CompatContext[C]
  import c.universe._
  import c.GlobalConversions._

  import c.ImplicitCandidate

  val internal = c.internal

  implicit def tupleToImplicitCandidate(t: (Type, Tree)): ImplicitCandidate = {
    ImplicitCandidate.tryUnapply(t) match {
      case Left(s) => c.abort(c.enclosingPosition, s)
      case Right((pre, sym, pt, tree)) => ImplicitCandidate(pre, sym, pt, tree)
    }
  }

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

  implicit class TypeOps(tpe: Type) {
    def typeParams = tpe match {
      case TypeRef(_, sym, _) => sym.asType.typeParams
      case _ => tpe.typeSymbol.asType.typeParams
    }

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

    def paramLists: List[List[Symbol]] = tpe.paramss map (_ map (x => x: Symbol))
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

  def appliedType(tc: Type, ts: List[Type]): Type = c.universe.appliedType(tc, ts)
  def appliedType(tc: Type, ts: Type*): Type = c.universe.appliedType(tc, ts.toList)

  def showCode(t: Tree): String = show(t)

  // forwarding methods for backwards compatability with 1.1.0

  type TypecheckMode = c.TypecheckMode
  val TERMmode = c.TERMmode
  val TYPEmode = c.TYPEmode
  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree =
    c.typecheck(tree, mode, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
  def untypecheck(tree: Tree): Tree = c.untypecheck(tree)

  def freshName() = c.freshName()
  def freshName(name: String) = c.freshName(name)
  def freshName[NameType <: Name](name: NameType) = c.freshName(name)

  def mkContextOps(c: Context): this.type = this
}
