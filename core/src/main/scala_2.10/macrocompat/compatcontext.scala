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
import scala.language.postfixOps

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.internal.Flags._
import scala.reflect.macros.runtime.{ Context => RuntimeContext }
import scala.reflect.macros.{ Attachments, Context, TypecheckException }
import scala.reflect.{ ClassTag, classTag }

// Interface only. Implementation should be in RuntimeCompatContext
sealed trait CompatContext extends Context {
  val c: Context
  override lazy val universe: c.universe.type = c.universe

  import universe._

  def freshName(): String
  def freshName(name: String): String
  def freshName[NameType <: Name](name: NameType): NameType

  case class ImplicitCandidate211(pre: Type, sym: Symbol, pt: Type, tree: Tree)
  val ImplicitCandidate: ImplicitCandidateCompanion
  abstract class ImplicitCandidateCompanion {
    def apply(pre: Type, sym: Symbol, pt: Type, tree: Tree): Unit
    def unapply(t: (Type, Tree)): Option[(Type, Symbol, Type, Tree)]
  }

  type TypecheckMode
  val TERMmode: TypecheckMode
  val TYPEmode: TypecheckMode

  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree

  def untypecheck(tree: Tree): Tree

  val internal: Internal
  abstract class Internal {
    def constantType(c: Constant): ConstantType

    def polyType(tparams: List[Symbol], tpe: Type): Type

    def enclosingOwner: Symbol

    val gen: Gen
    abstract class Gen {
      def mkAttributedRef(sym: Symbol): Tree

      def mkAttributedRef(pre: Type, sym: Symbol): Tree
    }

    object decorators

    def thisType(sym: Symbol): Type

    def singleType(pre: Type, sym: Symbol): Type

    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type

    def setInfo(sym: Symbol, tpe: Type): Symbol

    def newTermSymbol(owner: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol

    def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree

    def typeBounds(lo: Type, hi: Type): TypeBounds
  }

  val compatUniverse: CompatUniverse
  abstract class CompatUniverse {
    val internal: Internal

    val TypeName: TypeNameCompanion
    abstract class TypeNameCompanion {
      def apply(s: String): TypeName
      def unapply(name: TypeName): Option[String]
    }

    val TermName: TermNameCompanion
    abstract class TermNameCompanion {
      def apply(s: String): TermName
      def unapply(name: TermName): Option[String]
    }

    def symbolOf[T: WeakTypeTag]: TypeSymbol

    val termNames: TermNamesApi
    val typeNames: TypeNamesApi

    implicit def TypeOps(tpe: Type): TypeOps
    abstract class TypeOps {
      def typeParams: List[Symbol]
      def typeArgs: List[Type]
      def companion: Type
      def decl(nme: Name): Symbol
      def decls: MemberScope
      def dealias: Type
      def finalResultType: Type
      def paramLists: List[List[Symbol]]
    }

    implicit def MethodSymbolOps(sym: MethodSymbol): MethodSymbolOps
    abstract class MethodSymbolOps {
      def paramLists: List[List[Symbol]]
    }

    implicit def SymbolOps(sym: Symbol): SymbolOps
    abstract class SymbolOps {
      def companion: Symbol
      def info: Type
      def infoIn(site: Type): Type
      def isConstructor: Boolean
      def isAbstract: Boolean
      def overrides: List[Symbol]

      def isPrivateThis: Boolean
      def isProtectedThis: Boolean
    }

    implicit def TreeOps(tree: Tree): TreeOps
    abstract class TreeOps {
      def nonEmpty: Boolean
    }

    val CompatAnnotation: AnnotationCompanion
    abstract class AnnotationCompanion {
      def apply(tree: Tree): Annotation

      def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation
      def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
    }

    implicit def AnnotationOps(ann: Annotation): AnnotationOps
    abstract class AnnotationOps {
      def tree: Tree
    }

    def showCode(t: Tree): String

    val CompatModifiers: CompatModifiers
    abstract class CompatModifiers extends ModifiersCreator {
      def apply(flags: FlagSet, privateWithin: Name = typeNames.EMPTY, annots: List[Tree] = Nil): Modifiers
      def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])]
    }

    implicit def tupleToImplicitCandidate(t: (Type, Tree)): ImplicitCandidate211

    implicit def AttachmentsOps(as: Attachments): AttachmentsOps
    abstract class AttachmentsOps {
      def contains[T: ClassTag]: Boolean
      def isEmpty: Boolean
    }

    def noSelfType: ValDef
  }
}

class RuntimeCompatContext(val c: RuntimeContext) extends RuntimeContext with CompatContext { outer =>
  override lazy val universe: c.universe.type = c.universe

  import universe._

  lazy val callsiteTyper = c.callsiteTyper.asInstanceOf[analyzer.Typer]
  lazy val prefix: Expr[PrefixType] = c.prefix.asInstanceOf[Expr[PrefixType]]
  lazy val expandee: Tree = c.expandee.asInstanceOf[Tree]

  def freshName() = c.fresh
  def freshName(name: String) = c.fresh(name)
  def freshName[NameType <: Name](name: NameType) = c.fresh(name)


  object ImplicitCandidate extends ImplicitCandidateCompanion {
    def apply(pre: Type, sym: Symbol, pt: Type, tree: Tree) = ImplicitCandidate211(pre, sym, pt, tree)
    def unapply(t: (Type, Tree)): Option[(Type, Symbol, Type, Tree)] = tryUnapply(t).right.toOption

    def tryUnapply(t: (Type, Tree)): Either[String, (Type, Symbol, Type, Tree)] = {
      val (pt, tree) = t
      callsiteTyper.context.openImplicits.filter(oi => oi.pt == pt && oi.tree == tree) match {
        case List(oi) => Right((oi.info.pre, oi.info.sym, oi.pt, oi.tree))
        case Nil => Left(s"Failed to identify ImplicitCandidate for $t, none match")
        case xs => Left(s"Failed to identify ImplicitCandidate for $t, ${xs.size} match")
      }
    }
  }

  type TypecheckMode = Int
  val TERMmode = analyzer.EXPRmode
  val TYPEmode = analyzer.HKmode

  def typecheck(tree: Tree, mode: TypecheckMode = TERMmode, pt: Type = WildcardType, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
    val context = callsiteTyper.context
    val withImplicitFlag = if (!withImplicitViewsDisabled) (context.withImplicitsEnabled[Tree] _) else (context.withImplicitsDisabled[Tree] _)
    val withMacroFlag = if (!withMacrosDisabled) (context.withMacrosEnabled[Tree] _) else (context.withMacrosDisabled[Tree] _)
    def withContext(tree: => Tree) = withImplicitFlag(withMacroFlag(tree))
    def withWrapping(tree: Tree)(op: Tree => Tree) = if (mode == TERMmode) wrappingIntoTerm(tree)(op) else op(tree)
    def typecheckInternal(tree: Tree): analyzer.SilentResult[Tree] =
      callsiteTyper.silent(_.typed(duplicateAndKeepPositions(tree), mode, pt), reportAmbiguousErrors = false)
    withWrapping(tree)(wrappedTree => withContext(typecheckInternal(wrappedTree) match {
      case analyzer.SilentResultValue(result) =>
        result
      case error @ analyzer.SilentTypeError(_) =>
        if (!silent) throw new TypecheckException(error.err.errPos, error.err.errMsg)
        EmptyTree
    }))
  }

  def untypecheck(tree: Tree): Tree = resetLocalAttrs(tree)

  object internal extends Internal {
    def constantType(c: Constant): ConstantType = ConstantType(c)

    def polyType(tparams: List[Symbol], tpe: Type): Type = universe.genPolyType(tparams, tpe)

    def enclosingOwner: Symbol = callsiteTyper.context.owner

    object gen extends Gen {
      def mkAttributedRef(sym: Symbol): Tree = universe.gen.mkAttributedRef(sym)

      def mkAttributedRef(pre: Type, sym: Symbol): Tree = universe.gen.mkAttributedRef(pre, sym)
    }

    def thisType(sym: Symbol): Type = ThisType(sym)

    def singleType(pre: Type, sym: Symbol): Type = SingleType(pre, sym)

    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = universe.typeRef(pre, sym, args)

    def setInfo(sym: Symbol, tpe: Type): Symbol = sym.setTypeSignature(tpe)

    def newTermSymbol(owner: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol =
      owner.newTermSymbol(name, pos, flags)

    def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree =
      tree.substituteSymbols(from, to)

    def typeBounds(lo: Type, hi: Type): TypeBounds = TypeBounds(lo, hi)
  }

  object compatUniverse extends CompatUniverse {
    val internal = outer.internal

    object TypeName extends TypeNameCompanion {
      def apply(s: String) = newTypeName(s)
      def unapply(name: TypeName): Option[String] = Some(name.toString)
    }

    object TermName extends TermNameCompanion {
      def apply(s: String) = newTermName(s)
      def unapply(name: TermName): Option[String] = Some(name.toString)
    }

    def symbolOf[T: WeakTypeTag]: TypeSymbol =
      weakTypeOf[T].typeSymbolDirect.asType

    lazy val termNames = nme
    lazy val typeNames = tpnme

    implicit def TypeOps(tpe: Type): TypeOps =
      new TypeOps {
        def typeParams = tpe.typeParams

        def typeArgs: List[Type] = {
          import scala.language.reflectiveCalls
          def loop(tpe: Type): List[Type] = tpe match {
            case TypeRef(_, _, args) => args
            case et @ ExistentialType(_, underlying) =>
              loop(underlying).map(arg => et.asInstanceOf[{ def maybeRewrap(tpe: Type): Type }].maybeRewrap(arg))
            case _ => Nil
          }
          loop(tpe)
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

        def dealias: Type = {
          @tailrec
          def loop(tpe: Type): Type = tpe match {
            case tr @ TypeRef(pre, sym, args) if sym.isAliasType =>
              if(tr.typeParamsMatchArgs) loop(tr.betaReduce) else tpe
            case _ => tpe
          }
          loop(tpe)
        }

        def finalResultType: Type = {
          @tailrec
          def loop(tp: Type): Type = tp match {
            case PolyType(_, restpe)       => loop(restpe)
            case MethodType(_, restpe)     => loop(restpe)
            case NullaryMethodType(restpe) => loop(restpe)
            case _                         => tp
          }
          loop(tpe)
        }

        def paramLists: List[List[Symbol]] = tpe.paramss map (_ map (x => x: Symbol))
      }

    implicit def MethodSymbolOps(sym: MethodSymbol): MethodSymbolOps =
      new MethodSymbolOps {
        def paramLists = sym.paramss
      }

    implicit def SymbolOps(sym: Symbol): SymbolOps =
      new SymbolOps {
        def companion: Symbol = {
          if (sym.isModule && !sym.hasPackageFlag) sym.companionSymbol
          else if (sym.isModuleClass && !sym.isPackageClass) sym.sourceModule.companionSymbol
          else if (sym.isClass && !sym.isModuleClass && !sym.isPackageClass) sym.companionSymbol
          else NoSymbol
        }

        def info: Type = sym.typeSignature
        def infoIn(site: Type): Type = sym.typeSignatureIn(site)

        def isConstructor: Boolean = sym.isMethod &&sym.asMethod.isConstructor

        def isAbstract: Boolean = sym.isAbstractClass || sym.isDeferred || sym.isAbstractType

        def overrides: List[Symbol] = sym.allOverriddenSymbols

        def isPrivateThis: Boolean = (sym hasFlag PRIVATE) && (sym hasFlag LOCAL)

        def isProtectedThis: Boolean = (sym hasFlag PROTECTED) && (sym hasFlag LOCAL)
      }

    implicit def TreeOps(tree: Tree): TreeOps =
      new TreeOps {
        def nonEmpty = !tree.isEmpty
      }

    object CompatAnnotation extends AnnotationCompanion {
      import definitions._

      def apply(tree: Tree): Annotation = treeToAnnotation(tree)

      def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation =
        universe.Annotation(tpe, scalaArgs, javaArgs)

      def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])] =
        universe.Annotation.unapply(ann)

      // cut-n-pasted (with the comments) from
      // https://github.com/scala/scala/blob/v2.11.7/src/reflect/scala/reflect/internal/AnnotationInfos.scala#L348-L382
      def annotationToTree(ann: Annotation): Tree = {
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

      // cut-n-pasted (with the comments) from
      // https://github.com/scala/scala/blob/v2.11.7/src/reflect/scala/reflect/internal/AnnotationInfos.scala#L384-L403
      def treeToAnnotation(tree: Tree): Annotation = tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          def encodeJavaArg(arg: Tree): ClassfileAnnotArg = arg match {
            case Literal(const) => LiteralAnnotArg(const)
            case Apply(ArrayModule, args) => ArrayAnnotArg(args map encodeJavaArg toArray)
            case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) => NestedAnnotArg(treeToAnnotation(arg))
            case _ => throw new Exception(s"unexpected java argument shape $arg: literals, arrays and nested annotations are supported")
          }
          def encodeJavaArgs(args: List[Tree]): List[(Name, ClassfileAnnotArg)] = args match {
            case AssignOrNamedArg(Ident(name), arg) :: rest => (name, encodeJavaArg(arg)) :: encodeJavaArgs(rest)
            case arg :: rest => throw new Exception(s"unexpected java argument shape $arg: only AssignOrNamedArg trees are supported")
            case Nil => Nil
          }
          val atp = tpt.tpe
          if (atp != null && (atp.typeSymbol isNonBottomSubClass StaticAnnotationClass)) AnnotationInfo(atp, args, Nil)
          else if (atp != null && (atp.typeSymbol isNonBottomSubClass ClassfileAnnotationClass)) AnnotationInfo(atp, Nil, encodeJavaArgs(args))
          else throw new Exception(s"unexpected annotation type $atp: only subclasses of StaticAnnotation and ClassfileAnnotation are supported")
        case _ =>
          throw new Exception("""unexpected tree shape: only q"new $annType(..$args)" is supported""")
      }
    }

    implicit def AnnotationOps(ann: Annotation): AnnotationOps =
      new AnnotationOps {
        def tree: Tree = CompatAnnotation.annotationToTree(ann)
      }

    def showCode(t: Tree): String = show(t)

    object CompatModifiers extends CompatModifiers {
      def apply(flags: FlagSet, privateWithin: Name = typeNames.EMPTY, annots: List[Tree] = Nil): Modifiers =
        Modifiers(flags, privateWithin, annots)

      def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])] =
        Some((mods.flags, mods.privateWithin, mods.annotations))
    }

    implicit def tupleToImplicitCandidate(t: (Type, Tree)): ImplicitCandidate211 = {
      ImplicitCandidate.tryUnapply(t) match {
        case Left(s) => c.abort(c.enclosingPosition, s)
        case Right((pre, sym, pt, tree)) => ImplicitCandidate211(pre, sym, pt, tree)
      }
    }

    implicit def AttachmentsOps(as: Attachments): AttachmentsOps =
      new AttachmentsOps {
        private def matchesTag[T: ClassTag](datum: Any) =
          classTag[T].runtimeClass.isInstance(datum)

        def contains[T: ClassTag]: Boolean =
          !isEmpty && (as.all exists matchesTag[T])

        def isEmpty: Boolean = true
      }

    lazy val noSelfType = emptyValDef
  }
}
