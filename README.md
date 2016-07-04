# macro-compat: cross version Scala macro support

**macro-compat** is a small library which, in conjunction with the [macro-paradise][macro-paradise] compiler plugin,
allows you to compile macros with Scala 2.10.x which are written to the Scala 2.11/2 macro API. This means that your
macros can be written just once, for the current API, and still be portable to earlier Scala releases.

[![Build Status](https://api.travis-ci.org/milessabin/macro-compat.png?branch=master)](https://travis-ci.org/milessabin/macro-compat)
[![Stories in Ready](https://badge.waffle.io/milessabin/macro-compat.png?label=Ready)](https://waffle.io/milessabin/macro-compat)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/macro-compat)
[![Maven Central](https://img.shields.io/maven-central/v/org.typelevel/macro-compat_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/macro-compat_2.11)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.8.svg)](https://www.scala-js.org)

## Why you should use macro-compat

Scala macros are hard enough to write once ... writing them twice, once for each of the two API versions is even more
of a chore.

Currently people adopt one of the following approaches if they want portable macros,

+ They maintain separate git branches for Scala 2.11.x vs. 2.10.x variants.
+ They use SBT's [cross-version support for Scala sources][sbt-cross].
+ They write an abstraction layer over the macro API to hide the differences.

None of these is entirely satisfactory.

The branching model has worked fairly well for [shapeless][shapeless] but has become more cumbersome with the arrival
of Scala.JS ... a single branch build has become increasingly desirable.

Using SBT's cross version source support is effectively maintaining an "internal branch" within a single real branch
of your project, but without any of the tools which support managing branches effectively. Whilst this might be
adequate for very small amounts of macro code it gets increasingly awkward and error prone as the amount of macro code
grows.

Writing an abstraction layer that hides the differences between the macro API versions (insofar as it does so by
bringing Scala 2.10.x up to the 2.11.x API) is a _part_ of the solution proposed here. However it isn't enough. Two of
the biggest improvements of the Scala 2.11.x macro API over 2.10.x were the introduction of
[macro-bundles][macro-bundles] and the ability to type macro implementation method arguments and results as `Tree`
rather than `Expr[T]`. Both of these allow the signatures of macro implementation methods to be written significantly
more succintly and readably, and it would be a shame to have to give up on them just to remain compatible with Scala
2.10.x within a single branch.

**macro-compat** provides a backport of (parts of) the Scala 2.11.x macro API to 2.10.x and also provides an
annotation macro which provides support for macro bundles in 2.10.x and `Tree` as the type of macro implementation
method arguments and results. The intention is that you write macro code as macro bundles, exactly as you would for
Scala 2.11.x with the exception of a single `@bundle` annotation on the macro bundle class,

```scala
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import macrocompat.bundle

object Test {
  def foo: Int = macro TestMacro.fooImpl
  def bar(i: Int): String = macro TestMacro.barImpl
  def baz(is: Int*): Int = macro TestMacro.bazImpl
}

@bundle // macro-compat addition
class TestMacro(val c: whitebox.Context) {
  import c.universe._

  def fooImpl: Tree = q""" 23 """

  def barImpl(i: Tree): Tree = q""" "bar" """

  def bazImpl(is: Tree*): Tree = q""" 13 """
}
```

This code compiles on Scala 2.10.x, 2.11.x and 2.12.x.

The `@bundle` annotation is implemented as a macro annotation via the [macro-paradise][macro-paradise] compiler
plugin. On Scala 2.11.x and 2.12.x the annotation is simply eliminated during compilation, leaving no trace in the
resulting binaries. On Scala 2.10.x the annotation macro transforms the macro bundle class to an object definition
which is compatible with the 2.10.x macro API.

## Current status

This is a young project, initially extracted out of the [export-hook][export-hook] project and massaged into a more or
less usable form in free moments snatched during ICFP 2015. Since then a number of generous contributors have made
additions to the backport component and it is now seeing use in several other projects. As of version 1.1.1 backport
coverage has been expanded sufficiently to cover all the macro API usage in shapeless. I would be delighted for more
projects to pick it up and extend it to cover their needs as well.

If you would like to see or contribute particuluar extensions to the backport, please create issues here or hop on the
[gitter channel][macrocompat-gitter]. Discussion is also welcome on the [shapeless][shapeless-gitter] and
[cats][cats-gitter] gitter channels ... please let us know what you think.

## Using macro-compat

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master branch are built using [Travis CI][ci] and automatically published to the Sonatype
OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
```

Builds are available for Scala 2.10.x, 2.11.x and 2.12.x for Scala JDK and Scala.js.

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "macro-compat" % "1.1.1",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)
```

## Binary compatibility

As of version 1.0.7 macro-compat uses [MiMa][mima] to verify binary compatibility within minor versions. Binary
compatibility was broken in 1.0.3 and again in 1.0.6. In version 1.0.7 binary compatability with 1.0.3-5 has been
restored and 1.0.6 is now deprecated. The binary compatibility breaking changes were moved to 1.1.0 and hopefully
the addition of MiMa to the build will make a recurrence of this sort of breakage much less likely in future.

## Building macro-compat

macro-compat is built with SBT 0.13.11 or later.

## Participation

The macro-compat project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (Gitter, github, etc.) to be welcoming environments for everyone.

## Projects using macro-compat

+ [catalysts][catalysts]
+ [export-hook][export-hook]
+ [expressier][expressier]
+ [latr][latr]
+ [marley][marley]
+ [Monocle][monocle]
+ [refined][refined]
+ [shapeless][shapeless]
+ [simulacrum][simulacrum]
+ [tfm][tfm]

## Contributors

+ Adelbert Chang <adelbertc@gmail.com> [@adelbertchang](https://twitter.com/adelbertchang)
+ Alexandre Archambault <alexandre.archambault@gmail.com> [@alxarchambault](https://twitter.com/alxarchambault)
+ Alistair Johnson <alistair.johnson@johnsonusm.com> [@AlistairUSM](https://twitter.com/AlistairUSM)
+ Chris Hodapp <clhodapp1@gmail.com> [@clhodapp](https://twitter.com/clhodapp)
+ Dale Wijnand <dale.wijnand@gmail.com> [@dwijnand](https://twitter.com/dwijnand)
+ Frank S. Thomas <frank@timepit.eu> [@fst9000](https://twitter.com/fst9000)
+ Michael Pilquist <mpilquist@gmail.com> [@mpilquist](https://twitter.com/mpilquist)
+ Miles Sabin <miles@milessabin.com> [@milessabin](https://twitter.com/milessabin)
+ Naoki Aoyama <aoiro.aoino@gmail.com> [@AoiroAoino](https://twitter.com/AoiroAoino)
+ Philip Wills <otherphil@gmail.com> [@philwills](https://twitter.com/philwills)
+ Travis Brown <tbrown@twitter.com> [@travisbrown](https://twitter.com/travisbrown)
+ Your name here :-)

[macro-paradise]: http://docs.scala-lang.org/overviews/macros/paradise.html
[sbt-cross]: http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources
[shapeless]: https://github.com/milessabin/shapeless
[macro-bundles]: http://docs.scala-lang.org/overviews/macros/bundles.html
[export-hook]: https://github.com/milessabin/export-hook
[expressier]: https://github.com/travisbrown/expressier
[simulacrum]: https://github.com/mpilquist/simulacrum
[tfm]: https://github.com/adelbertc/tfm
[marley]: https://github.com/guardian/marley
[monocle]: https://github.com/julien-truffaut/Monocle
[shapeless-gitter]: https://gitter.im/milessabin/shapeless
[cats-gitter]: https://gitter.im/non/cats
[macrocompat-gitter]: https://gitter.im/milessabin/macro-compat
[typelevel]: http://typelevel.org/
[codeofconduct]: http://typelevel.org/conduct.html
[catalysts]: https://github.com/typelevel/catalysts
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~macro-compat
[ci]: https://travis-ci.org/milessabin/macro-compat
[mima]: https://github.com/typesafehub/migration-manager
[refined]: https://github.com/fthomas/refined
[latr]: https://github.com/runarorama/latr
