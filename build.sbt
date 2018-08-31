import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import sbtcrossproject.{crossProject, CrossProject, CrossType}
import ReleaseTransformations._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys
import MimaKeys.{mimaPreviousArtifacts, mimaBinaryIssueFilters}
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

lazy val scala211 = "2.11.12"

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.10.7",
  crossScalaVersions := Seq("2.10.7", scala211, "2.12.6", "2.13.0-M5")
)

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),

  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation"
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/milessabin/macro-compat"),
      "scm:git:git@github.com:milessabin/macro-compat.git"
    ))
) ++ crossVersionSharedSources ++ scalaMacroDependencies

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings

def configureJUnit(crossProject: CrossProject) = {
  crossProject
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .jvmSettings(
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.11" % "test"
  )
}

lazy val root = project.in(file("."))
  .aggregate(coreJS, coreJVM, testJS, testJVM)
  .dependsOn(coreJS, coreJVM, testJS, testJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .settings(moduleName := "macro-compat")
  .settings(coreSettings:_*)
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .nativeSettings(
    scalaVersion := scala211,
    crossScalaVersions := Seq(scala211)
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val test = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "macro-compat-test")
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val testJVM = test.jvm
lazy val testJS = test.js
lazy val testNative = test.native

lazy val nativeTest = project.in(file("native-test"))
  .disablePlugins(sbt.plugins.BackgroundRunPlugin)
  .dependsOn(testNative)
  .enablePlugins(ScalaNativePlugin)
  .settings(
    scalaVersion := scala211,
    noPublishSettings
  )

addCommandAlias("validate", ";root;compile;mimaReportBinaryIssues;test")
addCommandAlias("release-all", ";root;release")
addCommandAlias("js", ";project coreJS")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("native", ";project coreNative")
addCommandAlias("root", ";project root")

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.13+ is used, quasiquotes and macro-annotations are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 13 => Seq()
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
        )
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) => Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
          "org.scalamacros" %% "quasiquotes" % "2.1.1" cross CrossVersion.binary
        )
    }
  }
)

def scalaPartV = Def setting (CrossVersion partialVersion scalaVersion.value)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc).value.map { dir =>
        scalaPartV.value match {
          case Some((2, y)) if y == 10 => new File(dir.getPath + "_2.10")
          case Some((2, y)) if y >= 11 => new File(dir.getPath + "_2.11+")
        }
      }
    }
  }

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("https://github.com/milessabin/macro-compat")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/macro-compat"), "scm:git:git@github.com:milessabin/macro-compat.git")),
  pomExtra := (
    <developers>
      <developer>
        <id>milessabin</id>
        <name>Miles Sabin</name>
        <url>http://milessabin.com/blog</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val mimaSettings = mimaDefaultSettings ++ Seq(
  mimaPreviousArtifacts := {
    if(scalaVersion.value == "2.12.6" || scalaVersion.value == "2.13.0-M5") Set()
    else Set(organization.value %% moduleName.value % "1.1.0")
  },

  mimaBinaryIssueFilters ++= {
    // Filtering the methods that were added since the checked version
    // (these only break forward compatibility, not the backward one)
    Seq(
      ProblemFilters.exclude[MissingMethodProblem]("macrocompat.BundleMacro.mkForwarder")
    )
  }
)

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
