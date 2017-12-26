scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

addSbtPlugin("com.github.gseitz"  % "sbt-release"              % "1.0.8")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"                  % "1.1.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "0.6.23")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"          % "0.3.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "2.3")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.4.0")
