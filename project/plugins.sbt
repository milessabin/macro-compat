scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

addSbtPlugin("com.github.gseitz"  % "sbt-release"           % "1.0.4")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"               % "1.0.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"               % "0.9.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"           % "0.6.22")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"       % "0.1.14")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"          % "1.1")
