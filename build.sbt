scalaVersion in ThisBuild := "2.12.1"
crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1")  

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)

val minihaskell = crossProject.in(file(".")).settings(
    name := "minihaskell",
    normalizedName := "minihaskell",
    version := "0.0.1-SNAPSHOT",
    unmanagedSourceDirectories in Compile +=
        baseDirectory.value  / "shared" / "main" / "scala",
    libraryDependencies ++= Seq(
        "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test",
        "org.scalatest" %% "scalatest" % "3.0.1" % "test",
        "org.typelevel" %%% "cats" % "0.8.1",
        "com.lihaoyi" %%% "fastparse" % "0.4.2"
    ),
    organization := "eu.tilk",
    licenses += ("LGPL 3.0", url("https://opensource.org/licenses/LGPL-3.0")),
    scmInfo := Some(ScmInfo(
        url("https://github.com/tilk/minihaskell"),
        "scm:git:git@github.com:tilk/minihaskell.git",
        Some("scm:git:git@github.com:tilk/minihaskell.git"))),
    publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
            Some("snapshots" at nexus + "content/repositories/snapshots")
        else
            Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    pomExtra := (
        <developers>
          <developer>
            <id>tilk</id>
            <name>Marek Materzok</name>
            <url>https://github.com/tilk/</url>
          </developer>
        </developers>
    )
).jsSettings(
    name := "minihaskell JS"
).jvmSettings(
    name := "minihaskell JVM"
)

lazy val minihaskellJS = minihaskell.js
lazy val minihaskellJVM = minihaskell.jvm

