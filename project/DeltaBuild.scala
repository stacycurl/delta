import sbt._

import sbt.Keys._


object DeltaBuild extends Build {
  lazy val delta = (project in file(".")
    aggregate(core, cats, scalaz, generic, argonaut, matchers)
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := ())
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
    settings(libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3")
  )

  lazy val cats = (project configure dependantProject("cats")
    settings(libraryDependencies += "org.typelevel" %% "cats" % "0.4.1")
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
  )

  lazy val scalaz = (project configure dependantProject("scalaz")
    settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.2")
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
  )

  lazy val generic = (project configure dependantProject("generic")
    settings(libraryDependencies += "com.chuusai" %% "shapeless" % "2.1.0")
  )

  lazy val argonaut = (project configure dependantProject("argonaut")
    settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-M2")
  )

  lazy val matchers = (project configure dependantProject("matchers")
    dependsOn argonaut % "compile -> compile; test -> test"
    settings(libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4")
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  private def dependantProject(name: String)(project: Project) = (project in file(name)
    dependsOn core % "compile -> compile; test -> test"
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
  )

  private def commonSettings = Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.7",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
}
