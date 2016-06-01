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
    settings(libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6")
  )

  lazy val cats = (project configure dependantProject("cats")
    settings(libraryDependencies += "org.typelevel" %% "cats" % "0.8.1")
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  )

  lazy val scalaz = (project configure dependantProject("scalaz")
    settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M6")
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  )

  lazy val generic = (project configure dependantProject("generic")
    dependsOn matchers % "test -> test"
    settings(libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2")
  )

  lazy val argonaut = (project configure dependantProject("argonaut")
    settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-RC1")
  )

  lazy val matchers = (project configure dependantProject("matchers")
    dependsOn argonaut % "compile -> compile; test -> test"
    settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1")
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
    scalaVersion := "2.12.0",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    initialize := {
      val _ = initialize.value
      require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
    }
  )
}
