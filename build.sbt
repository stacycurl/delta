import Delta._
import sbt.Keys._
import sbt._


lazy val delta: Project = (project in file(".")
  aggregate(core, cats, scalaz, generic, argonaut, matchers)
  settings(commonSettings: _*)
  settings(publish / skip := true)
)

lazy val core: Project = (project in file("core")
  settings(commonSettings: _*)
  settings(
    name                := "delta-core",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
  )
)

lazy val cats: Project = (project configure dependantProject(core, "cats")
  settings(libraryDependencies += "org.typelevel" %% "cats" % "0.8.1")
  settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)

lazy val scalaz: Project = (project configure dependantProject(core, "scalaz")
  settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.3")
  settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)

lazy val generic: Project = (project configure dependantProject(core, "generic")
  dependsOn matchers % "test -> test"
  settings(libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2")
)

lazy val argonaut: Project = (project configure dependantProject(core, "argonaut")
  settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.3.3")
)

lazy val matchers: Project = (project configure dependantProject(core, "matchers")
  dependsOn argonaut % "test -> test"
  settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1")
)

//lazy val runAll = TaskKey[Unit]("run-all")
//
//def runAllIn(config: Configuration) = {
//  runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
//    (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
//  }
//}