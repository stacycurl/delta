import sbt._

import sbt.Keys._


object DeltaBuild extends Build {
  lazy val delta = (project in file(".")
    aggregate(core, generic)
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := ())
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
  )

  lazy val generic = (project in file("generic")
    dependsOn core % "compile -> compile; test -> test"
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
    settings(libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.1.0"
    ))
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.7",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"       % "1.0.3",
      "com.novocode"           %  "junit-interface" % "0.11"  % "test",
      "org.scalaz"             %% "scalaz-core"     % "7.1.0" % "test"
    )
  )
}
