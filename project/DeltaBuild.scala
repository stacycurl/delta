import sbt._

import net.virtualvoid.sbt.graph.{Plugin ⇒ GraphPlugin}
import org.scalastyle.sbt.ScalastylePlugin.{ Settings => scalaStyleSettings }

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object DeltaBuild extends Build {
  lazy val delta = (project in file(".")
    aggregate core
    settings(commonSettings: _*)
    settings(publish := (), publishLocal := ())
  )

  lazy val core = (project in file("core")
    settings(commonSettings: _*)
    settings(Publishing.settings: _*)
    settings(libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.1.0"
//      "com.chuusai" % "shapeless_2.10.4" % "2.1.0",
//      compilerPlugin("org.scalamacros" % "paradise_2.10.4" % "2.0.1")
    ))
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) ⇒ classes.foreach(c ⇒ runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = GraphPlugin.graphSettings ++ scalaStyleSettings ++ Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.11.6",
    maxErrors := 1,
    parallelExecution in Test := false,
    scalacOptions := Seq(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"
    ),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq(
      "com.novocode"  % "junit-interface" % "0.11"  % "test",
//      "org.specs2"     %% "specs2"      % "3.3.1" % "test",
      "org.scalaz"     %% "scalaz-core" % "7.1.0" % "test"
//      "org.scalacheck" %% "scalacheck"                % "1.10.1" % "test",
      //"org.scalaz"     %% "scalaz-scalacheck-binding" % "7.1.0"  % "test",
//      "org.typelevel"  % "scalaz-specs2_2.11"             % "0.3.0"  % "test"
    ),
    initialCommands in console := """import sjc.delta._""",
    coverageMinimum := 100,
    coverageFailOnMinimum := true
  )
}