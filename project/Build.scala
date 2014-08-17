import sbt._

import com.typesafe.sbt.SbtScalariform.scalariformSettings
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin.{ Settings => scalaStyleSettings }

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object DeltaBuild extends Build {
  lazy val delta = Project(
    id = "delta-parent",
    base = file("."),
    settings = commonSettings,
    aggregate = Seq(deltaCore, deltaExamples)
  )

  lazy val deltaCore = Project(
    id = "delta-core",
    base = file("core"),
    settings = commonSettings ++ Publishing.settings ++ Seq(
      libraryDependencies ++= Seq(
        "com.chuusai" % "shapeless_2.10.2"   % "2.0.0",
        "org.scalaz"  %% "scalaz-core" % "7.1.0"
      )
    )
  )

  lazy val deltaExamples = Project(
    id = "delta-examples",
    base = file("examples"),
    dependencies = Seq(deltaCore),
    settings = commonSettings ++ Seq(
      runAllIn(Compile)
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = graphSettings ++
  // uncomment when you want to reset the formatting of the project
  // scalariformSettings ++
  scalaStyleSettings ++ instrumentSettings ++ Seq(
    organization := "com.github.stacycurl",
    scalaVersion := "2.10.2",
    maxErrors := 1,
    parallelExecution in Test := true,
    scalacOptions := Seq(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.specs2"      % "specs2_2.10"               % "2.4" % "test",
      //"org.scalacheck" %% "scalacheck"                % "1.10.1" % "test",
      //"org.scalaz"     %% "scalaz-scalacheck-binding" % "7.1.0"  % "test",
      "org.typelevel"  % "scalaz-specs2_2.10"             % "0.3.0"  % "test"
    ),
    initialCommands in console := """import sjc.delta._""",
    minimumCoverage := 100,
    highlighting := true,
    failOnMinimumCoverage := true
  )
}
