import com.jsuereth.sbtpgp.SbtPgp.autoImport.usePgpKeyHex
import sbt.Keys._
import sbt.{Def, _}


object Delta {
  def dependantProject(core: Project, name: String)(project: Project): Project = (project in file(name)
    dependsOn core % "compile -> compile; test -> test"
    settings(commonSettings: _*)
  )

  lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
    homepage     := Some(url("https://github.com/stacycurl/delta")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer("stacycurl", "Stacy Curl", "stacy.curl@gmail.com", url("https://github.com/stacycurl"))
    ),
    usePgpKeyHex("pimpathon ci"),
    organization             := "com.github.stacycurl",
    scalaVersion             := "2.12.12",
    maxErrors                := 1,
    Test / parallelExecution := true,
    scalacOptions            := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),
    libraryDependencies      += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    initialize := {
      val _ = initialize.value
      require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
    }
  )  
}