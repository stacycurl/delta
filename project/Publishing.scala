import bintray.Plugin.bintrayPublishSettings
import scala.util.Properties

import bintray.Keys._
import sbt._
import Keys._

object Publishing extends Sonatype(DeltaBuild) {
  def projectUrl    = "https://github.com/stacycurl"
  def developerId   = "stacycurl"
  def developerName = "Stacy Curl"
}

abstract class Sonatype(build: Build) {
  import build._

  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"

  def projectUrl: String
  def developerId: String
  def developerName: String

  def scmUrl              = projectUrl
  def scmConnection       = "scm:git:" + scmUrl

  def generatePomExtra(scalaVersion: String): xml.NodeSeq = {
    <url>{ projectUrl }</url>
      <scm>
        <url>{ scmUrl }</url>
        <connection>{ scmConnection }</connection>
      </scm>
      <developers>
        <developer>
          <id>{ developerId }</id>
          <name>{ developerName }</name>
        </developer>
      </developers>
  }

  // travis ⇒ bintray, local ⇒ sonatype
  def settings: Seq[Setting[_]] = commonSettings ++ (Properties.envOrNone("BINTRAY_API_KEY") match {
    case Some(apiKey) ⇒ bintrayPublishSettings ++ Seq(
      repository in bintray := "repo",
      bintrayOrganization in bintray := None
    )
    case None ⇒ Seq(
      credentialsSetting,
      publishTo <<= version((v: String) ⇒ Some(if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
      pomIncludeRepository := (_ ⇒ false),
      pomExtra <<= scalaVersion(generatePomExtra)
    )
  })

  private def commonSettings: Seq[Setting[_]] = Seq(
    moduleName <<= name("delta-" + _),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

  lazy val credentialsSetting = credentials += {
    Seq("SONATYPE_USERNAME", "SONATYPE_PASSWORD").map(k ⇒ sys.env.get(k)) match {
      case Seq(Some(user), Some(pass)) ⇒
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _                           ⇒
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }
}