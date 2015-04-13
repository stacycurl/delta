import bintray.Plugin.bintrayPublishSettings
import scala.util.Properties

import bintray.Keys._
import sbt._
import Keys._


object Publishing {
  def settings: Seq[Setting[_]] = commonSettings ++ (Properties.envOrNone("BINTRAY_API_KEY") match {
    case Some(apiKey) ⇒ bintrayPublishSettings ++ Seq(
      repository in bintray := "repo",
      bintrayOrganization in bintray := None
    )
    case None ⇒ Seq(
      credentialsSetting,
      publishTo <<= version(repo),
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

  private lazy val credentialsSetting = credentials += {
    Seq("SONATYPE_USERNAME", "SONATYPE_PASSWORD").map(k ⇒ sys.env.get(k)) match {
      case Seq(Some(u), Some(p)) ⇒ Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", u, p)
      case _                     ⇒ Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }

  private def repo(version: String) = Some(if (version.trim endsWith "SNAPSHOT")
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/" else
    "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
  )

  private def generatePomExtra(scalaVersion: String): xml.NodeSeq = {
    <url>{ projectUrl }</url>
    <licenses>
      <license>
        <name>Apache License</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>{ projectUrl }</url>
      <connection>{ "scm:git:" + projectUrl }</connection>
    </scm>
    <developers>
      <developer>
        <id>stacycurl</id>
        <name>Stacy Curl</name>
      </developer>
    </developers>
  }

  private val projectUrl   = "https://github.com/stacycurl"
}