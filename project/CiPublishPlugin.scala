import com.jsuereth.sbtpgp.SbtPgp
import com.typesafe.sbt.GitPlugin
import sbt.Keys._
import sbt.plugins.JvmPlugin
import sbt.{Def, _}
import xerial.sbt.Sonatype
import xerial.sbt.Sonatype.autoImport._

import scala.sys.process._
import scala.util.control.NonFatal


object CiPublishPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins = JvmPlugin && SbtPgp && GitPlugin && Sonatype

  def isSecure: Boolean =
    System.getenv("PGP_SECRET") != null
        
  def setupGpg(): Unit = {
    val versionLine = List("gpg", "--version").!!.linesIterator.toList.head
    
    println(versionLine)
    
    val TaggedVersion = """(\d{1,14})([\.\d{1,14}]*)((?:-\w+)*)""".r
    
    val gpgVersion: Long = versionLine.split(" ").last match {
      case TaggedVersion(m, _, _) => m.toLong
      case _                         => 0L
    }
    
    // https://dev.gnupg.org/T2313
    val importCommand = if (gpgVersion < 2L) "--import" else "--batch --import"
      
    val secret = sys.env("PGP_SECRET")
    
    (s"echo $secret" #| "base64 --decode" #| s"gpg $importCommand").!
  }

  private def gitHubScmInfo(user: String, repo: String) =
    ScmInfo(
      url(s"https://github.com/$user/$repo"),
      s"scm:git:https://github.com/$user/$repo.git",
      Some(s"scm:git:git@github.com:$user/$repo.git")
    )

  override lazy val buildSettings: Seq[Def.Setting[_]] = List(
    scmInfo ~= {
      case Some(info) => Some(info)
      case None =>
        import scala.sys.process._
        val identifier = """([^\/]+?)"""
        val GitHubHttps = s"https://github.com/$identifier/$identifier(?:\\.git)?".r
        val GitHubGit   = s"git://github.com:$identifier/$identifier(?:\\.git)?".r
        val GitHubSsh   = s"git@github.com:$identifier/$identifier(?:\\.git)?".r
        try {
          val remote = List("git", "ls-remote", "--get-url", "origin").!!.trim()
          remote match {
            case GitHubHttps(user, repo) => Some(gitHubScmInfo(user, repo))
            case GitHubGit(user, repo)   => Some(gitHubScmInfo(user, repo))
            case GitHubSsh(user, repo)   => Some(gitHubScmInfo(user, repo))
            case _                       => None
          }
        } catch {
          case NonFatal(_) => None
        }
    }
  )

  override lazy val globalSettings: Seq[Def.Setting[_]] = List(
    publishArtifact.in(Test) := false,
    publishMavenStyle := true,
    commands += Command.command("ci-publish") { currentState =>
      if (!isSecure) {
        println("No access to secret variables, skipping publish")
        currentState
      } else {
        println("Running ci-publish")
        setupGpg()
        // https://github.com/olafurpg/sbt-ci-release/issues/64
        
        "set pgpSecretRing := pgpSecretRing.value" ::
        "set pgpPublicRing := pgpPublicRing.value" ::
        "+publishSigned" ::
        "sonatypeBundleRelease" ::
        currentState
      }
    }
  )
  
  override lazy val projectSettings: Seq[Def.Setting[_]] = List(
    publishConfiguration      := publishConfiguration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    publishTo                 := sonatypePublishToBundle.value
  )
}