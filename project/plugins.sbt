libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

resolvers ++= Seq("bintray-sbt-plugin-releases" at "http://dl.bintray.com/content/sbt/sbt-plugin-releases")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("http://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

// addSbtPlugin("org.scoverage"    %% "sbt-scoverage"         % "1.3.5")

addSbtPlugin("me.lessis"        %  "bintray-sbt"           % "0.1.2")

scalacOptions += "-deprecation"
