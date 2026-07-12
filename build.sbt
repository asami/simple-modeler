enablePlugins(JavaAppPackaging)

name := "simplemodeler"

organization := "org.simplemodeling"

version := "1.1.24-SNAPSHOT"

scalaVersion := "2.12.18"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

// resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2023/releases"

// deprecated
resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

// deprecated
resolvers += "GitHab releases 2025" at "https://raw.github.com/asami/maven-repository/2025/releases"

resolvers += "SimpleModeling.org" at "https://www.simplemodeling.org/repository/maven"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.3.30"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "2.2.5"

// libraryDependencies += "org.goldenport" %% "goldenport-sexpr" % "2.0.35"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.13.0"

libraryDependencies += "org.smartdox" %% "smartdox" % "2.4.16"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.10" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
publishTo := {
  val repo = sys.env.get("SIMPLEMODELING_MAVEN_LOCAL")
    .map(file)
    .getOrElse(baseDirectory.value / "maven-local")

  Some(
    Resolver.file(
      "local-simplemodeling-maven",
      repo
    )
  )
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty

def isSnapshotVersion(version: String): Boolean =
  version.endsWith("-SNAPSHOT")

def ensurePublishAllowed(version: String): Unit = {
  if (isSnapshotVersion(version))
    sys.error(s"Refusing to publish SNAPSHOT simple-modeler version $version. Use publishLocal for development versions.")
}

def ensurePublishLocalAllowed(version: String): Unit = {
  if (!isSnapshotVersion(version))
    sys.error(s"Refusing to publishLocal release simple-modeler version $version. Use publish for public release versions.")
}

def releaseDirtyFiles(base: File): Seq[String] = {
  import scala.sys.process._

  val output = new StringBuilder
  val logger = ProcessLogger(output append _ append "\n", output append _ append "\n")
  val exitCode = Process(Seq("git", "status", "--porcelain", "--", "build.sbt", "project", "src"), base).!(logger)
  if (exitCode != 0)
    sys.error(s"Unable to check git dirty state for release simple-modeler build:\n${output.toString.trim}")
  output.toString.split("\\r?\\n").iterator.map(_.trim).filter(_.nonEmpty).toSeq
}

def ensureReleaseTreeClean(version: String, base: File): Unit = {
  if (!isSnapshotVersion(version)) {
    val dirtyFiles = releaseDirtyFiles(base)
    if (dirtyFiles.nonEmpty) {
      val details = dirtyFiles.mkString("\n  ")
      sys.error(s"Refusing to compile/test release simple-modeler version $version with dirty source/build files:\n  $details\nUse a *-SNAPSHOT version for development changes.")
    }
  }
}

publish / skip := {
  ensurePublishAllowed(version.value)
  ensureReleaseTreeClean(version.value, baseDirectory.value)
  false
}

publishLocal / skip := {
  ensurePublishLocalAllowed(version.value)
  false
}

Compile / sources := {
  ensureReleaseTreeClean(version.value, baseDirectory.value)
  (Compile / sources).value
}

Test / definedTests := {
  ensureReleaseTreeClean(version.value, baseDirectory.value)
  (Test / definedTests).value
}

// Docker
maintainer in Docker := "Duke"

(Docker / dockerBaseImage).withRank(KeyRanks.Invisible) := "dockerfile/java"

// dockerExposedPorts in Docker := Seq(8080, 8080)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion,
      BuildInfoKey.action("build") {
        val fmt = new java.text.SimpleDateFormat("yyyyMMdd")
        fmt.setTimeZone(java.util.TimeZone.getTimeZone("JST"))
        fmt.format(new java.util.Date())
      }
    ),
    buildInfoPackage := "org.goldenport.kaleidox"
  )
