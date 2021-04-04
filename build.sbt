enablePlugins(JavaAppPackaging)

name := "simplemodeler"

organization := "org.simplemodeling"

version := "1.0.0"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.73"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.3.29"

libraryDependencies += "org.goldenport" %% "goldenport-sexpr" % "2.0.12"

libraryDependencies += "org.smartdox" %% "smartdox" % "1.2.9"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}

// Docker
maintainer in Docker := "Duke"

dockerBaseImage in Docker := "dockerfile/java"

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
