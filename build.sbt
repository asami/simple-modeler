enablePlugins(JavaAppPackaging)

name := "simplemodeler"

organization := "org.simplemodeling"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.44-SNAPSHOT"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.3.8-SNAPSHOT"

libraryDependencies += "org.goldenport" %% "goldenport-sexpr" % "2.0.2-SNAPSHOT"

libraryDependencies += "org.smartdox" %% "smartdox" % "1.2.6-SNAPSHOT"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))

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
