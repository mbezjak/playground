name := "sample"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "org.codehaus.jackson" % "jackson-core-asl" % "1.9.9",
    "org.specs2" %% "specs2" % "1.12.3" % "test"
)

resolvers += "releases" at "http://oss.sonatype.org/content/repositories/releases"

TaskKey[Unit]("hello") := println("hello world")
