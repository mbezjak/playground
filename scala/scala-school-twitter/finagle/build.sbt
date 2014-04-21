name := "intro-to-finagle"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "com.twitter" % "finagle-core" % "6.0.1",
    "com.twitter" % "finagle-http" % "6.0.1"
)

resolvers += "releases" at "http://maven.twttr.com"
