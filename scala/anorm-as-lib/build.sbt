name := "anorm-as-lib"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
    "play" %% "anorm" % "2.0.4"
    // + jdbc
)

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/releases"
