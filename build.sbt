name := "mustache"

version := "1.1"

scalaVersion := "2.10.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe Snapshots Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.1" % "test->default",
  "org.specs2" %% "specs2" % "2.1" % "test->default",
  "com.typesafe.akka" %% "akka-actor" % "2.3-SNAPSHOT" % "test->default"
)
