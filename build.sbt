name := "ScalaTorrent"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq (
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "com.typesafe.akka" %% "akka-actor" % "2.4.17",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "commons-io" % "commons-io" % "2.5"
)
