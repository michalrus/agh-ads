organization := "com.michalrus"

name := "zasd"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args", "-Yrangepos", "-Ywarn-dead-code", "-Ywarn-inaccessible",
  "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-numeric-widen",
  "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
)
