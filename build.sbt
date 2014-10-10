organization := "com.michalrus"

name := "zasd"

scalaVersion := "2.11.1"

scalacOptions in Compile ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args", "-Yrangepos", "-Ywarn-dead-code", "-Ywarn-inaccessible",
  "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-numeric-widen",
  "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard")
