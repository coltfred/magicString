name := "thin-string"

version := "0.1"

organization := "com.ironcorelabs"

scalaVersion := "2.11.8"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
  )
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-language:higherKinds"
  ) 
 
com.typesafe.sbt.SbtScalariform.scalariformSettings
