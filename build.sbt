ThisBuild / scalaVersion := "3.4.2"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.williamyaoh"
ThisBuild / organizationName := "com.williamyaoh"

lazy val tst = project
  .in(file("."))
  .settings(
    name := "tst",
    scalacOptions ++= Seq(
      "-release",
      "17",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-explaintypes",
      "-encoding",
      "UTF-8",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:privates",
      "-Wvalue-discard",
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
    ),
  )
