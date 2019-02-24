
ThisBuild / organization := "org.tdwarf"
ThisBuild / scalaVersion := "2.12.8"
// set the Scala version used for the project
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    // set the name of the project
    name := "cymbium",

    libraryDependencies += "org.typelevel" %% "spire" % "0.16.0",

    libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",

    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3"
    ),

    scalacOptions += "-Ypartial-unification"
  )

