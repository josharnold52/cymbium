
//ThisBuild / organization := "org.tdwarf"
//ThisBuild / scalaVersion := "2.12.8"
// set the Scala version used for the project
ThisBuild / version      := "0.1.0-SNAPSHOT"


lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",
  organization := "org.tdwarf",
  libraryDependencies += "org.typelevel" %% "spire" % "0.16.0",

  //libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",

  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  ),
  scalacOptions += "-feature",
  scalacOptions += "-deprecation"


)

lazy val core = (project in file("core"))
  .dependsOn(macroSub)
  .settings(
  	commonSettings,
    
  name := "cymbium",
    scalacOptions += "-Ypartial-unification"
  )

lazy val macroSub = (project in file("macro"))
  .settings(
    commonSettings
    //libraryDependencies += scalaReflect.value
    // other settings here
  )
