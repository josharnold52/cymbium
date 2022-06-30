
//ThisBuild / organization := "org.tdwarf"
//ThisBuild / scalaVersion := "2.12.8"
// set the Scala version used for the project
ThisBuild / version      := "0.1.0-SNAPSHOT"


lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",
  organization := "org.tdwarf",
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",
  libraryDependencies += "org.typelevel" %% "spire" % "0.16.0",

  //libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",

  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  ),

  libraryDependencies +=
    "org.typelevel" %% "discipline" % "0.10.0" % Test,

  libraryDependencies +=
    "org.specs2" %% "specs2-scalacheck" % "4.3.0" % Test,

  scalacOptions += "-feature",
  scalacOptions += "-unchecked",
  scalacOptions += "-deprecation"


)

lazy val core = (project in file("core"))
  .dependsOn(macroSub)
  .settings(
  	commonSettings,
    name := "cymbium",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies +=
      "org.typelevel" %% "spire-laws" % "0.16.0" % Test,
    libraryDependencies +=
      "org.typelevel" %% "cats-laws" % "1.6.0" % Test,

  )

lazy val macroSub = (project in file("macro"))
  .settings(
    commonSettings
    //libraryDependencies += scalaReflect.value
    // other settings here
  )


lazy val examples = (project in file("examples"))
  .dependsOn(core)
  .dependsOn(macroSub)
  .settings(
    commonSettings,
    scalacOptions += "-Ypartial-unification"

  )

//lazy val tests