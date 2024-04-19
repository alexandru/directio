import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import scalanative.build._

val scala3Version = "3.3.3"
val munitVersion = "0.7.29"

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(Seq(
    scalaVersion := scala3Version
))

lazy val root = crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Full)
    .in(file("."))
    .settings(
        name := "DirectIO",
        version := "0.1.0-SNAPSHOT",
    )
    .jvmSettings(
        libraryDependencies ++= Seq(
            "org.scalameta" %% "munit" % munitVersion % Test
        )
    )
    .nativeSettings(
        //...
    )
