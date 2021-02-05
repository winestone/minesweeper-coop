import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

val circeVersion = "0.13.0"
lazy val Http4sVersion = "0.21.18"
// lazy val UpickleVersion = "0.7.1"
lazy val ScalaJsDomVersion = "1.1.0"
lazy val ScalaTagsVersion = "0.9.3"

//    "org.specs2" %% "specs2-core" % "3.6.5" % "test"
lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.13.1",
  scalacOptions in Test ++= Seq("-Yrangepos"),
  cancelable in Global := true
)

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core"))
  .settings(commonSettings)
  .jsSettings()

lazy val core_js = core.js
lazy val core_jvm = core.jvm

lazy val client = (project in file("client"))
  .dependsOn(core_js)
  .settings(commonSettings)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % ScalaJsDomVersion,
      // "com.lihaoyi" %%% "upickle" % UpickleVersion,
      "com.lihaoyi" %%% "scalatags" % ScalaTagsVersion
    ),
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion),
    scalaJSUseMainModuleInitializer := true
  )

lazy val server = (project in file("server"))
  .dependsOn(core_jvm)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl",
      "org.http4s" %% "http4s-blaze-server",
      "org.http4s" %% "http4s-blaze-client",
//      "org.http4s" %% "http4s-argonaut",
    ).map(_ % Http4sVersion),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(core_js, core_jvm, client, server)
