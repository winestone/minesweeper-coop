import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

val circeVersion = "0.10.0"
lazy val Http4sVersion = "0.20.0-M6"
// lazy val UpickleVersion = "0.7.1"
lazy val ScalaJsDomVersion = "0.9.6"
lazy val ScalaTagsVersion = "0.6.7"

//    "org.specs2" %% "specs2-core" % "3.6.5" % "test"
lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.12.8",
  scalacOptions in Test ++= Seq("-Yrangepos"),
  cancelable in Global := true
)

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core"))
  .settings(commonSettings)
  .jsSettings(scalaJSUseMainModuleInitializer := true)

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
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s" %% "http4s-argonaut" % Http4sVersion,
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
  )
  .dependsOn(core_jvm)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(core_js, core_jvm, client, server)
