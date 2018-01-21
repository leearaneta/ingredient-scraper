name := "playingWithCats"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0",
  "org.jsoup" % "jsoup" % "1.8.3",
  "org.json4s" %% "json4s-native" % "3.6.0-M2",
  "com.softwaremill.sttp" %% "core" % "1.1.4",
  "com.softwaremill.sttp" %% "async-http-client-backend-future" % "1.1.4"
)