name := "playingWithCats"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.jsoup" % "jsoup" % "1.8.3",
  "org.seleniumhq.selenium" % "selenium-java" % "3.11.0",
  "com.twitter" %% "util-collection" % "18.3.0",
  "com.twitter" %% "finagle-http" % "18.3.0",
  "com.github.finagle" %% "finch-core" % "0.17.0",
  "com.github.finagle" %% "finch-circe" % "0.17.0",
  "com.hypertino" %% "inflector" % "1.0.1"
)

val circeVersion = "0.9.1"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)