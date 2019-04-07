lazy val commonSettings = Seq(
  organization := "com.ricardomiranda",
  scalaVersion := "2.12.6",
  version := "0.1.0-SNAPSHOT"
)
lazy val dependencies = Seq(
  // https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor-typed
  "com.typesafe.akka" %% "akka-actor-typed" % "2.5.22",
  // https://mvnrepository.com/artifact/org.scalatest/scalatest
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  // https://mvnrepository.com/artifact/com.typesafe.akka/akka-testkit-typed
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.5.22" % Test,
  //https://mvnrepository.com/artifact/com.github.stefanbirkner/system-rules
  "com.github.stefanbirkner" % "system-rules" % "1.17.2" % Test
)
lazy val root = (project in file(".")).
  settings(
    commonSettings,
    // set the main class for packaging the main jar
    mainClass in (Compile, packageBin) := Some("com.ricardomiranda.akka_goose_game.Main"),
    name := "AkkaGooseGame",
    libraryDependencies ++= dependencies
  )

  // Simple and constant jar name
  assemblyJarName in assembly := s"akka-goose-game.jar"
