name := "dochazka"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe" %% "play-plugins-redis" % "2.2.0"
)   


play.Project.playScalaSettings
