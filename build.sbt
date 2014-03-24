name := "dochazka"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe" %% "play-plugins-redis" % "2.2.0",
  "com.github.nscala-time" %% "nscala-time" % "0.8.0",
  "redis.clients" % "jedis" % "2.2.1"
)   


play.Project.playScalaSettings
