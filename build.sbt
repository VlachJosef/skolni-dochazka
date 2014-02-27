name := "skolni"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe" %% "play-plugins-redis" % "2.2.0",
  "com.github.nscala-time" %% "nscala-time" % "0.8.0",
  "redis.clients" % "jedis" % "2.2.1",
  "ws.securesocial" %% "securesocial" % "2.1.3"
)   

resolvers += Resolver.sonatypeRepo("releases") 

play.Project.playScalaSettings

