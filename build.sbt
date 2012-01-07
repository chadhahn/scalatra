import akka.sbt._

resolvers += "ScalaTools Snapshots nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots"

resolvers += "repository.jboss.org" at "https://repository.jboss.org/nexus/content/repositories/releases/"

resolvers += "Akka Repository" at "http://akka.io/repository"

libraryDependencies ++= Seq(
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.3.0",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.3.0",
  "com.typesafe.akka" % "akka-kernel" % "2.0-M2" % "provided;runtime",
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "org.jboss.netty" % "netty" % "3.2.7.Final",
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "com.google.guava" % "guava" % "10.0.1",
  "org.specs2" %% "specs2" % "1.7.1" % "test"
)

seq(AkkaKernelPlugin.distSettings :_*)