import sbt.Keys._
import sbt._

name := "data-migration"

version := "0.1.0"

scalaVersion := "2.12.1"

organization := "sensitydockerrepo"

trapExit := false

libraryDependencies ++= {
  Seq(
    "com.datastax.cassandra"       %  "cassandra-driver-core"  % "3.3.0",
    "com.typesafe"                 % "config"                  % "1.2.1",
    "ch.qos.logback"               % "logback-classic"         % "1.1.5",
    "org.neo4j.driver"             % "neo4j-java-driver"       % "1.5.0",
    "com.datastax.cassandra"       % "cassandra-driver-extras" % "3.3.0" % "optional",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"   % "2.9.0.pr1"
  )
}
//Assemble merge strategy
assemblyMergeStrategy in assembly := {
  case PathList("reference.conf")     => MergeStrategy.concat
  case PathList("META-INF", xs @ _ *) => MergeStrategy.discard
  case _                              => MergeStrategy.first
}

resolvers ++= Seq(
  "anormcypher" at "http://repo.anormcypher.org/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)


