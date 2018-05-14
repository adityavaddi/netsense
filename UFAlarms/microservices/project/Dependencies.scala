import sbt._

object Dependencies {
  val akkaVersion          = "2.5.4"
  val phantomDslVersion    = "2.3.0"
  val phantomProDslVersion = "2.16.4"
  val phantomProUdtVersion = "0.16.0"
  val akkaHttpVersion      = "10.0.7"
  val scalaTestVersion     = "3.0.1"
  val logbackVersion       = "1.1.5"
  val macroParadiseVersion = "2.1.0"
  val sparkVersion = "2.1.0"

  val phantomProCredentials = Seq(
    Credentials(
      "Bintray",
      "dl.bintray.com",
      "sensitysystems",
      "4b7a78e0e2ac03f4d56d2de06fa3118c5071b16d"
    )
  )

  val Core = Seq(
    "com.outworkers"               %% "phantom-dsl"                   % phantomDslVersion,
    "com.typesafe.akka"            %% "akka-actor"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-metrics"          % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-sharding"         % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-tools"            % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream"                   % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream-testkit"           % akkaVersion,
    "com.typesafe.akka"            %% "akka-testkit"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-slf4j"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-http-spray-json"          % akkaHttpVersion,
    "ch.qos.logback"               % "logback-classic"                % logbackVersion,
    "com.typesafe.akka"            %% "akka-stream-kafka"             % "0.14",
    "com.lightbend.akka"           %% "akka-stream-alpakka-mqtt"      % "0.11",
    "com.lightbend.akka"           %% "akka-stream-alpakka-cassandra" % "0.11",
    "io.scalac"                    %% "reactive-rabbit"               % "1.1.4",
    "org.scalatest"                %% "scalatest"                     % scalaTestVersion % "test",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"          % "2.9.0.pr1",
    "org.msgpack"                  % "msgpack-core"                   % "0.8.12",
    "org.msgpack"                  % "jackson-dataformat-msgpack"     % "0.8.12",
    "org.neo4j.driver"             % "neo4j-java-driver"              % "1.5.0",
    "org.bouncycastle"             % "bcpkix-jdk15on"                 % "1.56",
    "org.velvia"                   %% "msgpack4s"                     % "0.6.0",
    "nl.grons"                     %% "metrics-scala"                 % "3.5.6",
    "com.codahale.metrics"         % "metrics-graphite"               % "3.0.1",
    "com.codahale.metrics"         % "metrics-jvm"                    % "3.0.1",
    "org.json4s"                   %% "json4s-native"                 % "3.5.1",
    "org.json4s"                   %% "json4s-jackson"                % "3.5.1",
    "net.manub"                    %% "scalatest-embedded-kafka"      % "0.13.0",
    "org.velvia"                   %% "msgpack4s"                     % "0.6.0",
    "com.github.xuwei-k"           %% "msgpack4z-jawn"                % "0.3.5",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"          % "2.9.0.pr1",
    "com.github.xuwei-k"           % "msgpack4z-java"                 % "0.3.4",
    "org.specs2"                   %% "specs2-mock"          % "3.8.6" % "test",
    "commons-codec"                % "commons-codec"                  % "1.10"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  var AlertService = Seq(
    "com.typesafe.akka"            %% "akka-stream"          % akkaVersion,
    "com.outworkers"               %% "phantom-dsl"          % "2.20.0",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.0.pr1",
    "org.cassandraunit"            % "cassandra-unit"        % "3.1.3.2",
    "com.typesafe.akka"            %% "akka-stream-testkit"  % akkaVersion,
    "org.ehcache"                  % "ehcache"               % "3.5.2",
    "com.google.code.gson"         % "gson"                  % "2.8.1",
    "org.specs2"                   %% "specs2-mock"          % "3.8.6" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  var AlertSimulator = Seq(
    "com.typesafe.akka"            %% "akka-stream"          % akkaVersion,
    "com.outworkers"               %% "phantom-dsl"          % phantomDslVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.0.pr1",
    "org.cassandraunit"            % "cassandra-unit"        % "3.1.3.2",
    "com.typesafe.akka"            %% "akka-stream-testkit"  % akkaVersion
  )

  var SensorService = Seq(
    "ch.qos.logback"    % "logback-classic"           % "1.1.3",
    "com.outworkers"    %% "phantom-dsl"              % phantomDslVersion,
    "com.typesafe.akka" %% "akka-stream-kafka"        % "0.14",
    "com.typesafe"      % "config"                    % "1.3.1" % "test",
    "net.manub"         %% "scalatest-embedded-kafka" % "0.13.0" % "test",
    "org.cassandraunit" % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"        %% "specs2-mock"              % "3.8.6" % "test",
    "io.netty"          % "netty-transport-native-epoll"   % "4.1.20.Final",
    "com.github.tototoshi"         %% "scala-csv"            % "1.3.4"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  var ServerSentEventService = Seq(
    "io.spray"           %% "spray-json"                          % "1.3.3",
    "com.typesafe.akka"  %% "akka-persistence-cassandra"          % "0.54" % "test",
    "com.typesafe.akka"  %% "akka-persistence-cassandra-launcher" % "0.54" % "test",
    "com.typesafe.akka"  %% "akka-cluster"                        % akkaVersion,
    "com.lightbend.akka" %% "akka-stream-alpakka-cassandra"       % "0.10"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val TsAdapter = Seq(
    "com.typesafe.akka"            %% "akka-actor"                   % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-testkit"                 % akkaVersion,
    "org.apache.oltu.oauth2"       % "org.apache.oltu.oauth2.client" % "1.0.2",
    "com.typesafe.akka"            %% "akka-http"                    % akkaHttpVersion,
    "com.typesafe.akka"            %% "akka-http-spray-json"         % akkaHttpVersion,
    "com.typesafe.akka"            %% "akka-http-testkit"            % akkaHttpVersion,
    "org.scalatest"                %% "scalatest"                    % scalaTestVersion % "test",
    "io.scalac"                    %% "reactive-rabbit"              % "1.1.4",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"         % "2.9.0.pr1",
    "org.msgpack"                  % "msgpack-core"                  % "0.8.12",
    "org.msgpack"                  % "jackson-dataformat-msgpack"    % "0.8.12",
    "com.rabbitmq"                 % "amqp-client"                   % "4.0.0",
    "ch.qos.logback"               % "logback-classic"               % "1.1.8",
    "de.heikoseeberger"            %% "akka-http-jackson"            % "1.15.0",
    "com.typesafe.akka"            %% "akka-stream-kafka"            % "0.14",
    "com.github.cb372"             %% "scalacache-guava"             % "0.9.3",
    "com.outworkers"               %% "phantom-dsl"                  % phantomDslVersion,
    "org.scalamock"                %% "scalamock-scalatest-support"  % "3.5.0" % "test",
    "org.mockito"                  % "mockito-core"                  % "2.7.11",
    "org.cassandraunit"            % "cassandra-unit"                % "3.1.3.2",
    "com.github.nscala-time"       %% "nscala-time"                  % "2.16.0"
  )

  val EventSimulator = Seq(
    "com.rabbitmq"                 % "amqp-client"                    % "4.0.0",
    "com.typesafe.akka"            %% "akka-actor"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-agent"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-camel"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-metrics"          % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-sharding"         % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-tools"            % akkaVersion,
    "com.typesafe.akka"            %% "akka-contrib"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-multi-node-testkit"       % akkaVersion,
    "com.typesafe.akka"            %% "akka-osgi"                     % akkaVersion,
    "com.typesafe.akka"            %% "akka-persistence"              % akkaVersion,
    "com.typesafe.akka"            %% "akka-persistence-tck"          % akkaVersion,
    "com.typesafe.akka"            %% "akka-remote"                   % akkaVersion,
    "com.typesafe.akka"            %% "akka-slf4j"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream"                   % akkaVersion,
    "com.typesafe.akka"            %% "akka-http-core"                % "10.0.5",
    "com.typesafe.akka"            %% "akka-http"                     % "10.0.5",
    "com.typesafe.akka"            %% "akka-http-testkit"             % "10.0.5",
    "com.typesafe.akka"            %% "akka-http-spray-json"          % "10.0.5",
    "com.typesafe.akka"            %% "akka-http-jackson"             % "10.0.5",
    "ch.qos.logback"               % "logback-classic"                % "1.1.3",
    "io.scalac"                    %% "reactive-rabbit"               % "1.1.4",
    "com.outworkers"               %% "phantom-dsl"                   % phantomDslVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala"          % "2.9.0.pr1",
    "org.msgpack"                  % "msgpack-core"                   % "0.8.12",
    "org.msgpack"                  % "jackson-dataformat-msgpack"     % "0.8.12",
    "nl.grons"                     %% "metrics-scala"                 % "3.5.6",
    "com.gilt"                     %% "gfc-timeuuid"                  % "0.0.8",
    "com.newmotion"                %% "akka-rabbitmq"                 % "4.0.0",
    "org.eclipse.paho"             % "org.eclipse.paho.client.mqttv3" % "1.1.1",
    "commons-lang"                 % "commons-lang"                   % "2.6",
    "com.typesafe"                 % "config"                         % "1.3.1" % "test",
    "org.specs2"                   %% "specs2-mock"                   % "3.8.6" % "test",
    "org.scalamock"                %% "scalamock-scalatest-support"   % "3.5.0" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val TsEventSimulator = Seq(
    "com.typesafe.akka"            %% "akka-actor"                % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream"               % akkaVersion,
    "com.typesafe.akka"            %% "akka-testkit"              % akkaVersion,
    "com.typesafe.akka"            %% "akka-http"                 % akkaHttpVersion,
    "com.typesafe.akka"            %% "akka-http-spray-json"      % akkaHttpVersion,
    "com.typesafe.akka"            %% "akka-http-testkit"         % akkaHttpVersion,
    "org.scalatest"                %% "scalatest"                 % scalaTestVersion % "test",
    "io.scalac"                    %% "reactive-rabbit"           % "1.1.4",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"      % "2.9.0.pr1",
    "org.msgpack"                  % "msgpack-core"               % "0.8.12",
    "org.msgpack"                  % "jackson-dataformat-msgpack" % "0.8.12",
    "com.rabbitmq"                 % "amqp-client"                % "4.0.0",
    "ch.qos.logback"               % "logback-classic"            % "1.1.8",
    "de.heikoseeberger"            %% "akka-http-jackson"         % "1.15.0"
  )

  val BridgeService = Seq(
    "com.outworkers"               %% "phantom-dsl"                   % phantomDslVersion,
    "com.typesafe.akka"            %% "akka-actor"                    % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-metrics"          % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-sharding"         % akkaVersion,
    "com.typesafe.akka"            %% "akka-cluster-tools"            % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream"                   % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream-testkit"           % akkaVersion,
    "com.typesafe.akka"            %% "akka-testkit"                  % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream-kafka"             % "0.14" exclude ("log4j", "log4j"),
    "com.lightbend.akka"           %% "akka-stream-alpakka-mqtt"      % "0.11",
    "com.lightbend.akka"           %% "akka-stream-alpakka-cassandra" % "0.11",
    "io.scalac"                    %% "reactive-rabbit"               % "1.1.4",
    "org.scalatest"                %% "scalatest"                     % "3.0.1",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"          % "2.9.0.pr1",
    "org.msgpack"                  % "msgpack-core"                   % "0.8.12",
    "org.msgpack"                  % "jackson-dataformat-msgpack"     % "0.8.12",
    "org.bouncycastle"             % "bcpkix-jdk15on"                 % "1.56",
    "ch.qos.logback"               % "logback-classic"                % "1.1.5",
    "com.typesafe.scala-logging"   %% "scala-logging"                 % "3.5.0",
    "org.velvia"                   %% "msgpack4s"                     % "0.6.0",
    "com.github.xuwei-k"           %% "msgpack4z-jawn"                % "0.3.5",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"          % "2.9.0.pr1",
    "com.github.xuwei-k"           % "msgpack4z-java"                 % "0.3.4",
    "org.clojure"                  % "clojure"                        % "1.8.0",
    "io.moquette"                  % "moquette-broker"                % "0.10"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val SensorSampleSimulator = Seq(
    "ch.qos.logback"               % "logback-classic"       % "1.1.3",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.0.pr1",
    "nl.grons"                     %% "metrics-scala"        % "3.5.6",
    "com.typesafe.akka"            %% "akka-stream-kafka"    % "0.14",
    "io.kamon"                     %% "kamon-core"           % "0.6.6",
    "com.github.tototoshi"         %% "scala-csv"            % "1.3.4"
  )

  val ParkingCommon = Seq(
    compilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.full),
    "com.outworkers" %% "phantom-dsl" % phantomProDslVersion,
    "com.outworkers" %% "phantom-udt" % phantomProUdtVersion,
    "org.specs2"     %% "specs2-mock" % "3.8.6" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val Parking = Seq(
    "com.typesafe.akka"            %% "akka-stream"              % akkaVersion,
    "ch.qos.logback"               % "logback-classic"           % "1.1.3",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"     % "2.9.0.pr1",
    "com.typesafe.akka"            %% "akka-stream-testkit"      % akkaVersion,
    "com.typesafe.akka"            %% "akka-stream-kafka"        % "0.14",
    "com.typesafe"                 % "config"                    % "1.3.1" % "test",
    "net.manub"                    %% "scalatest-embedded-kafka" % "0.13.0" % "test",
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"                   %% "specs2-mock"              % "3.8.6" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  var ParkingWhatif = Seq(
    compilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.full),
    "com.outworkers"    %% "phantom-dsl"              % phantomProDslVersion,
    "com.outworkers"    %% "phantom-udt"              % phantomProUdtVersion,
    "ch.qos.logback"    % "logback-classic"           % "1.1.3",
    "com.typesafe.akka" %% "akka-stream-kafka"        % "0.14",
    "com.typesafe"      % "config"                    % "1.3.1" % "test",
    "net.manub"         %% "scalatest-embedded-kafka" % "0.13.0" % "test",
    "org.cassandraunit" % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"        %% "specs2-mock"              % "3.8.6" % "test",
    "io.netty"          % "netty-transport-native-epoll"   % "4.1.20.Final"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val BusinessTrigger = Seq(
    "com.outworkers"               %% "phantom-dsl"               % phantomProDslVersion,
    "ch.qos.logback"               % "logback-classic"           % "1.1.3",
    "com.typesafe.akka"            %% "akka-stream-kafka"        % "0.14",
    "com.typesafe"                 % "config"                    % "1.3.1" % "test",
    "net.manub"                    %% "scalatest-embedded-kafka" % "0.13.0" % "test",
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"                   %% "specs2-mock"              % "3.8.6" % "test",
    "io.netty"                     % "netty-transport-native-epoll"   % "4.1.20.Final"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))


  val MediaService = Seq(
    "org.specs2"                   %% "specs2-mock"              % "3.8.6" % "test",
    "org.mockito"                  % "mockito-core"               % "2.7.11",
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val OTANextGen = Seq(
    "ch.qos.logback"               %  "logback-classic"          % "1.1.3",
    "com.outworkers"               %% "phantom-dsl"              % phantomDslVersion,
    "com.typesafe.akka"            %% "akka-stream-kafka"        % "0.14",
    "org.cassandraunit"            %  "cassandra-unit"           % "3.1.3.2",
    "com.github.seratch"           %% "awscala"                  % "0.6.+",
    "nl.grons"                     %% "metrics-scala"        % "3.5.6",
    "org.scalatest"                %% "scalatest"                % "3.0.4" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val ScheduleService = Seq(
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2",
    "io.netty"                     % "netty-transport-native-epoll"   % "4.1.20.Final"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val ScheduleTransformerService = Seq(
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2",
    "io.netty"                     % "netty-transport-native-epoll"   % "4.1.20.Final"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  var BusinessAlertService = Seq(
    "ch.qos.logback"    % "logback-classic"           % "1.1.3",
    "com.outworkers"    %% "phantom-dsl"              % phantomProDslVersion,
    "com.typesafe.akka" %% "akka-stream-kafka"        % "0.14",
    "com.typesafe"      % "config"                    % "1.3.1" % "test",
    "net.manub"         %% "scalatest-embedded-kafka" % "0.13.0" % "test",
    "org.cassandraunit" % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"        %% "specs2-mock"              % "3.8.6" % "test",
    "io.netty"          % "netty-transport-native-epoll"   % "4.1.20.Final"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val TrafficService = Seq(
    "org.cassandraunit"            % "cassandra-unit"            % "3.1.3.2",
    "org.specs2"                   %% "specs2-mock"              % "3.8.6" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

  val GpsService = Seq(
    "org.cassandraunit"            % "cassandra-unit"        % "3.1.3.2",
    "org.specs2"                   %% "specs2-mock"              % "3.8.6" % "test"
  ).map(_.exclude("io.dropwizard.metrics", "metrics-core"))

}
