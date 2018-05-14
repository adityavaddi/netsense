import Dependencies._
import sbt.Keys._
import sbt._

//Aggregate of all projects
lazy val allProjects: Seq[ProjectReference] =
Seq(
  `core`,
  `ts-adapter`,
  `ts-adapter-db`,
  `alert-service`,
  `sensor-service`,
  `event-simulator`,
  `device-service`,
  `config-service`,
  `bridge-service`,
  `sensor-sample-kafka-simulator`,
  `parking-common`,
  `parking-policy-service`,
  `parking-group-policy-service`,
  `parking-tag-service`,
  `parking-whatif-service`,
  `business-trigger-service`,
  `kafkainit-service`,
  `schedule-service`,
  `schedule-transformer-service`,
  `kafkainit-service`,
  `media-service`,
  `ota-next-gen-service`,
  `business-alert-service`,
  `traffic-service`
)

//Common Settings
lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.12.1",
  organization := "sensitydockerrepo",
  credentials := phantomProCredentials,
  parallelExecution := false,
  test in assembly := {},
  fork in Test := true,
  envVars in Test := Map("metrics_enabled" -> "false")
)

//Root micro-services project
lazy val `microservices` = project
  .in(file("."))
  .aggregate(allProjects: _*)
  .settings(commonSettings)

//Core project
lazy val `core` = project
  .settings(
    commonSettings,
    resolvers ++= Seq(Resolver.bintrayRepo("velvia", "maven")),
    libraryDependencies ++= Core
  )

//Thingspace Adapter
lazy val `ts-adapter` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= TsAdapter
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Thingspace DB
lazy val `ts-adapter-db` = project.settings()

//sensor service project
lazy val `sensor-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.SensorService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Alert Service project
lazy val `alert-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.AlertService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Alert Simulator
lazy val `alert-simulator` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.AlertSimulator
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Event simulator
lazy val `event-simulator` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.EventSimulator
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Device service
lazy val `device-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(Resolver.bintrayRepo("velvia", "maven")),
    libraryDependencies ++= Dependencies.Core
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Server Sent Event
lazy val `sse-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(Resolver.bintrayRepo("velvia", "maven")),
    libraryDependencies ++= Dependencies.ServerSentEventService
  )
  .dependsOn(core)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Server Sent Event Simulator
lazy val `sse-service-simulator` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(Resolver.bintrayRepo("velvia", "maven")),
    libraryDependencies ++= Dependencies.ServerSentEventService
  )
  .dependsOn(`sse-service`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Config service
lazy val `config-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(Resolver.bintrayRepo("velvia", "maven")),
    libraryDependencies ++= Dependencies.Core
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//ts-event-simulator
lazy val `ts-event-simulator` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= TsEventSimulator
  )
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Bridge Service project
lazy val `bridge-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(Resolver.bintrayRepo("andsel", "maven")),
    libraryDependencies ++= Dependencies.BridgeService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//Bridge Service project
lazy val `sensor-sample-kafka-simulator` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.SensorSampleSimulator
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

lazy val `parking-common` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.bintrayRepo("outworkers", "enterprise-releases"),
      Resolver.jcenterRepo
    ),
    libraryDependencies ++= Dependencies.ParkingCommon
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//parking policy service
lazy val `parking-policy-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.Parking
  )
  .dependsOn(`parking-common`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//parking group policy service
lazy val `parking-group-policy-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.Parking
  )
  .dependsOn(`parking-common`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//kafka initializer
lazy val `kafkainit-service` = project
  .settings(commonSettings, Docker.Settings)
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)



//parking tag service
lazy val `parking-tag-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.Parking
  )
  .dependsOn(`parking-common`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//parking user data service
lazy val `parking-userdata-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.Parking
  )
  .dependsOn(`parking-common`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

// business trigger service
lazy val `parking-whatif-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    resolvers ++= Seq(
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.bintrayRepo("outworkers", "enterprise-releases"),
      Resolver.jcenterRepo
    ),
    libraryDependencies ++= Dependencies.ParkingWhatif
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

// business trigger service
lazy val `business-trigger-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.BusinessTrigger
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

// Media service
lazy val `media-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.MediaService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//OTA Next Gen service project
lazy val `ota-next-gen-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.OTANextGen
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//schedule service
lazy val `schedule-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.ScheduleService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//schedule transform service
lazy val `schedule-transformer-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.ScheduleTransformerService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

//business alert service
lazy val `business-alert-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.BusinessAlertService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

// Traffic Service
lazy val `traffic-service` = project
  .settings(
    commonSettings,
    resolvers ++= Seq(Resolver.sonatypeRepo("releases")),
    Docker.Settings,
    libraryDependencies ++= Dependencies.TrafficService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging)

// Gps Service
lazy val `gps-service` = project
  .settings(
    commonSettings,
    Docker.Settings,
    libraryDependencies ++= Dependencies.GpsService
  )
  .dependsOn(`core`)
  .enablePlugins(sbtdocker.DockerPlugin,JavaServerAppPackaging)

