import sbt.{Keys, _}
import sbtassembly.AssemblyPlugin.autoImport._
import sbtdocker.DockerPlugin.autoImport._
import sbtdocker.mutable.Dockerfile

object Docker {
  lazy val Settings = Seq(
    //Docker file
    dockerfile in docker := {
      val artifact: File     = assembly.value
      val artifactTargetPath = s"/app/${artifact.name}"

      new Dockerfile {
        from("anapsix/alpine-java")
        add(artifact, artifactTargetPath)
        entryPoint("java", "-jar","-XX:+UseG1GC","-XX:+UnlockExperimentalVMOptions", "-XX:+UseCGroupMemoryLimitForHeap",
          "-XX:MaxRAMFraction=1", artifactTargetPath)
      }
    },
    //Docker image name
    imageNames in docker := {
      val name         = Keys.name.value
      val organization = Keys.organization.value
      val version      = Keys.version.value

      Seq(ImageName(namespace = Some(organization), repository = name, tag = Some("latest")))
    },
    //Assemble merge strategy
    assemblyMergeStrategy in assembly := {
      case PathList("reference.conf")     => MergeStrategy.concat
      case PathList("META-INF", xs @ _ *) => MergeStrategy.discard
      case _                              => MergeStrategy.first
    }
  )
}
