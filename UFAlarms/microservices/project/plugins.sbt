resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
addSbtPlugin("com.eed3si9n"      % "sbt-assembly"          % "0.12.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-native-packager"   % "1.2.0-M8")
addSbtPlugin("se.marcuslonnberg" % "sbt-docker"            % "1.4.1")
addSbtPlugin("com.geirsson"      % "sbt-scalafmt"          % "0.6.1")
addSbtPlugin("org.scoverage"     % "sbt-scoverage"         % "1.5.0")
addSbtPlugin("org.scalastyle"    % "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")
