FROM    ubuntu:16.04
RUN     apt-get -qqy update \
        && apt-get -qqy install openjdk-8-jdk wget curl
WORKDIR /deploy
RUN     curl -L -o sbt-0.13.15.deb https://dl.bintray.com/sbt/debian/sbt-0.13.15.deb && \
        dpkg -i sbt-0.13.15.deb && \
        rm sbt-0.13.15.deb && \
        sbt sbtVersion
RUN     mkdir /deploy/microservices
WORKDIR /deploy/microservices
ADD     core ./core
ADD     src ./sse-service-simulator/src
ADD     build.sbt .
ADD     project ./project
RUN     sbt sse-service-simulator/assembly
# Remove src & move jar
RUN     mkdir -p /deploy/apps && \
        mv /deploy/microservices/sse-service-simulator/target/scala-2.12/sse-service-simulator-assembly-0.1.0.jar /deploy/apps/ && \
        rm -rf /deploy/microservices
WORKDIR /deploy/apps

CMD     ["java", "-jar", "/deploy/apps/sse-service-simulator-assembly-0.1.0.jar"]

