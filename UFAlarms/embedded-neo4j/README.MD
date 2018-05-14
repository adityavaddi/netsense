# Embedded Neo4J

To build, run the following in the root folder:
 
    gradlew -p embedded-neo4j clean build distZip

Copy the zip in embedded-neo4j/build/distributions to a machine with Java 8 installed.

Unzip the file. Move to the created folder.

    unzip embedded-neo4j-<version>.zip

    cd embedded-neo4j

Modify the configuration files in conf if necessary.

If you have sample data for the neo4j graph from elsewhere, replace the graph folder with the folder from the other instance that holds the graph database information.

Run the jar - 

    java -jar embedded-neo4j-<version>.jar


