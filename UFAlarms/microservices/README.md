Navigating projects interactively
=================================
 

At the sbt interactive prompt, type projects to list your projects and project <projectname> to select a current project.
When you run a task like compile, it runs on the current project. So you don’t necessarily have to compile the root project, you could compile only a subproject.

You can run a task in another project by explicitly specifying the project ID, such as subProjectID/compile.


Docker
======

To build docker images 
```
sbt <<projectName>>/docker
```

Example: 
```
sbt ts-adapter/docker
```

Scala Format Plugin
===================

To format the code
```
sbt scalafmt 
```

Scalastyle - SBT plugin
=======================
Usage

```
sbt scalastyle 
```

