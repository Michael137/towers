scalaVersion := "2.13.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.22-SNAPSHOT"

testFrameworks += new TestFramework(
    "org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

fork in run := false
fork in console := false
