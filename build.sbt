name := "algorithm-design-analysis"

version := "1.0"

scalaVersion := "2.13.12"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.21"


lazy val Benchmark = config("bench") extend Test

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")


Benchmark / parallelExecution  := false
Benchmark / fork := true
Benchmark / logBuffered  := false

configs(Benchmark)
inConfig(Benchmark)(Defaults.testSettings)