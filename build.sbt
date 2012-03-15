resolvers += Resolver.url("Typesafe repository - Snapshots", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-snapshots/"))(Patterns(false, "[organisation]/[module]/[revision]/jars/[artifact].[ext]"))

resolvers += Resolver.url("Typesafe repository - Releases", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))(Patterns(false, "[organisation]/[module]/[revision]/jars/[artifact].[ext]"))

sbtPlugin := true


name := "Disassembler2"

version := "0.1"

organization := "com.scalalabs"

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// set the Scala version used for the project
scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")


// Add a single dependency
libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "junit" % "junit" % "4.10" 


// Use the project version to determine the repository to publish to.
publishTo <<= version { (v: String) =>
  if(v endsWith "-SNAPSHOT")
    Some(ScalaToolsSnapshots)
  else
    Some(ScalaToolsReleases)
}