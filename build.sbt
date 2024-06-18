name := "klee"
version := "0.1.0-SNAPSHOT"
organization := "skac112"
scalaVersion := "3.3.0"
exportJars := true
libraryDependencies += "skac112" %% "vgutils" % "3.0.0-SNAPSHOT"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
//libraryDependencies += "skac112" %% "funnodes" % "0.0.3-SNAPSHOT"
//libraryDependencies += "skac112" % "pixlouds" % "0.1.0-SNAPSHOT"
//libraryDependencies += "org.clojure" % "clojure" % "1.10.1"
libraryDependencies += "org.scalanlp" %% "breeze" % "2.0.1-RC1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.2"
resolvers += Resolver.mavenLocal
//assembly / mainClass := Some("com.github.skac112.klee.Main$")
autoScalaLibrary := false
//assembly / assemblyPackageScala / assembleArtifact :=

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
