val sharedSettings = Seq(
  version := "0.1",
  organization := "edu.depauw",
  scalaVersion := "2.11.6"
)

lazy val fcsc = (project in file("."))
  .settings(sharedSettings: _*)
  .settings(
    name := "fcsc-scala",
    unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil,
    unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil,
    unmanagedResourceDirectories in Compile := Nil,
    unmanagedResourceDirectories in Test := Nil,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
      "com.storm-enroute" %% "scalameter-core" % "0.6"
    ),
    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases"
  )
		
lazy val doc = scalatex.ScalatexReadme(
  folder = "doc",
  url = "http://github.com/bhoward/FoundationsOfCS/tree/master",
  source = "Doc",
  targetFolder = "target/site"
 ).settings(sharedSettings: _*)
  .settings(
    name := "fcsc-doc",
    unmanagedSourceDirectories in Compile := Nil,
    unmanagedSourceDirectories in Test := Nil,
    unmanagedResourceDirectories in Compile := Nil,
    unmanagedResourceDirectories in Test := Nil
  )
