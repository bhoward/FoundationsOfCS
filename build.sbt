val sharedSettings = Seq(
  version := "0.1",
  organization := "edu.depauw",
  scalaVersion := "2.12.2"
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
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
      "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"
    ),
    resolvers += "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/releases"
  )
		
lazy val doc = scalatex.ScalatexReadme(
  projectId = "doc",
  wd = file(""),
  url = "http://github.com/bhoward/FoundationsOfCS/tree/master",
  source = "Doc"
 ).settings(sharedSettings: _*)
  .settings(
    name := "fcsc-doc",
    unmanagedSourceDirectories in Compile := Nil,
    unmanagedSourceDirectories in Test := Nil,
    unmanagedResourceDirectories in Compile := Nil,
    unmanagedResourceDirectories in Test := Nil
  )
