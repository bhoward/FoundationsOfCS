name := "fcsc-scala"

scalaVersion := "2.11.5"

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil

unmanagedResourceDirectories in Compile := Nil

unmanagedResourceDirectories in Test := Nil

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
