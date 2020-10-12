name := "SandBox"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq (
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl" % "1.9.11",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.9.11" classifier "natives-desktop",
)
