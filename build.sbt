lazy val cats = "org.spire-math" %% "cats" % "0.3.0"

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(cats),
  scalaVersion := "2.11.7",
  scalacOptions := Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-Ywarn-dead-code",
    "-Xlint",
    "-Xfatal-warnings"
  )
)

lazy val untyped = project.settings(commonSettings: _*)
lazy val gadt = project.settings(commonSettings: _*)
