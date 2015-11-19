lazy val commonSettings = Seq(
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
