val dottyVersion = "3.0.0-RC1"

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-bits" % "1.1.24",
      "org.scodec" %% "scodec-core" % "2.0-16-011fe29",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
