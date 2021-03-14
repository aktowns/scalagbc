val dottyVersion = "3.0.0-RC1"

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

name := "scalagbc"

version := "0.1.0"

scalaVersion := dottyVersion

libraryDependencies ++= Seq(
  "org.scodec"    %% "scodec-bits" % "1.1.24",
  "org.scodec"    %% "scodec-core" % "2.0-16-011fe29",
  "org.typelevel" %% "cats-core"   % "2.4.2",
  "org.typelevel" %% "cats-free"   % "2.4.2",
  "org.typelevel" %% "cats-effect" % "3.0.0-RC2"
)

scalacOptions ++= Seq(
  //  "-explain",             // explain errors in more detail
  "-deprecation",         // emit warning and location for usages of deprecated APIs
  "-feature",             // emit warning and location for usages of features that should be imported explicitly
  "-indent",              // allow significant indentation.
  "-new-syntax",          // require `then` and `do` in control expressions.
  "-print-lines",         // show source code line numbers.
  "-unchecked",           // enable additional warnings where generated code depends on assumptions
  "-Xmigration",          // warn about constructs whose behavior may have changed since version
  "-Ykind-projector",
 // "-rewrite" //, "-source 3.0-migration"
)
