import play.Project._

name := """hello-play-scala"""

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.2.0", 
  "org.webjars" % "bootstrap" % "2.3.1"
)

libraryDependencies ++= Seq(
  jdbc,
  anorm  
)

libraryDependencies ++= Seq( "mysql" % "mysql-connector-java" % "5.1.32" )

libraryDependencies ++= Seq( "commons-codec" % "commons-codec" % "1.7" )

libraryDependencies ++= Seq( "com.typesafe.play.plugins" %% "play-plugins-mailer" % "2.3.1" )

playScalaSettings
