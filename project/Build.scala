import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "BeerCrush"
    val appVersion      = "1.0"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.specs2" %% "specs2" % "1.8.2" % "test",
	  "play" %% "play-test" % "2.0" % "test"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
    )

}
