import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object depend {

  val scalazVersion   = "7.2.6"
  val specs2Version   = "3.8.5"

  val scalaz =
    Seq("org.scalaz" %%%! "scalaz-core" % scalazVersion) ++
    Seq("org.scalaz" %%   "scalaz-concurrent" % scalazVersion)

  val specs2 = Seq(
      "org.specs2" %% "specs2-core"
    , "org.specs2" %% "specs2-matcher-extra"
    , "org.specs2" %% "specs2-scalacheck"
    , "org.specs2" %% "specs2-html"
    , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

  val scalameter = Seq(
    "com.storm-enroute" %% "scalameter" % "0.7")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns))
}
