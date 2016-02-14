import sbt._
import Keys._

object depend {

  val scalazVersion   = "7.2.0"
  val specs2Version   = "3.7.1"
  val disorderVersion = "0.0.1-20150618032436-9c1f81e"

  val scalaz = Seq(
      "org.scalaz" %% "scalaz-core"
    ).map(_ % scalazVersion)

  val specs2 = Seq(
      "org.specs2" %% "specs2-core"
    , "org.specs2" %% "specs2-matcher-extra"
    , "org.specs2" %% "specs2-scalacheck"
    , "org.specs2" %% "specs2-html"
    , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

  val disorder = Seq(
    "com.ambiata" %% "disorder" % disorderVersion % "test"
  )

  val scalameter = Seq(
    "com.storm-enroute" %% "scalameter" % "0.7")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns))
}
