resolvers += Resolver.bintrayRepo("stew","snapshots")
libraryDependencies ++= Seq(
  "org.typelevel" %% "scalaz-outlaws" % "0.2",
  "net.arya" %% "util" % "0.0.3",
  scalaz("effect")
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.6.1" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

monocle
picky

