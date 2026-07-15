scalaVersion := "3.8.4"

lazy val root = rootProject
  .settings(
    name := "CatProof",
    libraryDependencies ++= Seq(
      //You can add library dependencies here, for example,
      //"org.scalatest" %% "scalatest" % "3.2.19" % Test,
      //"org.scalameta" %% "munit" % "1.2.3" % Test
      "org.scalafx" %% "scalafx" % "26.0.0-R38",
      "com.lihaoyi" %% "fastparse" % "3.1.1"
    )
  )
