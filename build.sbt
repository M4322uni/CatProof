scalaVersion := "3.8.4"

lazy val root = rootProject
  .settings(
    name := "CatProof",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "26.0.0-R38",
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.fxmisc.richtext" % "richtextfx" % "0.11.7"
    )
  )
