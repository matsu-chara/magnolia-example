lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.12.6",
    version      := "0.1.0-SNAPSHOT",
    name := "magnolia-example",
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia" % "0.7.1"
    )
  )
