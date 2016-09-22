lazy val commonSettings =
  (List
    ( organization := "scalaz"
    , scalaVersion := "2.11.8"
    , scalacOptions ++=
        (List
          ( "-deprecation"
          , "-encoding", "UTF-8"       // yes, this is 2 args
          , "-feature"
          , "-language:existentials"
          , "-language:higherKinds"
          , "-language:implicitConversions"
          , "-unchecked"
          , "-Xfatal-warnings"
          , "-Xlint"
          , "-Yno-adapted-args"
          , "-Ywarn-dead-code"        // N.B. doesn't work well with the ??? hole
          , "-Ywarn-numeric-widen"
          , "-Ywarn-value-discard"
          , "-Xfuture"
          , "-Ywarn-unused-import"     // 2.11 only
          )
        )
    )
  )

lazy val deriving =
  project.in(file("deriving")).
    settings(commonSettings :_*).
    settings(
        name := "newtype-deriving"
      , addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
      , libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )

lazy val examples =
  project.in(file("examples")).
    settings(commonSettings :_*).
    settings(
        name := "newtype-examples"
      , scalacOptions += "-Xprint:typer"
      , libraryDependencies ++=
          (List
            ( "org.scalaz" %% "scalaz-core" % "7.1.4"
            , "org.scalaz" %% "scalaz-effect" % "7.1.4"
            )
          )
      , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
      , addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
    ).
    dependsOn(deriving)

