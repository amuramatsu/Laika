import sbt._
import Keys._
import bintray.Plugin.bintrayPublishSettings
import bintray.Keys._
import sbtunidoc.Plugin.ScalaUnidoc
import sbtunidoc.Plugin.UnidocKeys._
import sbtunidoc.Plugin.unidocSettings

object Build extends Build {


  object Settings {
    
    lazy val basic = Seq(
      version               := "0.8.0-SNAPSHOT",
      homepage              := Some(new URL("http://planet42.github.io/Laika/")),
      organization          := "org.planet42",
      organizationHomepage  := Some(new URL("http://www.planet42.org")),
      description           := "Text Markup Transformer for sbt and Scala applications",
      startYear             := Some(2012),
      licenses              := Seq("Apache 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
      scalaVersion          := "2.10.6",
      scalacOptions         := Opts.compile.encoding("UTF-8") :+ 
                               Opts.compile.deprecation :+ 
                               Opts.compile.unchecked :+ 
                               "-feature" :+ 
                               "-language:implicitConversions" :+ 
                               "-language:postfixOps" :+ 
                               "-language:higherKinds"
    )
    
    lazy val module = basic ++ Seq(
      crossVersion       := CrossVersion.binary,
      crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1")
    )
    
  }
  
  object Dependencies {
    
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"  % "test"
        
    val jTidy     = "net.sf.jtidy"  % "jtidy"      % "r938" % "test"
        
    val config    = "com.typesafe"  % "config"     % "1.0.2"
    
    val fop       = "org.apache.xmlgraphics" % "fop" % "2.1" 

    val icu4j     = "com.ibm.icu"   % "icu4j"      % "58.2"
    
  }
  
  object Plugins {
    
    import sbt.ScriptedPlugin._
    
    val scripted = scriptedSettings ++ Seq(

      scriptedLaunchOpts ++=
        Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value),

      scriptedBufferLog := false
    
    )
    
  }
  
  object Publishing {
    
    lazy val mavenCentral = Seq(
      
      publishMavenStyle       := true,

      publishArtifact in Test := false,

      pomIncludeRepository    := { _ => false },

      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (version.value.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },

      pomExtra := (
        <scm>
          <url>https://github.com/planet42/Laika.git</url>
          <connection>scm:git:https://github.com/planet42/Laika.git</connection>
        </scm>
        <developers>
          <developer>
            <id>jenshalm</id>
            <name>Jens Halm</name>
            <url>http://www.planet42.org</url>
          </developer>
        </developers>)
    )
    
    lazy val sbtPlugins = bintrayPublishSettings ++ Seq(
      
      publishMavenStyle := false,
      repository in bintray := "sbt-plugins",
      bintrayOrganization in bintray := None
      
    )
    
    lazy val none = Seq(
      publish := (),
      publishLocal := (),
      publishTo := None
    )
    
  }
  
  
  lazy val root = Project("root", file("."))
    .aggregate(core, pdf, plugin)
    .settings(Settings.basic: _*)
    .settings(Publishing.none: _*)
    .settings(unidocSettings: _*)
    .settings(unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(plugin))
 
  lazy val core = Project("laika-core", file("core"))
    .settings(Settings.module: _*)
    .settings(Publishing.mavenCentral: _*)
    .settings(libraryDependencies ++= Seq(
      Dependencies.config,
      Dependencies.scalatest, 
      Dependencies.jTidy,
      Dependencies.icu4j
    ))
    
  lazy val pdf = Project("laika-pdf", file("pdf"))
    .dependsOn(core)
    .settings(Settings.module: _*)
    .settings(Publishing.mavenCentral: _*)
    .settings(libraryDependencies ++= Seq(
      Dependencies.fop,
      Dependencies.scalatest  
    ))
    
  lazy val plugin = Project("laika-sbt", file("sbt"))
    .dependsOn(core)
    .dependsOn(pdf)
    .settings(
      sbtPlugin := true, 
      crossScalaVersions := Seq("2.10.5")
     )
    .settings(Settings.basic: _*)
    .settings(Publishing.sbtPlugins: _*)
    .settings(Plugins.scripted: _*)
  
    
}
