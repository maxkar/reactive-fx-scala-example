import sbt._
import Keys._
import sbtunidoc.Plugin._

/* Junit interface docs: https://github.com/szeiger/junit-interface  */

object Tasks {
  import java.io._
  import scala.collection.mutable.ArrayBuffer

  lazy val assemblyDir = settingKey[File]("assembly_product_dir")
  lazy val unpackedDir = settingKey[File]("assembly_unpacked_dir")
  lazy val itemName = settingKey[String]("assembly_item_name")

  lazy val assemblyPrepare = taskKey[Option[(File, File)]]("assembly_prepare")
  lazy val internalName = settingKey[String]("internal_name")


  private def grepf(x : File, path : String, buf : ArrayBuffer[(File, String)]) : Unit = {
    val subs = x.listFiles()
    if (subs == null)
      buf += ((x, path))
     else {
       val nhead = if (path.isEmpty) "" else (path + "/")
       subs.foreach(sub ⇒  grepf(sub, nhead + sub.getName, buf))
    }
  }


  private def doAssemblyPrepare(item : String,
      unpacked : File, deps : Seq[Attributed[File]], owns : Seq[File],
      runClass : Option[String]) : Option[(File, File)] = {

    if (!runClass.isDefined)
      return None


    val libDir = unpacked / "lib"
    libDir.mkdirs
    val appFile = unpacked / (item + ".jar")

    val classDirs = new ArrayBuffer[File]
    val classpathEntries = new ArrayBuffer[String]

    def pf(x : File) : Unit = {
      if (x.exists)
        if (x.isDirectory)
          classDirs += x
        else {
          IO.copyFile(x, libDir/ x.getName)
          classpathEntries += ("lib/" + x.getName)
        }
    }


    deps.foreach(x ⇒ pf(x.data))
    owns.foreach(pf)

    val fb = new ArrayBuffer[(File, String)]

    val mf = new java.util.jar.Manifest
    if (!classpathEntries.isEmpty)
      mf.getMainAttributes().putValue("Class-Path", classpathEntries.mkString(" "))
    mf.getMainAttributes().putValue("Main-Class", runClass.get)

    classDirs.foreach(x ⇒ grepf(x, "", fb))
    IO.jar(fb, appFile, mf)

    Some((appFile, libDir))
  }



  lazy val fullAssembly = Seq(
    assemblyDir := target.value / "assembly",
    itemName := baseDirectory.value.getName,
    assemblyPrepare <<=
      (itemName, assemblyDir,
        dependencyClasspath.in(Compile),
        products.in(Compile),
        mainClass.in(Compile)) map doAssemblyPrepare
    )
}



object MySettings {
  val trashDir = settingKey[File]("Directory for trash")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test",
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"/*, "-optimise"*/),
    scalacOptions in (Compile, doc) ++= Opts.doc.title("Secret tools")
  ) ++ Tasks.fullAssembly ++ Seq(
    trashDir := new File(".target").getAbsoluteFile(),
    target := trashDir.value / Tasks.internalName.value
  )
}


abstract sealed class RefEntry {
  def refs() : Seq[ClasspathDep[ProjectReference]]
  def libs() : Seq[File]
}


class ProjectEntry(prj : ProjectReference) extends RefEntry {
  def refs() : Seq[ClasspathDep[ProjectReference]] = Seq(prj)
  def libs() : Seq[File] = Seq.empty
}


class LibEntry(jars : Seq[File]) extends RefEntry {
  def refs() : Seq[ClasspathDep[ProjectReference]] = Seq.empty
  def libs() : Seq[File] = jars
}



object CustomBuild extends Build {
  import MySettings._
  import scala.language.implicitConversions


  @inline
  private implicit def prj2ref(prj : Project) : RefEntry =
    new ProjectEntry(prj)


  /** Creates a new library. */
  private def lib(names : String*) : LibEntry =
    new LibEntry(names.map(x ⇒  file("ext-lib") / (x + ".jar")))


  private def prj(path : String, deps : RefEntry*) : Project = {
    val pname = path.replace('/', '_')
    val extJars : Seq[File] = deps.flatMap(x ⇒  x.libs)
    Project(pname, file(path),
      settings = buildSettings ++ Seq(
        Tasks.internalName := path,
        unmanagedJars in Compile := extJars.map(Attributed.blank),
        fork in run := true)).
      dependsOn(deps.flatMap(x ⇒  x.refs) : _*)
  }

  /* Libraries. */
  lazy val lib_fun = prj("lib/fun")
  lazy val lib_reactive = prj("lib/reactive", lib_fun)
  lazy val lib_async = prj("lib/async", lib_reactive, lib_fun)

  /* Projects. */
  lazy val app = prj("app", lib_async)

  lazy val root = Project("root", file("."),
    settings = buildSettings ++ unidocSettings ++ Seq(Tasks.internalName := "root")
  ).aggregate(
    lib_fun,
    lib_reactive,
    lib_async,
    app
  )
}
