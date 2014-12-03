package ru.maxkar.util.vfs

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.{PosixFilePermissions ⇒ Perms}



/**
 * Filesystem mounter item. Mounts and unmounts
 * item inside specific directory.
 * <p>All mount/unmount operations are synchronous.
 */
class FuseMounter private(tmpPath : Path) {
  import FuseMounter._

  /**
   * Checks if this mounter can mount file with the
   * specific name.
   */
  def canMount(name : String) : Boolean =
    argBuilderFor(name) != null



  /**
   * Mounts file into the file system and returns
   * location of new mount point.
   */
  def mount(target : File) : File = {
    val argBuilder = argBuilderFor(target.getName())
    if (argBuilder == null)
      throw new IllegalArgumentException(
        "Could not mount file " + target + ", unsupported type")
    val dst = createMountDir()
    val mountArgs = argBuilder(target.toPath(), dst)

    try {
      doCommand(mountArgs)
      dst.toFile()
    } catch {
      case e : Exception ⇒
        Files.delete(dst)
        throw e
    }
  }



  /**
   * Unmounts a mount point and deletes it.
   */
  def unmount(target : Path) =
    doCommand(Array(
      "/usr/bin/fusermount",
      "-u",
      target.toString()))



  /**
   * Creates a new mount directory.
   */
  private def createMountDir() : Path =
    Files.createTempDirectory(tmpPath, null, MOUNT_ATTRS)
}




/**
 * Fuse mounter companion and mounting logic.
 */
object FuseMounter {
  /** Argument builder for the file mounter.
   * First argument is source file, second is
   * destination point.
   * Function returs array of process builder arguments.
   */
  type ArgBuilder = (Path, Path) ⇒ Array[String]



  final class MountFailureException extends Exception



  /** Filesystem mounting attributes. */
  private val MOUNT_ATTRS = Perms.asFileAttribute(
    Perms.fromString("r-x------"))



  /**
   * Returns argument builder used to build arguments
   * for the given file type. Returns null if file type
   * is not mountable.
   */
  private def argBuilderFor(name : String) : ArgBuilder = {
    val n1 = name.toLowerCase()
    if (n1.endsWith(".zip"))
      zipArgs
    else if (n1.endsWith(".rar"))
      rarArgs
    else
      null
  }



  /** Argument builder for the zip files.  */
  private def zipArgs(src : Path, dst : Path) : Array[String] =
    Array(
      "/usr/bin/fuse-zip",
      "-r",
      src.toString(),
      dst.toString()
    )



  /** Argument builder for the rar files. */
  private def rarArgs(src : Path, dst : Path) : Array[String] =
    Array(
      "/usr/bin/rar2fs",
      src.toString(),
      dst.toString()
    )



  /**
   * Attempts to invoke mount command using passed arguments.
   * @returns <code>true</code> iff mounting was successfull.
   */
  private def doCommand(args : Array[String]) : Unit = {
    val pb = new ProcessBuilder(args : _*)
    pb.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)

    val proc = pb.start()
    proc.getInputStream().close()

    var ret = proc.waitFor()
    if (ret != 0) {
      System.err.println("Abnormal mount return code " + ret)
      throw new MountFailureException()
    }
  }
}
