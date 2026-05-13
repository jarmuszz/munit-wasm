package munit.internal.io

import java.net.URI

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import scala.scalajs.wasi
import scala.scalajs.wasi.filesystem.types.Descriptor

// obtained implementation by experimentation on the JDK.
class File(path: String/*, private val descriptor: Option[Descriptor]*/) {
  def this(parent: String, child: String) = this(parent + File.separator + child)
  def this(parent: File, child: String) = this(parent.getPath, child)
  def this(uri: URI) = this(
    if (uri.getScheme != "file")
      throw new IllegalArgumentException("URI scheme is not \"file\"")
    else uri.getPath
  )
  def toPath: MunitPath = MunitPath(path)
  def toURI: URI = {
    val file = getAbsoluteFile.toString
    val uripath =
      if (file.startsWith("/")) file else "/" + file.replace(File.separator, "/")
    val withslash =
      if (isDirectory && !uripath.endsWith("/")) uripath + "/" else uripath
    new URI("file", null, withslash, null)
  }
  def getAbsoluteFile: File = toPath.toAbsolutePath.toFile
  def getAbsolutePath: String = getAbsoluteFile.toString
  def getParentFile: File = toPath.getParent.toFile
  def mkdirs(): Unit = throw new UnsupportedOperationException(
    "mkdirs() is not supported in Scala.js"
  )
  def getPath: String = path
  def exists(): Boolean = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiIO.exists(path)
    } {
      JSIO.exists(path)
    }
  def isFile: Boolean =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiIO.isFile(path)
    } {
      JSIO.isFile(path)
    }
  def isDirectory: Boolean = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiIO.isDirectory(path)
    } {
      JSIO.isDirectory(path)
    }
  override def toString: String = path
}

object File {
  def listRoots(): Array[File] = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      wasi.filesystem.preopens.getDirectories.map { tuple =>
        new File(tuple._2)
      }
    } {
      Array(new File(
        JSIO.path match {
          case Some(p) => p.parse(p.resolve()).root.asInstanceOf[String]
          case None => "/"
        }
        // if (JSIO.isNode) JSPath.parse(JSPath.resolve()).root
        // else "/"
    ))
    }

  def separatorChar: Char = separator.charAt(0)

  def separator: String = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      "/"
    } {
      JSIO.path match {
        case Some(p) => p.sep.asInstanceOf[String]
        case None => "/"
    }
  }

  def pathSeparator: String = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      ":"
    } {
      JSIO.path match {
        case Some(p) => p.delimeter.asInstanceOf[String]
        case None => ":"
    }
  }
}
