package munit.internal.io

import java.net.URI

// obtained implementation by experimentation on the JDK.
class File(path: String) {
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
  def exists(): Boolean = FileSystem.exists(path)
  def isFile: Boolean = FileSystem.isFile(path)
  def isDirectory: Boolean = FileSystem.isDirectory(path)
  override def toString: String = path
}

object File {
  def listRoots(): Array[File] = FileSystem.listRoots().map(new File(_))

  def separatorChar: Char = separator.charAt(0)

  def separator: String = PathUtils.separator

  def pathSeparator: String = PathUtils.pathSeparator
}
