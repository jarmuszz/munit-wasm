package munit.internal.io

import scala.scalajs.js

private[internal] object JSPathUtils extends PathUtilsImpl {
  private def path: Option[js.Dynamic] = JSFileSystem.path

  def workingDirectory: String = JSFileSystem.cwd()

  def separator: String = path match {
    case Some(p) => p.sep.asInstanceOf[String]
    case None => "/"
  }

  def pathSeparator: String = path match {
    case Some(p) => p.delimeter.asInstanceOf[String]
    case None => ":"
  }

  def isAbsolute(filename: String): Boolean = path match {
    case Some(p) => p.isAbsolute(filename).asInstanceOf[Boolean]
    case None => filename.startsWith("/")
  }

  def getParent(filename: String): String = path match {
    case Some(p) => p.dirname(filename).asInstanceOf[String]
    case None => throw new UnsupportedOperationException(
        "Path.getParent() is only supported in Node.js"
      )
  }

  def basename(filename: String): String = path match {
    case Some(p) => p.basename(filename).asInstanceOf[String]
    case None => throw new UnsupportedOperationException(
        "Path.getFileName() is only supported in Node.js"
      )
  }

  def normalize(filename: String): String = path match {
    case Some(p) => p.normalize(filename).asInstanceOf[String]
    case None => throw new UnsupportedOperationException(
        "Path.normalize() is only supported in Node.js"
      )
  }

  def relativize(base: String, other: String): String = path match {
    case Some(p) => p.relative(base, other).asInstanceOf[String]
    case None => throw new UnsupportedOperationException(
        "Path.relativize() is only supported in Node.js"
      )
  }

  // JSPath.resolve(relpath, relpath) produces an absolute path from cwd.
  // This method turns the generated absolute path back into a relative path.
  private def adjustResolvedPath(base: String, resolved: String): String =
    if (isAbsolute(base)) resolved else relativize(workingDirectory, resolved)

  def resolve(base: String, other: String): String = path match {
    case Some(p) =>
      adjustResolvedPath(base, p.resolve(base, other).asInstanceOf[String])
    case None => throw new UnsupportedOperationException(
        "Path.resolve() is only supported in Node.js"
      )
  }

  def resolveSibling(base: String, other: String): String = path match {
    case Some(p) =>
      adjustResolvedPath(
        base,
        p.resolve(p.dirname(base).asInstanceOf[String], other)
          .asInstanceOf[String],
      )
    case None => throw new UnsupportedOperationException(
        "Path.resolveSibling() is only supported in Node.js"
      )
  }
}
