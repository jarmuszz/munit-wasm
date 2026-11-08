package munit.internal.io

import java.net.URI
import java.util

import scala.collection.JavaConverters._

// Rough implementation of java.nio.Path, should work similarly for the happy
// path but has undefined behavior for error handling.
case class MunitPath(filename: String) {
  private[this] val escapedSeparator = java.util.regex.Pattern
    .quote(File.separator)

  private def adjustIndex(idx: Int): Int = if (isAbsolute) idx + 1 else idx
  def subpath(beginIndex: Int, endIndex: Int): MunitPath = MunitPath(
    filename.split(escapedSeparator)
      .slice(adjustIndex(beginIndex), adjustIndex(endIndex)).mkString
  )
  def toFile: File = new File(filename)
  def isAbsolute: Boolean = PathUtils.isAbsolute(filename)

  def getName(index: Int): MunitPath = MunitPath(
    filename.split(escapedSeparator).lift(adjustIndex(index))
      .getOrElse(throw new IllegalArgumentException)
  )
  def getParent: MunitPath = MunitPath(PathUtils.getParent(filename))

  def toAbsolutePath: MunitPath =
    if (isAbsolute) this else MunitPath.workingDirectory.resolve(this)

  def relativize(other: MunitPath): MunitPath =
    MunitPath(PathUtils.relativize(filename, other.filename))
  def getNameCount: Int = {
    val strippeddrive =
      if (filename.length > 1 && filename(1) == ':') filename.substring(2)
      else filename
    val (first, remaining) = strippeddrive.split(escapedSeparator + "+")
      .span(_.isEmpty)
    if (remaining.isEmpty) first.length else remaining.length
  }
  def toUri: URI = toFile.toURI
  def getFileName(): MunitPath = MunitPath(PathUtils.basename(filename))
  def getRoot: MunitPath =
    if (!isAbsolute) null else MunitPath(File.separator)
  def normalize(): MunitPath = MunitPath(PathUtils.normalize(filename))
  def endsWith(other: MunitPath): Boolean = endsWith(other.toString)
  def endsWith(other: String): Boolean = paths(filename).endsWith(paths(other))
  def resolveSibling(other: MunitPath): MunitPath =
    resolveSibling(other.toString)
  def resolveSibling(other: String): MunitPath =
    MunitPath(PathUtils.resolveSibling(filename, other))
  def resolve(other: MunitPath): MunitPath = resolve(other.toString)
  def resolve(other: String): MunitPath =
    MunitPath(PathUtils.resolve(filename, other))
  def startsWith(other: MunitPath): Boolean = startsWith(other.toString)
  def startsWith(other: String): Boolean = paths(filename)
    .startsWith(paths(other))
  private def paths(name: String) = name.split(escapedSeparator)
  override def toString: String = filename
  def iterator(): util.Iterator[MunitPath] = filename.split(File.separator)
    .iterator.map(name => MunitPath(name): MunitPath).asJava
}

object MunitPath {
  def workingDirectory: MunitPath = MunitPath(PathUtils.workingDirectory)
}
