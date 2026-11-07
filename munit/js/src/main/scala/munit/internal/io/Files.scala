package munit.internal.io

import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.collection.JavaConverters._

object Files {
  def readAllLines(path: MunitPath): ju.List[String] = {
    val bytes = readAllBytes(path)
    val text = new String(bytes, StandardCharsets.UTF_8)
    text.linesIterator.toSeq.asJava
  }
  def readAllBytes(path: MunitPath): Array[Byte] =
    FileSystem.readAllBytes(path.toString)

  def exists(path: MunitPath): Boolean =
    FileSystem.exists(path.toString)
}
