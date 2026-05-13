package munit.internal.io

import java.nio.charset.StandardCharsets
import java.{util => ju}

import scala.collection.JavaConverters._
import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import scala.scalajs.wasi.filesystem.types.{Descriptor, DescriptorType, DirectoryEntryStream, DirectoryEntry, DescriptorFlags, OpenFlags, PathFlags, ErrorCode}
import scala.scalajs.wasi.io.streams.{InputStream, StreamError}
import scala.scalajs.wasi
import scala.scalajs.wit.{Ok, Err}

object Files {
  def readAllLines(path: MunitPath): ju.List[String] = {
    val bytes = readAllBytes(path)
    val text = new String(bytes, StandardCharsets.UTF_8)
    text.linesIterator.toSeq.asJava
  }
  def readAllBytes(path: MunitPath): Array[Byte] = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      val stringPath = path.toString
      val (preopen, tail) = stringPath.splitAt(stringPath.indexOf('/'))

      val preopens = wasi.filesystem.preopens.getDirectories()
      preopens.find(_._2 == preopen) match {
        case None => throw new Exception("No such file")
        case Some(pair) =>
          // TODO: better error handling
 
          val fd = pair._1
          val file = fd.openAt(
            pathFlags = PathFlags.symlinkFollow,
            path = tail,
            openFlags = OpenFlags.apply(0),
            flags = DescriptorFlags.read
          )

           file match {
            case err: Err[ErrorCode] => throw new Exception(err.value.toString)
            case ok: Ok[Descriptor] => {
              ok.value.readViaStream(0) match {
                case e: Err[ErrorCode] => throw new Exception(e.value.toString)
                case ok: Ok[InputStream] =>
                  val stream = ok.value
                  def loop(ab: mutable.ArrayBuilder[Byte]): Array[Byte] = stream.blockingRead(32) match {
                    case e: Err[StreamError] =>
                      //throw new Exception(e.value.toString)
                      ab.result()
                    case ok: Ok[Array[Byte]] =>
                      loop(ab.addAll(ok.value))
                  }

                  loop(mutable.ArrayBuilder.make[Byte])
              }
            }
          }
      }
    } {
      val jsArray = JSIO.fs match {
        case Some(fs) => fs.readFileSync(path.toString).asInstanceOf[js.Array[Int]]
        case None => new js.Array[Int](0)
      }
      val len = jsArray.length
      val result = new Array[Byte](len)
      var curr = 0
      while (curr < len) {
        result(curr) = jsArray(curr).toByte
        curr += 1
      }
      result
    }

  def exists(path: MunitPath): Boolean = 
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiIO.exists(path.toString)
    } {
      JSIO.exists(path.toString)
    }
}
