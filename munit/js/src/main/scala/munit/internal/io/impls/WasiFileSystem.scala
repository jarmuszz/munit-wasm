package munit.internal.io

import scala.collection.mutable

import scala.scalajs.wit.{Ok, Err}
import io.github.scalawasm.wasi4s.wasi
import io.github.scalawasm.wasi4s.wasi.filesystem.types.{Descriptor, DescriptorType, DescriptorFlags, OpenFlags, PathFlags, ErrorCode}
import io.github.scalawasm.wasi4s.wasi.io.streams.{InputStream, StreamError}

private[internal] object WasiFileSystem extends FileSystemImpl {

  def exists(path: String): Boolean = getDescriptor(path).isDefined

  def isFile(path: String): Boolean =
    getDescriptor(path).map { descriptor =>
      descriptor.getType() match {
        case ok: Ok[DescriptorType] => {
          val tpe = ok.value
          // should we count unknown as a file?
          tpe != DescriptorType.Directory || tpe != DescriptorType.Unknown
        }
        case _: Err[_] => false
      }
    }.getOrElse(false)

  def isDirectory(path: String): Boolean =
    getDescriptor(path).map { descriptor =>
      descriptor.getType() match {
        case ok: Ok[DescriptorType] => ok.value == DescriptorType.Directory
        case _: Err[_] => false
      }
    }.getOrElse(false)

  def readAllBytes(path: String): Array[Byte] = {
    val (preopen, tail) = path.splitAt(path.indexOf('/'))

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
                    ab ++= ok.value
                    loop(ab)
                }

                loop(mutable.ArrayBuilder.make[Byte])
            }
          }
        }
    }
  }

  def listRoots(): Array[String] =
    wasi.filesystem.preopens.getDirectories().map(_._2)

  def getDescriptor(path: String): Option[Descriptor] = {
    val (preopen, tail) = path.splitAt(path.indexOf('/'))

    val preopens = wasi.filesystem.preopens.getDirectories()

    preopens.find(_._2 == preopen).flatMap {
        _._1.openAt(
          pathFlags = PathFlags.symlinkFollow,
          path = tail,
          openFlags = OpenFlags.apply(0),
          flags = DescriptorFlags.read | DescriptorFlags.write
        ) match {
          case ok: Ok[Descriptor] => Some(ok.value)
          case err: Err[ErrorCode] => None
        }
    }
  }

  def getParent(path: String): Option[Descriptor] = {
    getDescriptor(path).flatMap { file =>
      val result = file.openAt(
        pathFlags = PathFlags.symlinkFollow,
        path = "..",
        openFlags = OpenFlags.directory,
        flags = DescriptorFlags.apply(0)
      )

      result match {
        case ok: Ok[Descriptor] => Some(ok.value)
        case _: Err[_] => None
      }
    }
  }
}
