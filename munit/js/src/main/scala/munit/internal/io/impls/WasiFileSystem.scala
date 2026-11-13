package munit.internal.io

import munit.internal.PlatformCompat

import java.io.IOException
import java.nio.file.InvalidPathException

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
import scala.scalajs.wit.{Err, Ok}

import io.github.scalawasm.wasi4s.wasi
import io.github.scalawasm.wasi4s.wasi.filesystem.types.{
  Descriptor, DescriptorFlags, DescriptorType, ErrorCode, OpenFlags, PathFlags,
}
import io.github.scalawasm.wasi4s.wasi.io.streams.{InputStream, StreamError}

private[internal] object WasiFileSystem extends FileSystemImpl {

  def exists(path: String): Boolean = getDescriptor(path).isRight

  def isFile(path: String): Boolean = getDescriptor(path).map(descriptor =>
    descriptor.getType() match {
      case ok: Ok[DescriptorType] =>
        val tpe = ok.value
        tpe == DescriptorType.RegularFile
      case _: Err[_] => false
    }
  ).getOrElse(false)

  def isDirectory(path: String): Boolean = getDescriptor(path).map(descriptor =>
    descriptor.getType() match {
      case ok: Ok[DescriptorType] => ok.value == DescriptorType.Directory
      case _: Err[_] => false
    }
  ).getOrElse(false)

  def doRead(
      stream: InputStream,
      acc: ArrayBuilder[Byte],
  ): Either[IOException, Array[Byte]] = stream.blockingRead(4096) match {
    case Err(StreamError.`Closed`) => Right(acc.result())
    case err: Err[StreamError] => Left(new IOException(err.value.toString()))
    case ok: Ok[Array[Byte]] => doRead(stream, acc ++= ok.value)
  }

  def readAllBytes(path: String): Array[Byte] = getDescriptor(path)
    .flatMap(descriptor =>
      descriptor.readViaStream(0) match {
        case err: Err[ErrorCode] => Left(new IOException(
            s"Could not create input-stream: ${err.value.toString}"
          ))
        case ok: Ok[InputStream] =>
          val stream = ok.value
          doRead(stream, ArrayBuilder.make)
      }
    ).fold(throw _, identity)

  def listRoots(): Array[String] = wasi.filesystem.preopens.getDirectories()
    .map(_._2)

  private def getDescriptor(path: String): Either[Exception, Descriptor] =
    WasiPathUtils.getRoot(path) match {
      case None => throw new IllegalArgumentException
      case Some((preopen, tail)) =>
        val preopens = wasi.filesystem.preopens.getDirectories()
        preopens.find(_._2 == preopen).toRight(new IOException(
          s"Could not find preopen:'$preopen'"
        )).flatMap(
          _._1.openAt(
            pathFlags = PathFlags.symlinkFollow,
            path = tail,
            openFlags = OpenFlags.apply(0),
            flags = DescriptorFlags.read | DescriptorFlags.write,
          ) match {
            case ok: Ok[Descriptor] => Right(ok.value)
            case err: Err[ErrorCode] => Left(new IOException(err.value.toString()))
          }
        )
    }

  private def getParent(path: String): Either[Exception, Descriptor] =
    getDescriptor(path).flatMap { file =>
      val result = file.openAt(
        pathFlags = PathFlags.symlinkFollow,
        path = "..",
        openFlags = OpenFlags.directory,
        flags = DescriptorFlags.apply(0),
      )

      result match {
        case ok: Ok[Descriptor] => Right(ok.value)
        case err: Err[ErrorCode] => Left(new IOException(err.value.toString()))
      }
    }
}
