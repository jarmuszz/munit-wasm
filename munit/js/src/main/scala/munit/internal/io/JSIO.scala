package munit.internal.io

import java.util.Optional
import munit.internal.io.File.separator
import scala.scalajs.js
import scala.util.Try
import scala.scalajs.wasi
import scala.scalajs.wasi.filesystem.types.{Descriptor, DescriptorType, DirectoryEntryStream, DirectoryEntry, DescriptorFlags, OpenFlags, PathFlags, ErrorCode}
import scala.scalajs.wit
import scala.scalajs.wit.Ok
import scala.scalajs.wit.Err

object WasiIO {
  private def cwd(): String = "."

  def exists(path: String): Boolean  = getDescriptor(path).isDefined

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


object JSIO {

  private def require(module: String): Option[js.Dynamic] = Try(
    js.Dynamic.global.require(module)
  ) // Node.js
    .orElse( // JSDOM
      Try(js.Dynamic.global.Node.constructor("return require")()(module))
    ).toOption
  val process: Option[js.Dynamic] = require("process")
  val path: Option[js.Dynamic] = require("path")
  val fs: Option[js.Dynamic] = require("fs")

  def cwd(): String = process match {
    case Some(p) => p.cwd().asInstanceOf[String]
    case None => "/"
  }

  def exists(path: String): Boolean = fs match {
    case Some(f) => f.existsSync(path).asInstanceOf[Boolean]
    case None => false
  }

  def isFile(path: String): Boolean = exists(path) &&
    (fs match {
      case Some(f) => f.lstatSync(path).isFile().asInstanceOf[Boolean]
      case None => false
    })

  def isDirectory(path: String): Boolean = exists(path) &&
    (fs match {
      case Some(f) => f.lstatSync(path).isDirectory().asInstanceOf[Boolean]
      case None => false
    })
}
