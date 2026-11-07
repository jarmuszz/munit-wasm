package munit.internal.io

import scala.scalajs.js
import scala.util.Try

private[internal] object JSFileSystem extends FileSystemImpl {

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

  def readAllBytes(path: String): Array[Byte] = {
    val jsArray = fs match {
      case Some(fs) => fs.readFileSync(path).asInstanceOf[js.Array[Int]]
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

  def listRoots(): Array[String] = Array(
    path match {
      case Some(p) => p.parse(p.resolve()).root.asInstanceOf[String]
      case None => "/"
    }
  )
}
