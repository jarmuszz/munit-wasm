package munit.internal.io

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}

private[internal] trait FileSystemImpl {
  def exists(path: String): Boolean
  def isFile(path: String): Boolean
  def isDirectory(path: String): Boolean
  def readAllBytes(path: String): Array[Byte]
  def listRoots(): Array[String]
}

object FileSystem {
  private val impl: FileSystemImpl =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiFileSystem: FileSystemImpl
    } {
      JSFileSystem: FileSystemImpl
    }

  def exists(path: String): Boolean = impl.exists(path)
  def isFile(path: String): Boolean = impl.isFile(path)
  def isDirectory(path: String): Boolean = impl.isDirectory(path)
  def readAllBytes(path: String): Array[Byte] = impl.readAllBytes(path)
  def listRoots(): Array[String] = impl.listRoots()
}
