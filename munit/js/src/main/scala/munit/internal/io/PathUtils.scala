package munit.internal.io

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}

private[internal] trait PathUtilsImpl {
  def workingDirectory: String
  def separator: String
  def pathSeparator: String
  def isAbsolute(path: String): Boolean
  def getParent(path: String): String
  def basename(path: String): String
  def normalize(path: String): String
  def relativize(base: String, other: String): String
  def resolve(base: String, other: String): String
  def resolveSibling(base: String, other: String): String
}

object PathUtils {
  private val impl: PathUtilsImpl =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiPathUtils: PathUtilsImpl
    } {
      JSPathUtils: PathUtilsImpl
    }

  def workingDirectory: String = impl.workingDirectory
  def separator: String = impl.separator
  def pathSeparator: String = impl.pathSeparator
  def isAbsolute(path: String): Boolean = impl.isAbsolute(path)
  def getParent(path: String): String = impl.getParent(path)
  def basename(path: String): String = impl.basename(path)
  def normalize(path: String): String = impl.normalize(path)
  def relativize(base: String, other: String): String =
    impl.relativize(base, other)
  def resolve(base: String, other: String): String = impl.resolve(base, other)
  def resolveSibling(base: String, other: String): String =
    impl.resolveSibling(base, other)
}
