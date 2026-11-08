package munit.internal.io

private[internal] object WasiPathUtils extends PathUtilsImpl {
  // TODO: How should the working directory behave on WASI?
  def workingDirectory: String = "."

  def separator: String = "/"

  def pathSeparator: String = ":"

  def isAbsolute(path: String): Boolean =
    path.startsWith("/")

  def getParent(path: String): String =
    if (path.lastIndexOf(separator) == -1) null
    else path.take(path.lastIndexOf(separator) - 1)

  def basename(path: String): String =
    path.split('/').last

  def normalize(path: String): String =
    path // TODO: normalize

  def relativize(base: String, other: String): String = {
    if (other.size >= base.size)
      throw new IllegalArgumentException

    val (lhs, rhs) = base.splitAt(other.size)

    if (lhs == other)
      rhs.tail
    else
      throw new IllegalArgumentException
  }

  def resolve(base: String, other: String): String =
    if (isAbsolute(other)) other else base + "/" + other

  def resolveSibling(base: String, other: String): String =
    getParent(base) + '/' + other

  def getRoot(path: String): Option[(String, String)] = {
    val idx = path.indexOf(separator)
    if (idx == -1 || idx == path.size - 1) { // "foo" or "foo/"
      None
    } else {
      val lhs = path.take(idx)
      val rhs = path.drop(idx + 1)
      Some((lhs, rhs))
    }
  }
}
