package munit.internal.io

private[internal] object WasiPathUtils extends PathUtilsImpl {
  // TODO: How should the working directory behave on WASI?
  def workingDirectory: String = "/"

  def separator: String = "/"

  def pathSeparator: String = ":"

  def isAbsolute(path: String): Boolean =
    // All paths in WASI p2 are relative
    false

  def dirname(path: String): String =
    path.patch(path.lastIndexOf('/'), "", 1)

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
    base + "/" + other

  def resolveSibling(base: String, other: String): String =
    dirname(base) + '/' + other
}
