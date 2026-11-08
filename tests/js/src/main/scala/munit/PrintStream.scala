package java.io

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import _root_.io.github.scalawasm.wasi4s.wasi.cli.stdout.getStdout

private object WasiPrinter {
  val stdout = getStdout()
  val nl = Array('\n'.toByte)

  @scala.annotation.tailrec
  def doPrintln(x: Array[Byte]): Unit =
    if (x.size <= 4095) {
      stdout.blockingWriteAndFlush(x)
      stdout.blockingWriteAndFlush(nl)
    } else {
      val (lhs, rhs) = x.splitAt(4096)
      stdout.blockingWriteAndFlush(lhs)
      doPrintln(rhs)
    }
}

class PrintStream(out: OutputStream) {
  def println(x: String): Unit =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiPrinter.doPrintln(x.getBytes)
    } {
      scala.Console.println(x)
    }

  def println(x: Object): Unit =
    println(x.toString())
}
