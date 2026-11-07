package java.io

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import _root_.io.github.scalawasm.wasi4s.wasi.cli.stdout.getStdout

private object WasiPrinter {
  val stdout = getStdout()

  @scala.annotation.tailrec
  def doPrint(x: Array[Byte]): Unit =
    if (x.size <= 4096) {
      stdout.blockingWriteAndFlush(x)
    } else {
      val (lhs, rhs) = x.splitAt(4096)
      stdout.blockingWriteAndFlush(lhs)
      doPrint(rhs)
    }
}

class PrintStream(out: OutputStream) {
  def println(x: String): Unit =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiPrinter.doPrint(x.getBytes)
    } {
      scala.Console.println(x)
    }

  def println(x: Object): Unit =
    println(x.toString())
}
