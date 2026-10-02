package munit.internal

import _root_.io.github.scalawasm.wasi4s.wasi.cli
import _root_.io.github.scalawasm.wasi4s.wasi.clocks
import _root_.io.github.scalawasm.wasi4s.wasi.random.insecure

object WasmSystem {
  private lazy val stdout = cli.stdout.getStdout()
  private lazy val stderr = cli.stderr.getStderr()

  def print(s: String, isErr: Boolean): Unit = {
    val out = if (isErr) stderr else stdout
    // blocking-write-and-flush will trap for more than 4096 bytes
    val maxBytes = 4096
    var arr: Array[Byte] = s.getBytes()
    while (arr.size > maxBytes) {
      val (l, r) = arr.splitAt(maxBytes)
      out.blockingWriteAndFlush(l)
      arr = r
    }

    if (arr.size > 0) out.blockingWriteAndFlush(arr)
  }

  def nanoTime(): Long =
    clocks.monotonic_clock.now()

  def currentTimeMillis(): scala.Long = {
    val now = clocks.wall_clock.now()
    (now.seconds * 1000L) + (now.nanoseconds / 1000000L)
  }

  def random(): Double = {
    // Map a 64-bit value to [0.0, 1.0), take the 53 high bits and scale by 2^-53.
    val i = insecure.getInsecureRandomU64()
    (i >>> 11).toDouble * (1.0 / (1L << 53))
  }
}
