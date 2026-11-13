package munit.internal

import sbt.testing.{EventHandler, Logger, Task}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Awaitable, ExecutionContext, Future}

import _root_.io.github.scalawasm.wasi4s.wasi.cli
import _root_.io.github.scalawasm.wasi4s.wasi.clocks

private[internal] object WasiPlatformCompat extends PlatformCompatImpl {

  private lazy val stdout = cli.stdout.getStdout()
  private lazy val stderr = cli.stderr.getStderr()

  private def print(s: String, isErr: Boolean): Unit = {
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

  def print(s: String): Unit = print(s, false)
  def println(s: String): Unit = print(s + '\n')

  def printStackTrace(t: Throwable): Unit =
    print(s"$t: Stacktraces are not available on pure WebAssembly builds\n", isErr = true)

  def nanoTime(): Long =
    clocks.monotonic_clock.now()

  def currentTimeMillis(): scala.Long = {
    val now = clocks.wall_clock.now()
    (now.seconds * 1000L) + (now.nanoseconds / 1000000L)
  }
  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.QueueExecutionContext()

  def awaitResult[T](awaitable: Awaitable[T]): T = Await
    .result(awaitable, Duration.Inf)

  def sleep(duration: FiniteDuration): Unit =
    clocks.monotonic_clock.subscribeDuration(duration.toNanos).block()

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    // WASI does not provide any way to timeout a future
    startFuture()

  def setTimeout(ms: Int)(body: => Unit): () => Unit = {
    // WASI durations are expressed in nanoseconds.
    clocks.monotonic_clock.subscribeDuration(ms.toLong * 1000000L).block()
    body

    () => ()
  }
}
