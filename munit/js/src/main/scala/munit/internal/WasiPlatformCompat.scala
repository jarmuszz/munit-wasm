package munit.internal

import sbt.testing.{EventHandler, Logger, Task}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Awaitable, ExecutionContext, Future}

import _root_.io.github.scalawasm.wasi4s.wasi.cli
import _root_.io.github.scalawasm.wasi4s.wasi.clocks

private[internal] object WasiPlatformCompat extends PlatformCompatImpl {

  private lazy val stdout = cli.stdout.getStdout()
  private lazy val stderr = cli.stderr.getStderr()

  def print(s: String): Unit = {
    // blocking-write-and-flush will trap for more than 4096 bytes
    val maxBytes = 4096
    var arr: Array[Byte] = s.getBytes()
    while (arr.size > maxBytes) {
      val (l, r) = arr.splitAt(maxBytes)
      stdout.blockingWriteAndFlush(l)
      arr = r
    }

    if (arr.size > 0) stdout.blockingWriteAndFlush(arr)
  }

  def println(s: String): Unit = print(s + '\n')

  def printStackTrace(t: Throwable): Unit =
    println(s"$t: Stacktraces are not available on pure WebAssembly builds")

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

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] = {
    task.execute(eventHandler, loggers, _ => ())
    Future.successful(())
  }

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    startFuture()

  def setTimeout(ms: Int)(body: => Unit): () => Unit = {
    // WASI durations are expressed in nanoseconds.
    clocks.monotonic_clock.subscribeDuration(ms.toLong * 100000L).block()
    body

    () => ()
  }
}
