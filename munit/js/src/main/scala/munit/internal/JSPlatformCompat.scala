package munit.internal

import sbt.testing.{EventHandler, Logger, Task}

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Awaitable, ExecutionContext, Future, Promise}
import scala.scalajs.js.timers
import scala.util.control.NonFatal

private[internal] object JSPlatformCompat extends PlatformCompatImpl {
  def nanoTime(): Long = System.nanoTime()

  def currentTimeMillis(): Long = System.currentTimeMillis()

  def print(s: String): Unit = scala.Console.out.print(s)

  def println(s: String): Unit = scala.Console.out.println(s)

  def printStackTrace(t: Throwable): Unit = t.printStackTrace()

  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  def awaitResult[T](awaitable: Awaitable[T]): T = Await
    .result(awaitable, Duration.Inf)

  // Scala.js is single threaded, so we can't block.
  def sleep(duration: FiniteDuration): Unit = ()

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    if (!duration.isFinite) startFuture()
    else {
      val onComplete = Promise[T]()
      val timeoutHandle = timers
        .setTimeout(duration.toMillis)(onComplete.tryFailure(
          new TimeoutException(s"test timed out after $duration")
        ))
      def completeWith(result: util.Try[T]): Unit = {
        onComplete.tryComplete(result)
        timers.clearTimeout(timeoutHandle)
      }
      ec.execute(() =>
        try startFuture().onComplete(completeWith)(ec)
        catch { case NonFatal(ex) => completeWith(util.Failure(ex)) }
      )
      onComplete.future
    }

  def setTimeout(ms: Int)(body: => Unit): () => Unit = {
    val timeoutHandle = timers.setTimeout(ms)(body)

    () => timers.clearTimeout(timeoutHandle)
  }
}
