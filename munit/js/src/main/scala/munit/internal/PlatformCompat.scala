package munit.internal

import munit.MUnitRunner

import sbt.testing.{EventHandler, Logger, Task, TaskDef}

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable, ExecutionContext, Future, Promise}
import scala.scalajs.concurrent.QueueExecutionContext.SingleThreadedExecutionContext
import scala.scalajs.js.timers
import scala.scalajs.reflect.Reflect
import scala.scalajs.wasi
import scala.util.control.NonFatal

object WasiPlatformCompat {

  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.QueueExecutionContext()

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] = {
    val ec = scala.concurrent.ExecutionContext.global
    val run: Runnable = () => task.execute(eventHandler, loggers, _ => ())
    ec.execute(run)
    Future.successful(())
  }

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] = {
    val start = System.currentTimeMillis()
    val ec = scala.concurrent.ExecutionContext.global
    val stec = ec.asInstanceOf[SingleThreadedExecutionContext]

    val out = Promise[T]()

    val run: Runnable = () => startFuture().onComplete(out.tryComplete)(ec)
    ec.execute(run)

    while (
      !out.isCompleted &&
      System.currentTimeMillis() <= start + duration.toMillis
    ) stec.loop()

    if (out.isCompleted) out.future
    else out.failure(new TimeoutException(s"test timed out after $duration"))
      .future
  }

  def setTimeout(ms: Int)(body: => Unit): () => Unit = {
    wasi.clocks.monotonic_clock.subscribeDuration(ms.toLong * 10000000).block()
    body

    () => ()
  }
}

object JSPlatformCompat {

  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] = {
    val p = Promise[Unit]()
    task.execute(eventHandler, loggers, _ => p.success(()))
    p.future
  }

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    if (!duration.isFinite) startFuture()
    else {
      println("wha..")
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
object PlatformCompat {
  import scala.scalajs.LinkingInfo.{ModuleKind, linkTimeIf, moduleKind}

  val executionContext: ExecutionContext = linkTimeIf(
    moduleKind == ModuleKind.WasmComponent
  )(WasiPlatformCompat.executionContext)(JSPlatformCompat.executionContext)

  def awaitResult[T](awaitable: Awaitable[T]): T = Await
    .result(awaitable, Duration.Inf)

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] = linkTimeIf(moduleKind == ModuleKind.WasmComponent)(
    WasiPlatformCompat.executeAsync(task, eventHandler, loggers)
  )(JSPlatformCompat.executeAsync(task, eventHandler, loggers))

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] = linkTimeIf(moduleKind == ModuleKind.WasmComponent)(
    WasiPlatformCompat.waitAtMost(startFuture, duration, ec)
  )(JSPlatformCompat.waitAtMost(startFuture, duration, ec))

  def setTimeout(ms: Int)(body: => Unit): () => Unit =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent)(
      WasiPlatformCompat.setTimeout(ms)(body)
    )(JSPlatformCompat.setTimeout(ms)(body))

  // Scala.js does not support looking up annotations at runtime.
  def isIgnoreSuite(cls: Class[_]): Boolean = false

  final val isJVM = false
  final val isJS = true
  final val isNative = false

  def newRunner(
      taskDef: TaskDef,
      classLoader: ClassLoader,
  ): Option[MUnitRunner] = Reflect
    .lookupInstantiatableClass(taskDef.fullyQualifiedName()).map(cls =>
      new MUnitRunner(
        cls.runtimeClass.asInstanceOf[Class[_ <: munit.Suite]],
        cls.newInstance().asInstanceOf[munit.Suite],
      )
    )
  private var myClassLoader: ClassLoader = _
  def setThisClassLoader(loader: ClassLoader): Unit = myClassLoader = loader
  def getThisClassLoader: ClassLoader = myClassLoader

  type InvocationTargetException = munit.internal.InvocationTargetException
  type UndeclaredThrowableException =
    munit.internal.UndeclaredThrowableException

  class LogBuffer {
    private val buffer = new scala.collection.mutable.ArrayBuffer[String]
    def append(message: String): Unit = if (message ne null) buffer += message
    def flush(): String =
      try buffer.mkString("\n")
      finally buffer.clear()
  }

}
