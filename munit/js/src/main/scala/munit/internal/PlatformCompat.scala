package munit.internal

import munit.MUnitRunner

import sbt.testing.{EventHandler, Logger, Task, TaskDef}

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Awaitable, ExecutionContext, Future, Promise}
import scala.scalajs.js.timers
import scala.scalajs.reflect.Reflect
import scala.util.control.NonFatal

// io is shadowed by munit.internal.io :/
import _root_.io.github.scalawasm.wasi4s.wasi.clocks

private sealed abstract class PlatformCompatSchema {
  def nanoTime(): Long
  def println(msg: String): Unit

  def printStackTrace(t: Throwable): Unit

  val executionContext: ExecutionContext

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit]

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T]

  def setTimeout(ms: Int)(body: => Unit): () => Unit
}

private object WasiPlatformCompat extends PlatformCompatSchema {

  def nanoTime() = WasmSystem.nanoTime()
  def println(msg: String) = WasmSystem.print(msg + '\n', false)

  def printStackTrace(t: Throwable) =
    this.println(s"${t}: Stacktraces are not available on pure WebAssembly builds")

  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.QueueExecutionContext()

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
    clocks.monotonic_clock.subscribeDuration(ms.toLong * 10000000).block()
    body

    () => ()
  }
}

private object JSPlatformCompat extends PlatformCompatSchema {

  def nanoTime(): Long = System.nanoTime()
  def println(msg: String) = scala.Console.println(msg)
  def printStackTrace(t: Throwable) = t.printStackTrace()

  val executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  def awaitResult[T](awaitable: Awaitable[T]): T = Await
    .result(awaitable, Duration.Inf)

  // Scala.js is single threaded, so we can't block.
  def sleep(duration: FiniteDuration): Unit = ()

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
  import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}

  private val factory: PlatformCompatSchema =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiPlatformCompat: PlatformCompatSchema
    } {
      JSPlatformCompat
    }

  def nanoTime() =
    factory.nanoTime()

  def println(msg: String) =
    factory.println(msg)

  def printStackTrace(t: Throwable) =
    factory.printStackTrace(t)

  val executionContext: ExecutionContext =
    factory.executionContext

  def awaitResult[T](awaitable: Awaitable[T]): T = Await
    .result(awaitable, Duration.Inf)

  // Scala.js is single threaded, so we can't block.
  def sleep(duration: FiniteDuration): Unit = ()

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] =
    factory.executeAsync(task, eventHandler, loggers)

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    factory.waitAtMost(startFuture, duration, ec)

  def setTimeout(ms: Int)(body: => Unit): () => Unit =
    factory.setTimeout(ms)(body)

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
