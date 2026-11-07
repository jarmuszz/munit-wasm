package munit.internal

import munit.MUnitRunner

import sbt.testing.{EventHandler, Logger, Task, TaskDef}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Awaitable, ExecutionContext, Future}
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import scala.scalajs.reflect.Reflect

private[internal] abstract class PlatformCompatImpl {
  def nanoTime(): Long

  def currentTimeMillis(): Long

  def print(s: String): Unit

  def println(s: String): Unit

  def printStackTrace(t: Throwable): Unit

  val executionContext: ExecutionContext

  def awaitResult[T](awaitable: Awaitable[T]): T

  def sleep(duration: FiniteDuration): Unit

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

object PlatformCompat {
  private val impl: PlatformCompatImpl =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiPlatformCompat: PlatformCompatImpl
    } {
      JSPlatformCompat: PlatformCompatImpl
    }

  def nanoTime(): Long = impl.nanoTime()

  def print(msg: String): Unit = impl.print(msg)

  def println(msg: String): Unit = impl.println(msg)

  def printStackTrace(t: Throwable): Unit = impl.printStackTrace(t)

  val executionContext: ExecutionContext =
    impl.executionContext

  def awaitResult[T](awaitable: Awaitable[T]): T =
    impl.awaitResult(awaitable)

  def sleep(duration: FiniteDuration): Unit =
    impl.sleep(duration)

  def executeAsync(
      task: Task,
      eventHandler: EventHandler,
      loggers: Array[Logger],
  ): Future[Unit] =
    impl.executeAsync(task, eventHandler, loggers)

  def waitAtMost[T](
      startFuture: () => Future[T],
      duration: Duration,
      ec: ExecutionContext,
  ): Future[T] =
    impl.waitAtMost(startFuture, duration, ec)

  def setTimeout(ms: Int)(body: => Unit): () => Unit =
    impl.setTimeout(ms)(body)

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
