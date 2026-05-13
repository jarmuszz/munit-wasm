package munit

import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.scalajs.js.timers._
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.WasmComponent

class TimeoutSuite extends BaseSuite {
  override def munitTimeout: Duration = Duration(3, "ms")

  def wasiIngore(opts: TestOptions): TestOptions = 
    linkTimeIf(moduleKind == WasmComponent)(opts.ignore)(opts)

  test(wasiIngore("setTimeout-exceeds".fail)) {
    linkTimeIf(moduleKind == WasmComponent)() {
      val promise = Promise[Unit]()
      setTimeout(1000)(promise.success(()))
      promise.future
    }
  }

  test(wasiIngore("setTimeout-passes")) {
    linkTimeIf(moduleKind == WasmComponent)() {
      val promise = Promise[Unit]()
      setTimeout(1)(promise.success(()))
      promise.future
    }
  }

  // We can't use an infinite loop because it blocks the main thread preventing the test from completing.
  //   test("infinite-loop".fail) {
  //     ThrottleCpu.run()
  //   }
}
