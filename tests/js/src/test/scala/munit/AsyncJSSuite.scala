package munit

import scala.concurrent.Promise
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.WasmComponent

class AsyncJSSuite extends FunSuite {
  def wasiIngore(opts: TestOptions): TestOptions = 
    linkTimeIf(moduleKind == WasmComponent)(opts.ignore)(opts)

  test(wasiIngore("async-ok")) {
    linkTimeIf(moduleKind == WasmComponent)(()) {
      val p = Promise[Unit]()
      scala.scalajs.js.timers.setTimeout(100)(p.success(()))
      p.future
    }
  }

  test(wasiIngore("async-error".fail)) {
    linkTimeIf(moduleKind == WasmComponent)(()) {
      val p = Promise[Unit]()
      scala.scalajs.js.timers
        .setTimeout(100)(p.failure(new RuntimeException("boom")))
      p.future
    }
  }
}
