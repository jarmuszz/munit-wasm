package munit

import scala.concurrent.Promise

class PleaseWorkSuite extends FunSuite {
  test("pls") {
    val p = Promise[Unit]()
    p.future
  }
}
