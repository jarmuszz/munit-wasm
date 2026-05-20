package munit

import scala.concurrent.Promise
import scala.concurrent.duration._

class PleaseWorkSuite extends FunSuite {

  override def munitTimeout: Duration = 5.seconds

  test("pls") {
    val p = Promise[Unit]()
    p.future
  }
}
