package vine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import vine.color._

@RunWith(classOf[JUnitRunner])
class colorTest extends FunSuite {

  test(""""transparent" = transparent white""") {
    assert(color("#fff") == color.white)
  }
  test(""""#fff" = white""") {
    assert(color("#fff") == color.white)
  }
  test(""""#ffffff" = white""") {
    assert(color("#ffffff") == color.white)
  }
  test(""""#000" = black""") {
    assert(color("#000") == color.black)
  }
  test(""""#000000" = black""") {
    assert(color("#000000") == color.black)
  }
  test(""""#fff0" = transparent white""") {
    assert(color("#fff0") == color.white.transparent)
  }
  test(""""#ffffff00" = transparent white""") {
    assert(color("#ffffff00") == color.white.transparent)
  }
  test(""""#0000" = transparent black""") {
    assert(color("#0000") == color.black.transparent)
  }
  test(""""#00000000" = transparent black""") {
    assert(color("#00000000") == color.black.transparent)
  }
  test(""""#3333" = "#33333333"""") {
    assert(color("#3333") == color("#33333333"))
  }

}
