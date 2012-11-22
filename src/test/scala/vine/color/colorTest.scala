package vine.color

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class colorTest extends FunSuite {

  test(""""transparent" = transparent white""") {
    assert(parse("transparent") == white.transparent)
  }
  test(""""#fff" = white""") {
    assert(parse("#fff") == white)
  }
  test(""""#ffffff" = white""") {
    assert(parse("#ffffff") == white)
  }
  test(""""#000" = black""") {
    assert(parse("#000") == black)
  }
  test(""""#000000" = black""") {
    assert(parse("#000000") == black)
  }
  test(""""#fff0" = transparent white""") {
    assert(parse("#fff0") == white.transparent)
  }
  test(""""#ffffff00" = transparent white""") {
    assert(parse("#ffffff00") == white.transparent)
  }
  test(""""#0000" = transparent black""") {
    assert(parse("#0000") == black.transparent)
  }
  test(""""#00000000" = transparent black""") {
    assert(parse("#00000000") == black.transparent)
  }
  test(""""#3333" = "#33333333"""") {
    assert(parse("#3333") == parse("#33333333"))
  }

}
