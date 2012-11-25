package vine.collection

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class circularSetTest extends FunSuite {

  test("Empty set") {
    val x = new CircularSet[String]
    assert(x.size == 0)
    assert(!x.iterator.hasNext)
  }

  test("Insert one element") {
    val x = new CircularSet[Int]
    x.insert(4)
    assert(x.size == 1)
    assert(x.iterator.next == 4)
    assert(x.iterator(4).next == 4)
  }

  test("Insert two elements") {
    val x = new CircularSet[Int]
    x.insert(4, 6)
    assert(x.size == 2)
    val it = x.iterator(6)
    assert(it.next == 6)
    assert(it.next == 4)
    assert(!it.hasNext)
  }

  test("Insert three elements") {
    val x = new CircularSet[Int]
    x.insert(4, 6, 8)
    assert(x.size == 3)
    val it = x.iterator(6)
    assert(it.next == 6)
    assert(it.next == 8)
    assert(it.next == 4)
    assert(!it.hasNext)
  }

  test("Insert two then one (1)") {
    val x = new CircularSet[Int]
    x.insert(4, 6)
    x.insert(4,5,6)
    val it = x.iterator(4)
    assert(it.next == 4)
    assert(it.next == 5)
    assert(it.next == 6)
    assert(!it.hasNext)
  }

  test("Insert two then one (2)") {
    val x = new CircularSet[Int]
    x.insert(4, 6)
    x.insert(6, 5, 4)
    val it = x.iterator(4)
    assert(it.next == 4)
    assert(it.next == 6)
    assert(it.next == 5)
    assert(!it.hasNext)
  }

}
