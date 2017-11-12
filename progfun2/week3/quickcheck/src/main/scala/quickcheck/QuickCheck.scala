package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOf2") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(insert(a - 1, h)) == a - 1
  }

  property("mergeEmpty") = forAll { h: H =>
    meld(h, empty) == h
  }

  property("deleteMin") = forAll { (h: H) =>
    def _del(h: H, list: List[Int]): List[Int] = if (isEmpty(h)) list else {
      val m = findMin(h)
      _del(deleteMin(h), list ++ List(m))
    }

    val list = _del(h, Nil)
    list == list.sorted
  }
}
