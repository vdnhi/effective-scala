package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

class HeapProperties(heapInterface: HeapInterface) extends Properties("Heap"):
  
  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*


  // Examples of properties
  property("inserting the minimal element and then finding it should return the same minimal element") =
    forAll { (heap: List[Node]) =>
      val min = if isEmpty(heap) then 0 else findMin(heap)
      findMin(insert(min, heap)) == min
    }

  property("the minimum of a heap of two elements should be the smallest of the two elements") =
    forAll { (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val min: Int = Math.min(x1, x2)
      findMin(heap) == min
    }

  property("delete minumum of heap of one element should return an empty heap") =
    forAll { (x: Int) =>
      // create a heap with exactly one element, `x`
      val heap1: List[Node] = heapInterface.insert(x, heapInterface.empty)
      // delete the minimal element from it
      val heap0: List[Node] = heapInterface.deleteMin(heap1)
      // check that heap0 is empty
      heap0.isEmpty
    }

  property("continually finding and deleting the minimal element of a heap should return a sorted sequence") =
    // recursively traverse the heap
    def check(heap: List[Node]): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then
        true
      else
        // find the minimal element
        val x1: Int = heapInterface.findMin(heap)
        // delete the minimal element of `heap`
        val heap2: List[Node] = heapInterface.deleteMin(heap)
        // find the minimal element in `heap2`
        val x2: Int = heapInterface.findMin(heap2)
        // check that the deleted element is smaller than the minimal element
        // of the remaining heap, and that the remaining heap verifies the
        // same property (by recursively calling `check`)
        val checked: Boolean = x1 <= x2
        checked
    // check arbitrary heaps
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  // TODO Write more properties here to detect the bugs
  // in bogus BinomialHeap implementations
  property("given an empty heap and a list of integers, insert all to heap and repeatly finding and deleting return the intial sequence of intergers") =
    def check(seq: Seq[Int], heap: List[Node]): Boolean =
      if heap.isEmpty && seq.isEmpty then
        true
      else if heap.isEmpty || seq.isEmpty then
        false
      else
        val heapElement = heapInterface.findMin(heap)
        seq.head == heapElement && check(seq.tail, heapInterface.deleteMin(heap))

    forAll { (seq: Seq[Int]) =>
      var heap = heapInterface.empty
      seq.foreach(item => heap = heapInterface.insert(item, heap))
      check(seq.sortBy(_.self), heap)
    }

  property("finding a minimum of melding of any two heaps should return a minimum of one or the other") =
    forAll(genHeap.suchThat(_.nonEmpty)) { (firstHeap: List[Node]) =>
      forAll(genHeap.suchThat(_.nonEmpty)) { (secondHeap: List[Node]) =>
        val firstMin = heapInterface.findMin(firstHeap)
        val secondMin = heapInterface.findMin(secondHeap)
        val meldedHeap = heapInterface.meld(firstHeap, secondHeap)
        heapInterface.findMin(meldedHeap) == Math.min(firstMin, secondMin)
      }
    }

  // random heap generator --- DO NOT MODIFY
  private lazy val genHeap: Gen[List[Node]] = oneOf(const(empty),
    for
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(v, h)
  )

  private given Arbitrary[List[Node]] = Arbitrary(genHeap)
  
end HeapProperties
