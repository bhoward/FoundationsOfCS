package chapter5

import scala.collection.mutable.ArrayBuffer
import java.util.Scanner

// Based on Figure 5.54 of Aho & Ullman, except it's a min-heap
object BinaryHeap {
  class Heap {
    private val A = ArrayBuffer(Int.MinValue) // sentinel at index 0

    private def swap(i: Int, j: Int): Unit = {
      val temp = A(i)
      A(i) = A(j)
      A(j) = temp
    }

    private def bubbleUp(i: Int): Unit = {
      if (i > 1) { // not at root
        val parent = i / 2
        if (A(i) < A(parent)) {
          swap(i, parent)
          bubbleUp(parent)
        }
      }
    }

    private def bubbleDown(i: Int): Unit = {
      val left = 2 * i
      val right = 2 * i + 1
      if (left < A.size) {
        // choose smaller child
        val child = if (right < A.size && A(right) < A(left)) {
          right
        } else {
          left
        }
        if (A(child) < A(i)) {
          swap(i, child)
          bubbleDown(child)
        }
      }
    }

    def insert(x: Int): Unit = {
      A.append(x)
      bubbleUp(A.size - 1)
    }

    def insertAll(xs: List[Int]): Unit = {
      A.appendAll(xs)
      for (i <- (A.size - 1) / 2 to 1 by -1) {
        bubbleDown(i)
      }
    }

    def findMin: Option[Int] = {
      if (A.size > 1) {
        Option(A(1))
      } else {
        None
      }
    }

    def deleteMin(): Unit = {
      if (A.size > 1) {
        swap(1, A.size - 1)
        A.trimEnd(1)
        bubbleDown(1)
      }
    }

    def clear(): Unit = {
      A.clear()
    }
  }

  def removeAll(h: Heap): List[Int] = h.findMin match {
    case None => Nil
    case Some(m) => {
      h.deleteMin()
      m :: removeAll(h)
    }
  }

  def heapSort(xs: List[Int]): List[Int] = {
    val h = new Heap
    h.insertAll(xs)
    removeAll(h)
  }

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val list = makeList(in)
    printList(heapSort(list))
  }

  def makeList(in: Scanner): List[Int] = {
    if (!in.hasNextInt) {
      Nil
    } else {
      val x = in.nextInt()
      x :: makeList(in)
    }
  }

  def printList(list: List[Int]): Unit = {
    for (current <- list) {
      println(current)
    }
  }
}
