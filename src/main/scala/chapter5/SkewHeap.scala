package chapter5

import java.util.Scanner

object SkewHeap {
  sealed trait Heap
  case object Empty extends Heap
  case class Node(left: Heap, value: Int, right: Heap) extends Heap
  
  private def merge(h1: Heap, h2: Heap): Heap = (h1, h2) match {
    case (Empty, _) => h2
    case (_, Empty) => h1
    case (Node(l1, v1, r1), Node(l2, v2, r2)) =>
      if (v1 <= v2) {
        Node(r1, v1, merge(l1, h2))
      } else {
        Node(r2, v2, merge(l2, h1))
      }
  }

  def insert(h: Heap, x: Int): Heap = {
    merge(h, Node(Empty, x, Empty))
  }
  
  
  def insertAll(h: Heap, xs: List[Int]): Heap = xs match {
    case Nil => h
    case head :: tail => insertAll(insert(h, head), tail)
  }

  def findMin(h: Heap): Option[Int] = h match {
    case Empty => None
    case Node(_, m, _) => Option(m)
  }

  def deleteMin(h: Heap): Heap = h match {
    case Empty => Empty
    case Node(l, _, r) => merge(l, r)
  }

  // tail-recursive version
  def removeAll(h: Heap, result: List[Int] = Nil): List[Int] = findMin(h) match {
    case None => result.reverse
    case Some(m) => {
      removeAll(deleteMin(h), m :: result)
    }
  }

  def heapSort(xs: List[Int]): List[Int] = {
    removeAll(insertAll(Empty, xs))
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