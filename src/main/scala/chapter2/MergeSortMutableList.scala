package chapter2

// Based on Figure 2.31 of Aho & Ullman
object MergeSortMutableList {
  import java.util.Scanner

  class LIST(val element: Int, var next: LIST)
  val NULL: LIST = null // scalastyle:ignore

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val list = makeList(in)
    printList(mergeSort(list))
  }

  def makeList(in: Scanner): LIST = {
    if (!in.hasNextInt) {
      NULL
    } else {
      val x = in.nextInt()
      new LIST(x, makeList(in))
    }
  }

  def printList(list: LIST): Unit = {
    var current = list
    while (current != NULL) {
      println(current.element)
      current = current.next
    }
  }

  def mergeSort(list: LIST): LIST = {
    if (list == NULL) {
      list
    } else if (list.next == NULL) {
      list
    } else {
      val secondList = split(list)
      merge(mergeSort(list), mergeSort(secondList))
    }
  }

  def merge(list1: LIST, list2: LIST): LIST = {
    if (list1 == NULL) {
      list2
    } else if (list2 == NULL) {
      list1
    } else if (list1.element <= list2.element) {
      list1.next = merge(list1.next, list2)
      list1
    } else {
      list2.next = merge(list1, list2.next)
      list2
    }
  }

  def split(list: LIST): LIST = {
    if (list == NULL) {
      NULL
    } else if (list.next == NULL) {
      NULL
    } else {
      val secondCell = list.next
      list.next = secondCell.next
      secondCell.next = split(secondCell.next)
      secondCell
    }
  }

  // Convenience methods for testing -- construct from built-in List[Int]
  def makeList(L: List[Int]): LIST = L match {
    case Nil          => NULL
    case head :: tail => new LIST(head, makeList(tail))
  }

  def equalLists(list1: LIST, list2: LIST): Boolean = {
    if (list1 == NULL) {
      list2 == NULL
    } else if (list2 == NULL) {
      false
    } else {
      list1.element == list2.element && equalLists(list1.next, list2.next)
    }
  }
}
