package chapter5

import org.scalameter.api._
import scala.util.Random

object SortBenchmark extends PerformanceTest.OfflineReport {
  val rand = new Random(0)
  val sizes = Gen.range("size")(500, 5000, 500)

  val sortedLists = for (size <- sizes) yield List.range(1, size)
  val reverseLists = for (size <- sizes) yield List.range(size, 1, -1)
  val randomLists = for (size <- sizes) yield List.fill(size)(rand.nextInt)

  performance of "Sorting" config (verbose -> false) in {
    performance of "Sorted" in {
      measure method "Tree Sort" in {
        using(sortedLists) in {
          L => BinarySearchTree.treeSort(L)
        }
      }
      measure method "AVL Tree Sort" in {
        using(sortedLists) in {
          L => AVLBinarySearchTree.treeSort(L)
        }
      }
      measure method "Binary Heap Sort" in {
        using(sortedLists) in {
          L => BinaryHeap.heapSort(L)
        }
      }
      measure method "Skew Heap Sort" in {
        using(sortedLists) in {
          L => SkewHeap.heapSort(L)
        }
      }
    }

    performance of "Reverse" in {
      measure method "Tree Sort" in {
        using(reverseLists) in {
          L => BinarySearchTree.treeSort(L)
        }
      }
      measure method "AVL Tree Sort" in {
        using(reverseLists) in {
          L => AVLBinarySearchTree.treeSort(L)
        }
      }
      measure method "Binary Heap Sort" in {
        using(reverseLists) in {
          L => BinaryHeap.heapSort(L)
        }
      }
      measure method "Skew Heap Sort" in {
        using(reverseLists) in {
          L => SkewHeap.heapSort(L)
        }
      }
    }

    performance of "Random" in {
      measure method "Tree Sort" in {
        using(randomLists) in {
          L => BinarySearchTree.treeSort(L)
        }
      }
      measure method "AVL Tree Sort" in {
        using(randomLists) in {
          L => AVLBinarySearchTree.treeSort(L)
        }
      }
      measure method "Binary Heap Sort" in {
        using(randomLists) in {
          L => BinaryHeap.heapSort(L)
        }
      }
      measure method "Skew Heap Sort" in {
        using(randomLists) in {
          L => SkewHeap.heapSort(L)
        }
      }
    }
  }
}