package chapter2

import org.scalameter.api._
import scala.util.Random

object SortBenchmark extends PerformanceTest.OfflineReport {
  val rand = new Random(0)
  val sizes = Gen.range("size")(500, 5000, 500)

  val sortedArrays = for (size <- sizes) yield Array.range(1, size)
  val reverseArrays = for (size <- sizes) yield Array.range(size, 1, -1)
  val randomArrays = for (size <- sizes) yield Array.fill(size)(rand.nextInt)

  val sortedLists = sortedArrays map (_.toList)
  val reverseLists = reverseArrays map (_.toList)
  val randomLists = randomArrays map (_.toList)

  performance of "Sorting" config (verbose -> false) in {
    performance of "Sorted" in {
      measure method "Selection Sort" in {
        using(sortedArrays) in {
          A => IterativeSelectionSort.selectionSort(A)
        }
      }
      measure method "Recursive Selection Sort" in {
        using(sortedArrays) in {
          A => RecursiveSelectionSort.recSelectionSort(A)
        }
      }
      measure method "Merge Sort" in {
        using(sortedLists) in {
          L => MergeSortImmutableList.mergeSort(L)
        }
      }
    }

    performance of "Reverse" in {
      measure method "Selection Sort" in {
        using(reverseArrays) in {
          A => IterativeSelectionSort.selectionSort(A)
        }
      }
      measure method "Recursive Selection Sort" in {
        using(reverseArrays) in {
          A => RecursiveSelectionSort.recSelectionSort(A)
        }
      }
      measure method "Merge Sort" in {
        using(reverseLists) in {
          L => MergeSortImmutableList.mergeSort(L)
        }
      }
    }

    performance of "Random" in {
      measure method "Selection Sort" in {
        using(randomArrays) in {
          A => IterativeSelectionSort.selectionSort(A)
        }
      }
      measure method "Recursive Selection Sort" in {
        using(randomArrays) in {
          A => RecursiveSelectionSort.recSelectionSort(A)
        }
      }
      measure method "Merge Sort" in {
        using(randomLists) in {
          L => MergeSortImmutableList.mergeSort(L)
        }
      }
    }
  }
}