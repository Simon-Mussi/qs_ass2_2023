package at.tugraz.ist.qs2023

import at.tugraz.ist.qs2023.simple.SimpleFunctions._
//import at.tugraz.ist.qs2023.simple.SimpleFunctionsMutant1._
//import at.tugraz.ist.qs2023.simple.SimpleFunctionsMutant2._
//import at.tugraz.ist.qs2023.simple.SimpleFunctionsMutant3._
//import at.tugraz.ist.qs2023.simple.SimpleFunctionsMutant4._
import at.tugraz.ist.qs2023.simple.SimpleJavaFunctions
import org.junit.runner.RunWith
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

// Consult the following scalacheck documentation
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#concepts
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#generators

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class SimpleFunctionsTest extends Properties("SimpleFunctionsTest") {

  // Gen is some sort of function from scala check,
  // it is responsible to provide you random generated test data
  private val nonEmptyIntListGen: Gen[List[Int]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])

  // insertionSort Java style
  property("insertionSort Java: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = SimpleJavaFunctions.insertionSort(xs.toArray)
    var correctFlag = true;
    if (xs.nonEmpty) {
      for (i <- 0 until sorted.length - 1) {
        if (sorted(i) > sorted(i + 1))
          correctFlag = false;
      }
      correctFlag // would be the return val
    }
    else
      false // returns false if xs is empty
  }

  // insertionSort the beautiful scala way
  property("insertionSort: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = insertionSort(xs)
    xs.nonEmpty ==> xs.indices.tail.forall((i: Int) => sorted(i - 1) <= sorted(i))
  }
  property("insertionSort: permutation") = forAll { (xs: List[Int]) =>
    val sorted = insertionSort(xs)

    def count(a: Int, as: List[Int]) = as.count(_ == a)

    xs.forall((x: Int) => count(x, xs) == count(x, sorted))
  }

  // check if other properties -> look in slides
  // mutation testing > 90%
  // maximum
  // really find the max value
  // list not empty -> false
  // keep list in same order as called
  // TODO: max() properties
  property("max Java: largest value") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val maxVal = SimpleJavaFunctions.max(xs.toArray)
    var correctFlag = true;
    if (xs.nonEmpty) {
      for (i <- 0 until xs.length) {
        if (xs(i) > maxVal)
          correctFlag = false;
      }
      correctFlag // would be the return val
    }
    else
      false // returns false if xs is empty
  }

  property("max: largest value") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val maxVal = max(xs)
    xs.forall(value => value <= maxVal)
  }

  property("max: max value present in list") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val maxVal = max(xs)
    xs.contains(maxVal)
  }


  // minimal index
  // smallest element -> index
  // non empty -> false
  // not out of bounds -> precond
  // TODO: minIndex() properties
  property("minIndex Java: smallest element with index") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val minIndex = SimpleJavaFunctions.minIndex(xs.toArray)
    var correctFlag = true;
    if (xs.nonEmpty) {
      for (i <- 0 until xs.length) {
        if (i != minIndex && xs(i) < xs(minIndex))
          correctFlag = false;
      }
      correctFlag // would be the return val
    }
    else
      false // returns false if xs is empty
  }


  property("min index: smallest element with index") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val index = minIndex(xs)
    xs.indices.forall((i: Int) => i == index || xs(index) <= xs(i))
  }

  property("minIndex: always non-negative") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    minIndex(xs) >= 0
  }

  property("minIndex: index within bounds") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val index = minIndex(xs)
    index >= 0 && index < xs.length
  }


  // symmetric difference
  // check if distinct
  // return unique elements
  // return set of unique elements -> not in both
  // returns length <= sum of lists
  // TODO: symmetricDifference() properties
  property("symetricDifference: check if in List") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = symmetricDifference(xs, ys)
    set.forall(setVal => (xs.contains(setVal) && !ys.contains(setVal)) || (!xs.contains(setVal) && ys.contains(setVal)))
  }

  property("symmetricDifference: size less than or equal to sum of input sizes") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = symmetricDifference(xs, ys)
    set.size <= xs.size + ys.size
  }

  property("symmetricDifference: no duplicates") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = symmetricDifference(xs, ys)
    set.distinct == set
  }

  // intersection
  // returns set of elements in both lists -> <= smallest list
  // check if both are distinct/sets
  // TODO: intersection() properties
  property("intersection: check all possibilities") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = intersection(xs, ys)
    (xs intersect ys).forall(set.contains)
  }

  property("intersection: size less than or equal to smallest input size") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = intersection(xs, ys)
    set.size <= xs.size.min(ys.size)
  }

  property("intersection: no duplicates") = forAll { (xs: List[Int], ys: List[Int]) =>
    val set = intersection(xs, ys)
    set.distinct == set
  }

  // Smallest missing positive integer
  // check if > 0
  // empty list -> 1
  // values smaller zero -> 1
  // integer <= len(list) + 1
  // TODO: smallestMissingPositiveInteger() properties
  property("smallestMissingPositiveInteger: smallest correct Int") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val smallestMissInt = smallestMissingPositiveInteger(xs)
    smallestMissInt == (1 to xs.length+2).find(!xs.contains(_)).getOrElse(xs.length+2)
  }

  property("smallestMissingPositiveInteger: always greater than 0") = forAll { (xs: List[Int]) =>
    smallestMissingPositiveInteger(xs) > 0
  }

  property("smallestMissingPositiveInteger: not in original list") = forAll { (xs: List[Int]) =>
    !xs.contains(smallestMissingPositiveInteger(xs))
  }

}
