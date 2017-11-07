package number_recognition

import org.scalatest.FlatSpec

class ValidNumbersTest extends FlatSpec{

	// Structured Basis: nominal case
	// Good data: min normal config: size of 1
	behavior of "booleanSegmentedNumber"
	it should "test nominal, min normal config" in {
		val segments = List.fill(1)(3)
		val number = ValidNumbers.booleanSegmentedNumber(segments)
		assert(number == List(false, false, true, false, false, false, false))
	}

	// Good data: normal config: size of 3
	it should "test with normal configuration" in {
		val segments = List(1,2,3)
		val number = ValidNumbers.booleanSegmentedNumber(segments)
		assert(number == List(true, true,true, false, false, false, false))
	}

	// Good data: max normal config: size of 7
	it should "test with max normal configuration" in {
		val segments = List(1, 2, 3, 4, 5, 6, 7)
		val number = ValidNumbers.booleanSegmentedNumber(segments)
		assert(number == List.fill(7)(true))
	}

	// Structured basis: The for loop is false
	// Bad data: Nil list
	it should "test with a Nil list, first for loop is false" in {
		val segments = Nil
		val number = ValidNumbers.booleanSegmentedNumber(Nil)
		assert(number == List.fill(7)(false))
	}
}
