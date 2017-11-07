package number_recognition

import org.scalatest.FlatSpec

class ValidNumbersTest extends FlatSpec{

	// Structured Basis: nominal case
	// Good data: min normal config
	behavior of "booleanSegmentedNumber"
	it should "test nominal, min normal config" in {
		val segments = List.fill(7)(3)
		val number = ValidNumbers.booleanSegmentedNumber(segments)
		assert(number == List(false, false, true, false, false, false, false))
	}
	

}
