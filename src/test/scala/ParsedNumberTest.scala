package number_recognition

import org.scalatest.FlatSpec

class ParsedNumberTest extends FlatSpec {

	// Good data: segments is not empty, min configuration of 1 segment
	behavior of "segments"
	it should "test a normal input, min confuration" in {
		val list = List(1)
		val number = ParsedNumber(list)

		assert(number.segments == List(1))
	}

	// Good data: normal/max configuration: 7 segments
	it should "test normal/max config" in {
		val list = List(9,8,7,6,5,4,3,2,1)
		val number = ParsedNumber(list)

		assert(number.segments == List(1,2,3,4,5,6,7,8,9))
	}

	// Bad data: Nil segments list
	it should "test with a Nil segment list" in {
		val list = Nil
		val number = ParsedNumber(list)

		assert(number.segments == Nil)
	}
}
