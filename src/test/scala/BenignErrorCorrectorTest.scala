package number_recognition

import org.scalatest.FlatSpec

class BenignErrorCorrectorTest extends FlatSpec{

	// Structured basis: nominal case
	// Bad data: lines have invalid characters in them
	behavior of "correct"
	it should "test nominally" in {
		val lines = Array("test".toCharArray())
		val result = BenignErrorCorrector.instance.correct(lines)

		assert(result.head.mkString("") == "    ")
	}

	// Structured basis: If statement false
	it should "test with all valid data, if statement false" in {
		val lines = Array("||||".toCharArray())
		val result = BenignErrorCorrector.instance.correct(lines)

		assert(result.head.mkString("") == "||||")
	}

}
