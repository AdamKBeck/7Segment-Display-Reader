package number_recognition

import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import scala.collection.mutable.ListBuffer

class NumberConstructorTest extends FlatSpec with PrivateMethodTester {

	// Structured basis: nominal case
	// Good data: min normal config
	behavior of "constructNumbers"
	it should "test nominally, min normal config" in {
		Thread.sleep(100) // The other test classes interfere with flagging severeError
		Logger.instance.severeError = false

		val parsedNumbers: List[ParsedNumber] = List(
			ParsedNumber(List(4))
		)

		val constructNumbersFrom = PrivateMethod[Either[List[Int], String]]('constructNumbersFrom)
		val result = NumberConstructor.instance.invokePrivate(constructNumbersFrom(parsedNumbers))

		assert(result == Right("ambiguous"))
	}

	// Structured basis: for statement false
	// Bad data: Nil list of parsedNumbers
	it should "test Nil list of parsedNumbers" in {
		Thread.sleep(150) // The other test classes interfere with flagging severeError
		Logger.instance.severeError = false

		val constructNumbersFrom = PrivateMethod[Either[List[Int], String]]('constructNumbersFrom)
		val result = NumberConstructor.instance.invokePrivate(constructNumbersFrom(Nil))

		assert(result == Left(List()))
	}

	// Structured basis: first if statement false
	// Good data: normal config: 4 numbers
	it should "test for a valid list of numbers, normal config" in {
		Thread.sleep(250) // The other test classes interfere with flagging severeError
		Logger.instance.severeError = false

		val parsedNumbers: List[ParsedNumber] = List(
			ParsedNumber(List(4,7)),
			ParsedNumber(List(4,7)),
			ParsedNumber(List(4,7)),
			ParsedNumber(List(4,7))
		)

		val constructNumbersFrom = PrivateMethod[Either[List[Int], String]]('constructNumbersFrom)
		val result = NumberConstructor.instance.invokePrivate(constructNumbersFrom(parsedNumbers))

		assert(result == Left(List(1,1,1,1)))
	}

	// Structured basis: second if statement false
	// Boundary: x = 1
	it should "test for a valid list a non-ambiguous garbled number, max config" in {
		Thread.sleep(300) // The other test classes interfere with flagging severeError
		Logger.instance.severeError = false

		val parsedNumbers: List[ParsedNumber] = List(
			ParsedNumber(List(2, 3, 4, 5, 6, 7))
		)

		val constructNumbersFrom = PrivateMethod[Either[List[Int], String]]('constructNumbersFrom)
		val result = NumberConstructor.instance.invokePrivate(constructNumbersFrom(parsedNumbers))

		assert(result == Left(List(8)))
	}

	// Good data: max config of 7 parsed numbers
	// Boundary: x > 1
	it should "test for a large list" in {
		Logger.instance.severeError = false

		val parsedNumbers: List[ParsedNumber] = List(
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7)),
			ParsedNumber(List(2, 3, 4, 5, 6, 7))
		)

		val constructNumbersFrom = PrivateMethod[Either[List[Int], String]]('constructNumbersFrom)
		val result = NumberConstructor.instance.invokePrivate(constructNumbersFrom(parsedNumbers))

		assert(result == Right("failure"))
	}

	// Structered basis: nominal case
	// Bad data: number is all false
	// Min normal config: list of size 1
	behavior of "logIfEmpty"
	it should "test nominally, all false number, min normal config" in {
		val logIfEmpty = PrivateMethod[Unit]('logIfEmpty)
		NumberConstructor.instance.invokePrivate(logIfEmpty(List(false)))

		assert(Logger.instance.severeError)
	}

	// Structered basis: if statement is true
	// Normal/max config: A list of size 7
	it should "test an all true list of size 7, normal/max config" in {
		Thread.sleep(40)
		val logIfEmpty = PrivateMethod[Unit]('logIfEmpty)
		NumberConstructor.instance.invokePrivate(logIfEmpty(List(true, true, true, true, true, true, true)))

		assert(Logger.instance.severeError)
	}

	// Bad data: empty list
	it should "test for an empty list" in{
	Logger.instance.severeError = false
		Thread.sleep(40)
		val logIfEmpty = PrivateMethod[Unit]('logIfEmpty)
		NumberConstructor.instance.invokePrivate(logIfEmpty(List()))

		assert(Logger.instance.severeError)
	}

	// Structured basis
	// Good data: min normal config: non null lists of size 1
	behavior of "isSegmentSubset"
	it should "test nominally, min normal config" in {
		val isSegmentSubset = PrivateMethod[Boolean]('isSegmentSubset)
		val result = NumberConstructor.instance.invokePrivate(isSegmentSubset(List(true), List(false)))
		assert(!result)
	}

	// Good data:  normal/max config: non null lists of size 7
	it should "test normal/max config" in {
		val isSegmentSubset = PrivateMethod[Boolean]('isSegmentSubset)
		val result = NumberConstructor.instance.invokePrivate(isSegmentSubset(
			List(true, true, true, true, true, true, true),
			List(false, false, false, false, false, false, false)))
		assert(!result)
	}

	// Structured basis, for loop is false
	// Bad data, parsed number is Nil
	it should "test parsed number is Nil, for loop is false" in {
		val isSegmentSubset = PrivateMethod[Boolean]('isSegmentSubset)
		val result = NumberConstructor.instance.invokePrivate(isSegmentSubset(
			Nil,
			List(false, false, false, false, false, false, false)))
		assert(result)
	}

	// Structured basis, if statement is false (first boolean condition)
	it should "test a subset, first boolean condition is false" in {
		val isSegmentSubset = PrivateMethod[Boolean]('isSegmentSubset)
		val result = NumberConstructor.instance.invokePrivate(isSegmentSubset(
			List(false, false),
			List(false, false)))
		assert(result)
	}

	// Structured basis, if statement false (second boolean condition)
	it should "test a valid number that's all true" in {
		val isSegmentSubset = PrivateMethod[Boolean]('isSegmentSubset)
		val result = NumberConstructor.instance.invokePrivate(isSegmentSubset(
			List(false, false),
			List(true, true)))
		assert(result)
	}


	// Structured basis, nominal test
	// Bad data: Nil listbuffer
	// Good data: min/normal/max configs for paramater values
	behavior of "opticalRecognitionOutcome"
	it should "test nominally, Nil listbuffer, other paramater values are of Good data" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(true, 0, 0, ListBuffer[Int]()))

		assert(result == Right("failure"))
	}

	// Structured basis, first if statement false
	// Boundary: defectedCount >= 2
	it should "test non severe error, second if statement false, upper boundary" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(false, 3, 0, ListBuffer[Int]()))

		assert(result == Right("failure"))
	}

	// Boundary: defectedCount == 2
	it should "test non severe error, second if statement false, equal boundary" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(false, 2, 0, ListBuffer[Int]()))

		assert(result == Right("failure"))
	}

	// Structured basis, second if statement false
	// Boundary: defectedCount < 2
	// Boundary: ambiguousCount == 1
	it should "test non severe error, second if statement false, lesser < 2" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(false, 1, 1, ListBuffer[Int]()))

		assert(result == Right("ambiguous"))
	}

	// Structured basis, third if statement is false
	// Boundary: ambiguousCount > 1
	it should "test non severe error, third if statement false, greter boundary" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(false, 1, 2, ListBuffer[Int]()))

		assert(result == Left(Nil))
	}

	// Structured basis, third if statement is false
	// Boundary: ambiguousCount < 1
	it should "test non severe error, third if statement false, leser boundary" in {
		val opticalRecognitionOutcome = PrivateMethod[Either[List[Int], String]]('opticalRecognitionOutcome)
		val result = NumberConstructor.instance.invokePrivate(opticalRecognitionOutcome(false, 1, 0, ListBuffer[Int]()))

		assert(result == Left(Nil))
	}
}
