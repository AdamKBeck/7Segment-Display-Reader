package number_recognition

import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer
import org.scalatest.PrivateMethodTester

class NumberParserTest extends FlatSpec with PrivateMethodTester{

	// Structured basis, nominal case
	// Good data: normal/max amount: 9 numbers
	behavior of "parsedNumbers"
	it should "test nominally, normal/max amount" in {
		val lines = ListBuffer[Array[Char]]()

		lines += "    _  _     _  _  _  _  _ ".toCharArray
		lines += "  | _| _||_||_ |_   ||_||_|".toCharArray
		lines += "  ||_  _|  | _||_|. ||_| _|".toCharArray

		val parsedNumber = NumberParser.instance.parsedNumbers(lines.toArray)

		assert(parsedNumber.length == 9)

	}

	// Good data: min amount: 1 number
	it should "test min nominal case" in {
		val lines = ListBuffer[Array[Char]]()

		lines += "   ".toCharArray
		lines += "  |".toCharArray
		lines += "  |".toCharArray

		val parsedNumber = NumberParser.instance.parsedNumbers(lines.toArray)

		assert(parsedNumber.length == 1)
	}

	// Bad data: Nil list
	// Structures basis: for statement is false
	it should "test Nil lines" in {
		assertThrows[NoSuchElementException] {
			val parsedNumber = NumberParser.instance.parsedNumbers(Array())
		}
	}

	// Good data: Line is not empty: min,normal configuration
	behavior of "groupedLine"
	it should "test with a nonempty line" in {
		val groupedLine = PrivateMethod[List[Array[Char]]]('groupedLine)

		val result = NumberParser.instance.invokePrivate(groupedLine("abc".toCharArray, 2))

		assert(result(0)(0) == 'a' && result(0)(1) == 'b' && result(1)(0) == 'c')
	}

	// Good data: max configuration: length 100 line
	it should "test with max normal config" in {
		val groupedLine = PrivateMethod[List[Array[Char]]]('groupedLine)
		var str = ListBuffer[Char]()

		for (i <- 1 to 100) {
			str += 'a'
		}

		val line = str.toList.mkString("").toCharArray

		val result = NumberParser.instance.invokePrivate(groupedLine(line, 2))

		assert(result.length == 50)
	}


	// Bad data: Nil line
	it should "test with a Nil line" in {
		val groupedLine = PrivateMethod[List[Array[Char]]]('groupedLine)
		val result = NumberParser.instance.invokePrivate(groupedLine("".toCharArray, 2))
		assert(result == Nil)
	}


	// Structured basis: nominal case
	// Good data: min/normal/max normal case: a valid number
	behavior of "parsedNumber"
	it should "test nominally for min/normal/max case" in {
		val top = Array(' ', ' ', ' ')
		val middle = Array(' ', ' ', '|')
		val bottom = Array(' ', ' ', '|')

		val parsedNumber = PrivateMethod[ParsedNumber]('parsedNumber)
		val result = NumberParser.instance.invokePrivate(parsedNumber(top, middle, bottom))

		assert(result.segments == List(4,7))
	}


	// Structured basis: for statement false
	// Bad data: Nil lists for top, middle, and bottom
	it should "test with all Nil lists" in {
		val top = Array[Char]()
		val middle = Array[Char]()
		val bottom = Array[Char]()

		val parsedNumber = PrivateMethod[ParsedNumber]('parsedNumber)
		val result = NumberParser.instance.invokePrivate(parsedNumber(top, middle, bottom))

		assert(result.segments == List())
	}

	// Structured basis: if statement false
	// Bad data: invalid segment characters in the lists
	it should "test with all invalid segment characters in all lists" in {
		val top = Array[Char]('a','b','c')
		val middle = Array[Char]('a','b','c')
		val bottom = Array[Char]('a','b','c')

		val parsedNumber = PrivateMethod[ParsedNumber]('parsedNumber)
		val result = NumberParser.instance.invokePrivate(parsedNumber(top, middle, bottom))

		assert(result.segments == List())
	}

	// Good data: non empty char
	behavior of "isSegmentSymbolValid"
	it should "test nominally with any char not empty" in {
		val isSegmentSymbolValid = PrivateMethod[Boolean]('isSegmentSymbolValid)
		val result = NumberParser.instance.invokePrivate(isSegmentSymbolValid('|'))

		assert(result)
	}

	// Bad data: empty character
	it should "test with a empty character" in {
		val isSegmentSymbolValid = PrivateMethod[Boolean]('isSegmentSymbolValid)
		val result = NumberParser.instance.invokePrivate(isSegmentSymbolValid(' '))

		assert(!result)
	}

	// Structured basis: if statement false
	// Good data: a non 0 or 2 index, min/normal/max config
	behavior of "checkSegmentPosition"
	it should "test none 0 or 2, if statement false, for a min/normal/max config" in {
		Logger.instance.severeError = false

		val checkSegmentPosition = PrivateMethod[Unit]('checkSegmentPosition)
		NumberParser.instance.invokePrivate(checkSegmentPosition(1))
		assert(!Logger.instance.severeError)
	}

	// Structured basis: nominal case
	it should "test nominally" in {
		Logger.instance.severeError = false

		val checkSegmentPosition = PrivateMethod[Unit]('checkSegmentPosition)
		NumberParser.instance.invokePrivate(checkSegmentPosition(0))
		assert(Logger.instance.severeError)
	}

	// Structured basis: first boolean condition false
	it should "test first boolean condition false, test 1" in {
		Logger.instance.severeError = false

		val checkSegmentPosition = PrivateMethod[Unit]('checkSegmentPosition)
		NumberParser.instance.invokePrivate(checkSegmentPosition(1))
		assert(!Logger.instance.severeError)
	}

	// Structured basis: second boolean condition false
	it should "test second boolean condition false, test 3" in {
		Logger.instance.severeError = false

		val checkSegmentPosition = PrivateMethod[Unit]('checkSegmentPosition)
		NumberParser.instance.invokePrivate(checkSegmentPosition(3))
		assert(!Logger.instance.severeError)
	}


	// Structures basis: nominal case
	// Boundary and Good data: min/normal/max config where x > 2
	behavior of "indexToSegmentNumber"
	it should "test nominally for min/normal/max config" in {
		val indexToSegmentNumber = PrivateMethod[Int]('indexToSegmentNumber)
		val result = NumberParser.instance.invokePrivate(indexToSegmentNumber(4))
		assert(result == 3)
	}

	// Structures basis: if statement false
	// Boundary: x < 2
	it should "test for when index < 2" in {
		val indexToSegmentNumber = PrivateMethod[Int]('indexToSegmentNumber)
		val result = NumberParser.instance.invokePrivate(indexToSegmentNumber(1))
		assert(result == 1)
	}

	// Boundary: x = 2
	it should "test for when index = 2" in {
		val indexToSegmentNumber = PrivateMethod[Int]('indexToSegmentNumber)
		val result = NumberParser.instance.invokePrivate(indexToSegmentNumber(2))
		assert(result == 2)
	}
}
