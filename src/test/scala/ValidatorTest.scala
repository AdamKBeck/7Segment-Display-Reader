package number_recognition

import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import java.nio.file._
import scala.collection.mutable.ListBuffer

class ValidatorTest extends FlatSpec with PrivateMethodTester{

	// Structured Basis: nominal case
	// Good data: min normal config: empty but created file
	behavior of "lines"
	it should "test nominally, min normal config" in {
		Files.deleteIfExists(Paths.get("src/testFile.txt"))
		Logger.instance.createFile(List(), "testFile.txt")
		val lines = Validator.instance.lines("testFile.txt")
		Files.deleteIfExists(Paths.get("src/testFile.txt"))

		assert(lines.length == 0)
	}

	// Good data: normal config: The file contains a 2 lines
	it should "test normal config" in {
		Files.deleteIfExists(Paths.get("src/testFile.txt"))
		Logger.instance.createFile(List("line1", "line2"), "testFile.txt")
		val lines = Validator.instance.lines("testFile.txt")
		Files.deleteIfExists(Paths.get("src/testFile.txt"))

		assert(lines.length == 2)
	}

	// Good data: max normal config: The file contains 4 lines
	it should "test max config" in {
		Files.deleteIfExists(Paths.get("src/testFile.txt"))
		Logger.instance.createFile(List("line1", "line2", "line3", "line4"), "testFile.txt")
		val lines = Validator.instance.lines("testFile.txt")
		Files.deleteIfExists(Paths.get("src/testFile.txt"))

		assert(lines.length == 4)
	}

	// Structured Basis: First if statement is false
	// Bad data: No file is created
	it should "no file exists, first if statment is false" in {
		val lines = Validator.instance.lines("thisFileDoesntExist.txt")
		Files.deleteIfExists(Paths.get("src/testFile.txt"))
		assert(lines.length == 0)
	}

	// Structured Basis: nominal case
	// Good data: min normal config: 3 lines
	behavior of "benign lines"
	it should "test nominally, min normal config" in {
		val lines = Array(Array('t'), Array('|'), Array('_'))
		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines(0)(0) == ' ' &&
			benignLines(1)(0) == '|' &&
			benignLines(2)(0) == '_')
	}

	// Good data: normal config, max normal config: 3 decently filled lines
	it should "test normal config, max normal config" in {
		val firstLine = "test"
		val secondLine = "another test"
		val thirdLine = "yet another test"
		val lines = Array(firstLine.toCharArray, secondLine.toCharArray,
			thirdLine.toCharArray)

		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines.length == 3)
	}

	// Bad data: line length is 2
	// Structured Basis: first if statement false
	it should "test line length is 2, first if statment is false" in {
		val lines = Array("test".toCharArray)
		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines.length == 0)
	}

	// Structured Basis: second boolean condition false, first part false
	it should "test 4 lines, second boolean condition false, first part false" in {
		val lines = Array("test".toCharArray, "test".toCharArray, "test".toCharArray,
			"test".toCharArray(), "test".toCharArray)
		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines.length == 0)
	}

	// Structured Basis: second boolean condition false, second part false
	it should "test 4 lines, 4th line nonempty, second boolean condition false, second part false" in {
		val lines = Array("test".toCharArray, "test".toCharArray, "test".toCharArray,
			"test".toCharArray(), "test".toCharArray)
		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines.length == 0)
	}

	// Structured Basis: first boolean condition false
	it should "test 4 lines, first boolean condition false" in {
		val lines = Array("test".toCharArray, "test".toCharArray, "test".toCharArray,
			 "".toCharArray)
		val benignLines = Validator.instance.benignLines(lines)
		assert(benignLines.length == 4)
	}


	// Structured Basis: nominal case
	// Good data: min/normal/max normal config: non empty lines of width 27
	behavior of "parsedNumbers"
	it should "test nominally, min/normal/max config " in {
		val lines = Array(
		"    _  _  _  _  _  _     _ ".toCharArray,
		"|_||_|| || ||_   |  |  ||  ".toCharArray,
		"  | _||_||_||_|  |  |  | _|".toCharArray
		)

		val parsedNumbers = Validator.instance.parsedNumbers(lines)

		val firstNumber = parsedNumbers.head

		assert(firstNumber.segments == List(2,3,4,7))
	}

	// Structured basis: first if statement false
	// Bad data: empty lines
	it should "test empty lines, first if statement false" in {
		val lines = Array[Array[Char]]()
		val pasedNumbers = Validator.instance.parsedNumbers(lines)
		assert(pasedNumbers.isEmpty)
	}

	// Structured basis: second if statement false
	// Bad data: invalid width lines
	it should "test lines of invalid width" in {
		val lines = Array("test".toCharArray)
		val pasedNumbers = Validator.instance.parsedNumbers(lines)
		assert(pasedNumbers.isEmpty)
	}

	// Structured Basis: nominal case
	// Bad data: A line with an invalid width
	behavior of "isValidWidth"
	it should "test nominally, a line has invalid width" in {
		val line = Array("test".toCharArray)
		val isValidWidth = PrivateMethod[Boolean]('isValidWidth)
		val result = Validator.instance.invokePrivate(isValidWidth(line))
		assert(!result)
	}

	// Structured basis: For loop false
	// Bad data: no lines
	it should "test empty lines, for statement false" in {
		val isValidWidth = PrivateMethod[Boolean]('isValidWidth)
		val array = Array[Array[Char]]()
		val result = Validator.instance.invokePrivate(isValidWidth(array))
		assert(result)
	}

	// Structured basis: if statement false
	// Good data: min config: one line has a width of 27, 1 line total
	it should "test single line of good width, first if statement false" in {
		val isValidWidth = PrivateMethod[Boolean]('isValidWidth)
		val array = Array("abdjfldiekdlfiekdldieldiekd".toCharArray)
		val result = Validator.instance.invokePrivate(isValidWidth(array))
		assert(result)
	}

	// Good data: normal config: 3 lines has a width of 27, 3 line total
	it should "test normal config" in {
		val isValidWidth = PrivateMethod[Boolean]('isValidWidth)
		val array = ListBuffer("abdjfldiekdlfiekdldieldiekd".toCharArray)
		array += "abdjfldiekdlfiekdldieldiekd".toCharArray
		array += "abdjfldiekdlfiekdldieldiekd".toCharArray

		val result = Validator.instance.invokePrivate(isValidWidth(array.toArray))
		assert(result)
	}

	// Good data: max normal config: 100 lines has a width of 27, 3 line total
	it should "test max config" in {
		val isValidWidth = PrivateMethod[Boolean]('isValidWidth)
		val array = ListBuffer[Array[Char]]()

		for (i <- 1 to 100) {
			array += "abdjfldiekdlfiekdldieldiekd".toCharArray
		}

		val result = Validator.instance.invokePrivate(isValidWidth(array.toArray))
		assert(result)
	}

	// Structured Basis: nominal case
	// Good data: min normal config: 1 number in the list
	behavior of "constructNumbersFrom"
	it should "test nominally, min normal config" in {
		Logger.instance.severeError = false

		val parsedNumber = List(ParsedNumber(List(4,7)))
		val result = Validator.instance.constructNumbersFrom(parsedNumber)
		assert(result == Left(List(1)))
	}

	// Good data:  normal config: 3 numbers in the list
	it should "test normal config" in {
		Logger.instance.severeError = false

		val parsedNumbers = ListBuffer[ParsedNumber]()

		for (i <- 1 to 3) {
			parsedNumbers += ParsedNumber(List(4,7))
		}
		val result = Validator.instance.constructNumbersFrom(parsedNumbers.toList)
		assert(result == Left(List(1,1,1)))
	}

	// Good data:  max normal config: 100 numbers in the list
	it should "test max normal config" in {
		Logger.instance.severeError = false

		val parsedNumbers = ListBuffer[ParsedNumber]()

		for (i <- 1 to 100) {
			parsedNumbers += ParsedNumber(List(4,7))
		}
		val result = Validator.instance.constructNumbersFrom(parsedNumbers.toList)


		val buffer = ListBuffer[Int]()
		for (i <- 1 to 100) {
			buffer += 1
		}

		assert(result == Left(buffer.toList))
	}

	// Structured basis: first if statement false
	// Bad data: severe error
	it should "test severe error" in {
		Logger.instance.severeError = true

		val result = Validator.instance.constructNumbersFrom(List(ParsedNumber(List(4,5))))
		assert(result == Right("failure"))
	}
}
