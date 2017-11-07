package number_recognition

import org.scalatest.FlatSpec
import java.io._
import java.nio.file._
class OpticalRecognitionTest extends FlatSpec{

	// Helper method to clean up file I/O when testing
	private def cleanup(): Unit = {
		Files.deleteIfExists(Paths.get("src/output.txt"))
		Files.deleteIfExists(Paths.get("src/input.txt"))
		Files.deleteIfExists(Paths.get("src/log.txt"))
		Logger.instance.severeError = false
	}
	// Structured basis: nominal case
	// Bad data: input only contains quit, no numbers
	behavior of "main method"
	it should "test nominally" in {
		cleanup()

		val input = "quit"
		val in: InputStream = new ByteArrayInputStream(input.getBytes())
		System.setIn(in)
		OpticalRecognition.main(Array())

		System.setIn(System.in)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "failure")
	}

	// Structured Basis: second if statement false
	// Bad data: input is not 3 lines long
	it should "test with less than 3 total lines long" in {
		cleanup()

		val input = "garbage first line \nquit"
		val in: InputStream = new ByteArrayInputStream(input.getBytes())
		System.setIn(in)
		OpticalRecognition.main(Array())

		System.setIn(System.in)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "failure")
	}

	// Structured Basis: first if statement false
	// Good data: min/normal/max normal case txt file input contains a valid number
	// First case statement is true
	it should "test with good txt file data forming a string of numbers, min/normal/max normal case, Right case return" in {
		cleanup()

		val topLine =    "       _     _  _  _  _  _ "
		val middleLine = "  | _| _||_||_ |_   ||_||_|"
		val bottomLine = "  ||_   |  | _||_|. ||_| _|"

		val args = Array(topLine, middleLine, bottomLine)

		OpticalRecognition.main(args)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "failure")
	}

	it should "test with good txt file data forming a string of numbers, min/normal/max normal case, Left case return" in {
		Thread.sleep(200) // to avoid interferring with file I/O from other tests
		cleanup()

		val topLine =    "    _  _  _  _  _  _  _  _ "
		val middleLine = "  | _| _||_||_ |_   ||_||_|"
		val bottomLine = "  ||_  _| _| _||_|. ||_| _|"

		val args = Array(topLine, middleLine, bottomLine)

		OpticalRecognition.main(args)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "123956789")
	}
}
