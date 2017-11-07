package number_recognition

import org.scalatest.FlatSpec
import java.io._
import java.nio.file._
class OpticalRecognitionTest extends FlatSpec{


	// Structured basis: nominal case
	// Bad data: input only contains quit, no numbers
	behavior of "main method"
	it should "test nominally" in {
		Files.deleteIfExists(Paths.get("src/output.txt"))
		val input = "quit"
		val in: InputStream = new ByteArrayInputStream(input.getBytes())
		System.setIn(in)
		OpticalRecognition.main(Array())


		System.setIn(System.in)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "failure")
	}

	// Structured Basis: first if statement false
	// Good data: min/normal/max normal case txt file input contains a valid number
	it should "test with good txt file data forming a string of numbers, min/normal/max normal case" in {
		val topLine = "    _  _     _  _  _  _  _ "
		val middleLine = "  | _| _||_||_ |_   ||_||_|"
		val bottomLine = "  ||_  _|  | _||_|. ||_| _|"

		val args = Array(topLine, middleLine, bottomLine)

		Logger.instance.log("stuff")
		OpticalRecognition.main(args)
		val lines = FileReader.instance.lines("output.txt")
		assert(lines.head.mkString("") == "failure")
	}
}
