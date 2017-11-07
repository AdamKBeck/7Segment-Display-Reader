package number_recognition

import org.scalatest.FlatSpec

class FileReaderTest extends FlatSpec {
	// Structured basis, nominal case
	// Good data: filename not null, lines in file
	behavior of "lines"
	it should "test nominally" in {
		val output = FileReader.instance.lines("hw10b.in.txt")
		assert(output.length == 3)
	}

	// Structured basis: for statement false
	// Bad data: no lines in file
	it should "test with no lines in txt, for statement false" in {
		val output = FileReader.instance.lines("blankfile.txt")
		assert(output.length == 0)
	}

}
