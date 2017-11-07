package number_recognition

import org.scalatest.FlatSpec
import java.io._
import java.nio.file._

class LoggerTest extends FlatSpec{

	behavior of "log"
	it should "write to a file" in {
		Logger.instance.log("sample message", "loggertest.txt")
		val lines = FileReader.instance.lines("loggertest.txt")
		assert(lines(0).mkString("") == "sample message")
	}

	// Structured basis, for statement false
	// Bad data: Nil filenames
	behavior of "delete"
	it should "test with a Nil fileName" in {
		Logger.instance.delete(Set())

		assert(new File("src/loggertest.txt").exists())
	}

	// Structured basis, nominal case
	// Good data, a set not Nil
	it should "test nominally" in {
		Logger.instance.delete(Set("loggertest.txt"))

		assert(!new File("src/loggertest.txt").exists())
	}

	// Structured basis, nominal case
	// Good data: input is not Nil
	behavior of "createFile"
	it should "test nominally" in {
		Logger.instance.createFile(List("test"), "loggertest2.txt")
		assert(new File("src/loggertest2.txt").exists())
	}

	// Structured basis, for statement false
	// Bad data, input List is Nil
	it should "test with Nil input, for statment false" in {
		Logger.instance.createFile(List(), "loggertest3.txt")
		assert(!new File("src/loggertest3.txt").exists())
	}


}
