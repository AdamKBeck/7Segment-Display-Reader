package number_recognition

import org.scalatest.FlatSpec
import java.util.Random
import java.nio.file._

class StressTest extends FlatSpec {

	behavior of "stress test"
	it should "randomly add benign errors to a valid set of numbers" in {
		Thread.sleep(2000)


		val topLine =    "    _  _  _  _  _  _  _  _ "
		val middleLine = "  | _| _||_||_ |_   ||_||_|"
		val bottomLine = "  ||_  _| _| _||_|  ||_| _|"

		val args = Array(topLine, middleLine, bottomLine)
		OpticalRecognition.main(args)

		var rand = new Random()

		var badOutput = false

		for (i <- 1 to 100) {
			Files.deleteIfExists(Paths.get("src/input.txt"))

			val randLine = rand.nextInt(3)
			val randIndex = rand.nextInt(27)

			if (args(randLine)(randIndex) == ' ') {
				args(randLine)(randIndex) == '.'
			}

			OpticalRecognition.main(args)

			val outputLines = FileReader.instance.lines("output.txt")
			val output = outputLines.head.mkString("")
			if (output != "123956789") {
				badOutput = true
			}
		}
		assert(!badOutput)
	}

}
