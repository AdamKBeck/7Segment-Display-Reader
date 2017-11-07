package number_recognition

import org.scalatest.FlatSpec
import java.util.Random
import java.nio.file._

class StressTest extends FlatSpec {

	// Add benign errors to a valid input.
	behavior of "stress test"
	it should "pass benign value erorrs, throw errors on invalid segment positions" in {
		Thread.sleep(3000)
		Logger.instance.severeError = false


		val topLine =    "    _  _  _  _  _  _  _  _ "
		val middleLine = "  | _| _||_||_ |_   ||_||_|"
		val bottomLine = "  ||_  _| _| _||_|  ||_| _|"

		val args = Array(topLine, middleLine, bottomLine)
		OpticalRecognition.main(args)

		var rand = new Random()

		var badOutput = false

		for (i <- 1 to 100) {
			Files.deleteIfExists(Paths.get("src/input.txt"))
			Files.deleteIfExists(Paths.get("src/output.txt"))

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


		// put invalid segments in the lines
		for (i <- 1 to 100) {
			var topLine =    "    _  _  _  _  _  _  _  _ "
			var middleLine = "  | _| _||_||_ |_   ||_||_|"
			var bottomLine = "  ||_  _| _| _||_|  ||_| _|"

			val randslot = ((rand.nextInt(8)+1)*3)-3

			topLine = topLine.updated(randslot, '|')

			val args = Array(topLine, middleLine, bottomLine)

			Files.deleteIfExists(Paths.get("src/input.txt"))
			Files.deleteIfExists(Paths.get("src/output.txt"))

			OpticalRecognition.main(args)

			val outputLines = FileReader.instance.lines("output.txt")
			val output = outputLines.head.mkString("")

			if (output != "failure") {
				badOutput = true
			}
		}
		// badOutput should be false
		assert(!badOutput)

	}

}
