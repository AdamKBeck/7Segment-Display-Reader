package number_recognition

import java.io.BufferedInputStream
import java.util.Scanner
import scala.util.control.Breaks._

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {
}

object OpticalRecognition {
	def main(args: Array[String]): Unit = {

		if (args.length == 0) {
			val stdIn = new Scanner(new BufferedInputStream(System.in))
			println("Enter your input, then type 'quit' to start the optical recognition process")

			breakable {
				while (true) {
					val line = stdIn.nextLine()
					if (line == "quit") {
						break
					}

					Logger.instance.log(line, "input.txt")
				}
			}
		}

		else {
			for (line <- args) {
				Logger.instance.log(line, "input.txt")
			}
		}

		val lines = Validator.instance.lines("input.txt")

		val benignLines = Validator.instance.benignLines(lines)

		val parsedNumbers = Validator.instance.parsedNumbers(benignLines)
		val output = Validator.instance.constructNumbersFrom(parsedNumbers)

		output match {
			case Right(s) => {
				println(s)
				Logger.instance.log(s, "output.txt")
			}
			case Left(list) => {
				println(list.mkString(""))
				Logger.instance.log(list.mkString(""), "output.txt")
			}
		}
	}
}