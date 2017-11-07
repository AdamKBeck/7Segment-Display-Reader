package number_recognition

import scala.collection.mutable.ListBuffer

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition {
	def main(args: Array[String]): Unit = {
		// TODO: Take standard input and write to file, then read it
		val	fileName = "hw10a.in.txt"
		val lines = Validator.instance.lines(fileName)
		val benignLines = Validator.instance.benignLines(lines)


		for (line <- benignLines) {
			println(line.mkString(""))
		}

		val parsedNumbers = Validator.instance.parsedNumbers(benignLines)
		val output = Validator.instance.constructNumbersFrom(parsedNumbers)

		output match {
			case Right(s) => println(s)
			case Left(list) => println(list.mkString(""))
		}
	}


}
