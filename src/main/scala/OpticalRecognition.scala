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
		val parsedNumbers = Validator.instance.parsedNumbers(benignLines)
		val output = Validator.instance.constructNumbersFrom(parsedNumbers)

		/* Although parsed numbers can form, we could still have a severe error.
		 * A segment could have been in the wrong spot (index 0 and 2) */





//		constructNumbersFromFile(fileName) match {
//			case Right(s) => println(s)
//			case Left(list) => println(list.mkString(""))
//		}
	}


	// Returns the outcome of the optial recognition based on the error severity, defected count, ambiguous count, and parsed numbers
	private def opticalRecognitionOutcome(severeError: Boolean, defectedCount: Int,
		ambiguousCount: Int, numbersFromFile: ListBuffer[Int]): Either[List[Int], String] = {
		if (severeError) {
			Right("failure")
		}
		else if (defectedCount >= 2) {
			Logger.instance.log("Error: More than 1 defected number found")
			Right("failure")
		}
		else if (ambiguousCount == 1) {
			Right("ambiguous")
		}
		else {
			Left(numbersFromFile.toList)
		}

	}

	// Determines if a number is empty, that is, a number has no segments. Log this, as this is an error
	private def logIfEmpty(number: List[Boolean]): Unit = {
		var isEmpty = true
		for (boolean <- number) {
			if (boolean) {
				isEmpty = false
			}
		}

		if (isEmpty) {
			Logger.instance.log("A digit is empty (invisible and missing). Error: Not 9 digits long")
			Logger.markSevereError()
		}
	}

	/* Determines if a parsedNumber is a segment sebset of validNumber, that is, if
	 * all the true values in parsedNumber appear in validNumber */
	private def isSegmentSubset(parsedNumber: List[Boolean], validNumber: List[Boolean]): Boolean = {
		var isSubset = true

		for (i <- parsedNumber.indices) {
			if (parsedNumber(i) && !validNumber(i)) {
				isSubset = false
			}
		}

		isSubset
	}
}
