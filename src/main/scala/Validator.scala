package number_recognition

case class Validator private(){
	private val _lineWidth = 27
	def lineWidth = _lineWidth

	/* Validates that the fileName is a valid file, and if so, gets the lines of the file
	 * Otherwise, mark a severe error, and return an array of no lines, as no file was found
	 */
	def lines(fileName: String): Array[Array[Char]] = {
		if (new java.io.File("src/"+fileName).exists) {
			FileReader.instance.lines(fileName)
		}
		else {
			Logger.instance.log("File not found: " + fileName)
			Logger.markSevereError()
			Array()
		}
	}

	// Returns the benign lines of the txt file, that is, the benign erros in the lines are corrected for
	def benignLines(lines: Array[Array[Char]]): Array[Array[Char]] = {
		if (lines.length == 3 || (lines.length == 4 && lines(3).length == 0)) {
			BenignErrorCorrector.instance.correct(lines)
		}

		else {
			Logger.instance.log("There are not 3 lines in the input")
			Logger.markSevereError()
			Array()
		}
	}

	// Returns each number in the benign lines as a parsed Number, or logs why the input was in an invalid form
	def parsedNumbers(benignLines: Array[Array[Char]]): List[ParsedNumber] = {
		if (benignLines.nonEmpty) {
			if (isValidWidth(benignLines)) {
				NumberParser.instance.parsedNumbers(benignLines)
			}

			else {
				Logger.instance.log("Lines are all not length 27. Error: Not 9 digits long")
				Logger.markSevereError()
				Nil
			}
		}

		else {
			Logger.instance.log("No benign lines could be formed from the input")
			Logger.markSevereError()
			Nil
		}
	}

	/* Helper method to determine if a set of lines are rectangular, that is,
	 * If every line is of the same width */
	private def isValidWidth(benignLines: Array[Array[Char]]): Boolean = {
		val width = lineWidth
		for (line <- benignLines) {
			if (line.length != width){
				return false

			}
		}
		true
	}

	/* Reads the input file and constructs numbers from the segments in the file.
	 * Named as a procedure as this bulk and primary purpose of this function is to parse/decode the numbers.
	 * Finally, after all of this, we return the number, or a string representing the type of failure */
	def constructNumbersFrom(parsedNumbers: List[ParsedNumber]): Either[List[Int], String] = {
		if (!Logger.instance.severeError) {
			NumberConstructor.instance.constructNumbersFrom(parsedNumbers)
		}

		else {
			Right("failure")
		}
	}
}

object Validator {
	private val _instance = Validator()

	def instance = _instance
}
