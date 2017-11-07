package number_recognition

case class Validator private(){

	private val _groupSize = 3
	def groupSize = _groupSize

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
		if (lines.length == 3) {
			BenignErrorCorrector.instance.correct(lines)
		}

		else {
			Logger.instance.log("There are not 3 lines in the input")
			Logger.markSevereError()
			Array()
		}
	}

	def parsedNumbers(benignLines: Array[Array[Char]]): List[ParsedNumber] = {
		if (benignLines.nonEmpty) {

			val topGroup = groupedLine(benignLines.head, groupSize)
			val middleGroup = groupedLine(benignLines(1), groupSize)
			val bottomGroup = groupedLine(benignLines.last, groupSize)
			val parsedNumbers = ListBuffer[ParsedNumber]()
			// TODO: make above in a separate file
		}

		else {
			Logger.instance.log("No benign lines could be formed from the input")
			Logger.markSevereError()
			Array()
		}


	}


}

object Validator {
	private val _instance = Validator()

	def instance = _instance
}
