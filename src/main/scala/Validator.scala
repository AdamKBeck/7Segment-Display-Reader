package number_recognition

case class Validator private(){
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

	// Returns each number in the benign lines as a parsed Number, or logs why the input was in an invalid form
	def parsedNumbers(benignLines: Array[Array[Char]]): List[ParsedNumber] = {
		if (benignLines.nonEmpty) {
			if (isValidWidth(benignLines)) {
				NumberParser.instance.parsedNumbers(benignLines)
			}

			else {
				Logger.instance.log("Lines are all not length 27")
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
		val headWidth = benignLines.head.length
		var isSameWidth = true

		for (line <- benignLines.tail) {
			if (line.length != headWidth) {
				isSameWidth = false

			}
		}

		isSameWidth
	}

	/* Reads the input file and constructs numbers from the segments in the file.
	 * Named as a procedure as this bulk and primary purpose of this function is to parse/decode the numbers.
	 * Finally, after all of this, we return the number, or a string representing the type of failure */
	private def constructNumbersFrom(parsedNumbers: List[ParsedNumber]): Either[List[Int], String] = {
		if (!Logger.instance.severeError) {
			

		}

		else {
			"failure"
		}

		val cache = ValidNumbers.numberCache

		// Setup a list of numbers from the file, and a counter for ambiguous and defected numbers
		val numbersFromFile = ListBuffer[Int]()
		var defectedCount = 0
		var ambiguousCount = 0

		// Note if each number is ambiguous, defected, both, or neither (append this valid number to our list)
		for (parsedNumber <- numbers) {
			val number = ValidNumbers.booleanSegmentedNumber(parsedNumber.segments)

			// Filter the cache against the segments we have, and see what remains in the cache
			val keys = cache.keySet
			val possibleMatch = cache.getOrElse(number, None)

			// If the cache didn't contain, this parsed number, determine if it's ambiguous or if we can deduce what it is
			if (possibleMatch == None) {
				defectedCount += 1
				logIfEmpty(number)

				var remaining = keys.filter(k => isSegmentSubset(number, k))

				// If this number is a subset of multiple numbers, there's no number we can deduce from the missing segments
				if (remaining.size > 1) {
					ambiguousCount += 1
				}

				// This number must be a subset of the 8, so get it, and append 8 to our list
				else {
					numbersFromFile += cache(remaining.head)
				}
			}

			// If the cahche contained this parsed number, append the number as an int to our list
			else {
				numbersFromFile += cache(number)
			}
		}

		opticalRecognitionOutcome(Logger.instance.severeError, defectedCount, ambiguousCount, numbersFromFile)
	}



}

object Validator {
	private val _instance = Validator()

	def instance = _instance
}
