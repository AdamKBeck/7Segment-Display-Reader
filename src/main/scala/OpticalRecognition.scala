package number_recognition

import scala.collection.mutable.ListBuffer

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition {
	val groupSize = 3

	def main(args: Array[String]): Unit = {
		// TODO: Take standard input and write to file, then read it
		val	fileName = "hw10a.in.txt"
		val lines = Validator.instance.lines(fileName)




//		constructNumbersFromFile(fileName) match {
//			case Right(s) => println(s)
//			case Left(list) => println(list.mkString(""))
//		}
	}

	/* Reads the input file and constructs numbers from the segments in the file.
	 * Named as a procedure as this bulk and primary purpose of this function is to parse/decode the numbers.
	 * Finally, after all of this, we return the number, or a string representing the type of failure
	 */

	private def constructNumbersFromFile(fileName: String): Either[List[Int], String] = {
		// Read file and parse numbers
		val reader = FileReader.instance
		val lines = reader.lines(fileName)
		val numbers = linesToNumbers(lines)

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

	/* Constructs numbers out of each line by splitting in groups of 3 */
	private def linesToNumbers(lines: Array[Array[Char]]): List[ParsedNumber] = {
		val benignLines = correctBenignErrorsIn(lines.clone)

		val topGroup = groupedLine(benignLines.head, groupSize)
		val middleGroup = groupedLine(benignLines(1), groupSize)
		val bottomGroup = groupedLine(benignLines.last, groupSize)

		val parsedNumbers = ListBuffer[ParsedNumber]()

		for (i <- topGroup.indices) {
			parsedNumbers += parsedNumber(topGroup(i), middleGroup(i), bottomGroup(i))
		}

		parsedNumbers.toList
	}

	/* Corrects the benign errors in the lines. A benign error is if a non |, _, or space is encountered.
	 * For robustness, this character will change into a space, but the error logger will make note of this.
	 * Named after as a procedure as the main goal of this method is to correct the benign errors in the lines.
	 * This is more apparent if there is deemed to be many types of benign errors. If this is true, this method
	 * will be extremely long, consisting of correcting for these errors, hence why it is named procedurally */
	private def correctBenignErrorsIn(lines: Array[Array[Char]]): Array[Array[Char]] = {
		val linesCopy = lines.clone

		val validCharacters = " |_"

		linesCopy.map(line => {
			line.map(char => {
				if (!validCharacters.contains(char)){
					Logger.instance.log("Benign value " + char.toString + " was changed to a space")
					' '
				}
				else {
					char
				}
			})
		})

		linesCopy
	}

	/* Groups a given line from the text file by a grouping size, and return a list of these groups.
	 * Used in linesToNumbers to construct numbers in groups of 3 */
	private def groupedLine(line: Array[Char], size: Int): List[Array[Char]] = line.grouped(size).toList


	/* Returns a ParsedNumber number given the top, middle, and bottom parts of the number in the input file */
	private def parsedNumber(top: Array[Char], middle: Array[Char], bottom: Array[Char]): ParsedNumber = {
		val segments = ListBuffer[Int]()

		val charsWithIndices = {
			top.zip(Array(0,1,2)) ++ middle.zip(Array(3,4,5)) ++ bottom.zip(Array(6,7,8))
		}

		for ((char, index) <- charsWithIndices) {
			if (isSegmentSymbolValid(char)) {
				checkSegmentPosition(index)
				segments += indexToSegmentNumber(index)
			}
		}

		ParsedNumber(segments.toList)
	}

	// Logs if a segment index is in an invalid spot
	private def checkSegmentPosition(index: Int): Unit = {
		if (index == 0 || index == 2) {
			Logger.instance.log("A segment is in the wrong spot at an index of: " + index)
			Logger.markSevereError()
		}
	}

	// Determines if a character from the input file is a valid segment character
	private def isSegmentSymbolValid(char: Char): Boolean = char == '|' || char == '_'

	/* Converts the index a character was found in a top, middle, bottom group to the correct segment index
	 * Method written in ternary syntax style https://alvinalexander.com/scala/scala-ternary-operator-syntax */
	private def indexToSegmentNumber(index: Int): Int = if (index > 2) index - 1 else index

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
