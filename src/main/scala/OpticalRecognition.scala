package number_recognition

import scala.collection.mutable.ListBuffer

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition {
	val groupSize = 3

	def main(args: Array[String]): Unit = {
		// Check severity and things
		// TODO: Take standard input and write to file, then read it
		var	fileName = "hw10a.in.txt"
		constructNumbersFromFile(fileName) match {
			case Right(s) => println(s)
			case Left(list) => println(list.mkString(""))
		}
	}

	/* Reads the input file and constructs numbers from the segments in the file.
	 * Named as a procedure as this bulk and primary purpose of this function is to parse/decode the numbers.
	 * Finally, after all of this, we return the number, or a string representing the type of failure
	 */

	private def constructNumbersFromFile(fileName: String): Either[List[Int], String] = {

		val reader = FileReader()

		val lines = reader.lines(fileName)

		val numbers = linesToNumbers(lines)

		val cache = ValidNumbers.numberCache

		for (line <- lines) {
			println(line.mkString(""))
		}

		val numbersFromFile = ListBuffer[Int]()
		var defectedCount = 0
		var ambiguousCount = 0

		for (parsedNumber <- numbers) {
			val number = ValidNumbers.booleanSegmentedNumber(parsedNumber.segments)
			// Filter the cache against the segements we have, and see what remains in the cache
			val keys = cache.keySet

			val possibleMatch = cache.getOrElse(number, None)

			if (possibleMatch == None) {
				defectedCount += 1

				var remaining = keys.filter(k => isSegmentSubset(number, k))
				if (remaining.size > 1) {
					ambiguousCount += 1
				}
				else {
					numbersFromFile += cache(remaining.head)
				}
			}
			else {
				numbersFromFile += cache(number)
			}
		}

		if (defectedCount >= 2) {
			Right("failure")
		}
		else if (ambiguousCount == 1) {
			Right("ambiguous")
		}
		else {

			Left(numbersFromFile.toList)
		}
	}

	/* Constructs numbers out of each line by splitting in groups of 3 */
	def linesToNumbers(lines: Array[Array[Char]]): List[ParsedNumber] = {
		// TODO: check if lines are all same length, log if needed fill with benign values, etc.
		// TODO: Check all the 5 conditions



		if (areLinesValidSize(lines)) {
			val topGroup = groupedLine(lines.head, groupSize)
			val middleGroup = groupedLine(lines(1), groupSize)
			val bottomGroup = groupedLine(lines.last, groupSize)

			val parsedNumbers = ListBuffer[ParsedNumber]()

			for (i <- topGroup.indices) {
				parsedNumbers += parsedNumber(topGroup(i), middleGroup(i), bottomGroup(i))

			}

			parsedNumbers.toList
		}

		else {
			Logger.instance.log("Lines are not 3 lines with 27 characters each")
			Logger.markSevereError()
			Nil
		}
	}

	// Determines whether lines are valid, logging the instances where they are not
	def areLinesValidSize(lines: Array[Array[Char]]): Boolean = {
		if (lines.length != 3) {
			Logger.instance.log("There are not 3 lines given")
			Logger.markSevereError()
			false
		}

		else {
			for (line <- lines) {
				if (line.length != 27) {
					Logger.instance.log("A line length was not of size 27, line: " + lines.indexOf(line))
					Logger.markSevereError()
					return false
				}
			}

			true
		}
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
			if (isValid(char)) {
				segments += indexToSegmentNumber(index)
			}
		}

		ParsedNumber(segments.toList)
	}

	// Determines if a character from the input file is a valid segment character
	private def isValid(char: Char): Boolean = char == '|' || char == '_'


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
