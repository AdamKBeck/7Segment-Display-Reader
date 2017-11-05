package number_recognition

import scala.collection.mutable.ListBuffer

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition {
	val groupSize = 3

	def main(args: Array[String]): Unit = {
		var fileName = "hw10a.in.txt"
		constructNumbersFromFile(fileName) match {
			case Right(s) => println(s)
			case Left(list) => println(list.mkString(""))
		}
		fileName = "hw10b.in.txt"
		constructNumbersFromFile(fileName) match {
			case Right(s) => println(s)
			case Left(list) => println(list.mkString(""))
		}
		fileName = "hw10c.in.txt"
		constructNumbersFromFile(fileName) match {
			case Right(s) => println(s)
			case Left(list) => println(list.mkString(""))
		}
		fileName = "hw10d.in.txt"
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

		for (parsedNumber <- numbers) {
			val number = ValidNumbers.booleanSegmentedNumber(parsedNumber.segments)
			// Filter the cache against the segements we have, and see what remains in the cache
			val keys = cache.keySet

			val possibleMatch = cache.getOrElse(number, None)

			if (possibleMatch == None) {
				var remaining = keys.filter(k => isSegmentSubset(number, k))
				if (remaining.size > 1) {
					// Too many solutions
					return Right("ambiguous")
				}
				else {
					// No solution
					return Right("failure")
				}
			}

			else {
				numbersFromFile += cache(number)
			}
		}

		Left(numbersFromFile.toList)
	}

	/* Constructs numbers out of each line by splitting in groups of 3 */
	def linesToNumbers(lines: Array[Array[Char]]): List[ParsedNumber] = {
		// TODO: check if lines are all same length, log if needed fill with benign values, etc.

		val topGroup = groupedLine(lines.head, groupSize)
		val middleGroup = groupedLine(lines(1), groupSize)
		val bottomGroup = groupedLine(lines.last, groupSize)

		val parsedNumbers = ListBuffer[ParsedNumber]()

		for (i <- topGroup.indices) {
			parsedNumbers += parsedNumber(topGroup(i), middleGroup(i), bottomGroup(i))

		}

		parsedNumbers.toList
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