package number_recognition

import scala.collection.mutable.ListBuffer

/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition{
	val groupSize = 3

	def main(args: Array[String]): Unit = {
		println("Test")
	}


	/* Constructs numbers out of each line by splitting in groups of 3 */
	def linesToNumbers(lines: Array[Array[Char]]): List[ParsedNumber] = {
		// TODO: check if lines are all same length, log if needed fill with benign values, etc.

		val top = groupedLine(lines.head, groupSize)
		val middle = groupedLine(lines(1), groupSize)
		val bottom = groupedLine(lines.last, groupSize)

		val parsedNumbers = ListBuffer[ParsedNumber]()

		for (topGroup <- top; middleGroup <- middle; bottomGroup <- bottom) {
			parsedNumbers += parsedNumber(topGroup, middleGroup, bottomGroup)

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


}
