package number_recognition

import scala.collection.mutable.ListBuffer

case class NumberParser private(){
	private val _groupSize = 3
	def groupSize = _groupSize

	def parsedNumbers(benignLines: Array[Array[Char]]): List[ParsedNumber] = {
		val topGroup = groupedLine(benignLines.head, groupSize)
		val middleGroup = groupedLine(benignLines(1), groupSize)
		val bottomGroup = groupedLine(benignLines.last, groupSize)
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
			if (isSegmentSymbolValid(char)) {
				checkSegmentPosition(index)
				segments += indexToSegmentNumber(index)
			}
		}

		ParsedNumber(segments.toList)
	}

	// Determines if a character from the input file is a valid segment character
	private def isSegmentSymbolValid(char: Char): Boolean = char == '|' || char == '_'

	// Logs if a segment index is in an invalid spot
	private def checkSegmentPosition(index: Int): Unit = {
		if (index == 0 || index == 2) {
			Logger.instance.log("A segment is in the wrong spot at an index of: " + index + ". Error: Extra segment(s)")
			Logger.markSevereError()
		}
	}

	/* Converts the index a character was found in a top, middle, bottom group to the correct segment index
	 * Method written in ternary syntax style https://alvinalexander.com/scala/scala-ternary-operator-syntax */
	private def indexToSegmentNumber(index: Int): Int = if (index > 2) index - 1 else index
}

object NumberParser {
	private val _instance = NumberParser()

	def instance = _instance
}
