package number_recognition

/* This class reads an input text file containing the numbers. FileReader will try to split up each of the 3 lines of
 * 27 characters into 9 number objects. This class does not check if each number is valid. It only checks the validity of
 * if the input file is of the correct size and if each character is of the corrrect type (e.g. only using "|" and "-")
 */

import scala.io.Source

case class FileReader private() {

	/* Reads a txt file line by line, and returns each line as an Array
	 * Named as a function because we are returning the lines of a file */
	def lines(fileName: String): Array[Array[Char]] = {
		val bufferedSource = Source.fromFile("src/"+fileName)

		// Convert the txt file to an array of lines, each line is a character array
		val lines = (
			for (line <- bufferedSource.getLines()) yield {
				line.toCharArray
			}
			).toArray

		bufferedSource.close()

		lines
	}
}

object FileReader {
	private val _instance = FileReader()

	def instance = _instance
}
