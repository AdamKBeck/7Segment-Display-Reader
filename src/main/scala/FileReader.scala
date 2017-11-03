/* This class reads an input text file containing the numbers. FileReader will try to split up each of the 3 lines of
 * 27 characters into 9 number objects. This class does not check if each number is valid. It only checks the validity of
 * if the input file is of the correct size and if each character is of the corrrect type (e.g. only using "|" and "-")
 */

import scala.io.Source

case class FileReader() {

	/* Reads a txt file line by line, and returns each line as an Array
	 * Named as a function because we are returning the lines of a file */
	private def lines(fileName: String): Array[String] = {
		val bufferedSource = Source.fromFile(fileName)

		// yield each line to a sequence, then convert that to an array because it may need to be modified
		val lines =
			(for (line <- bufferedSource.getLines()) yield line).toArray

		bufferedSource.close()
		
		lines
	}

}
