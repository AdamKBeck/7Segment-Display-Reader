/* This class contains the main method to launch the application. This program reads in an input file
 * parses the 7 segmented numbers in the input file, and tries to construct the number these segments represent */
case class OpticalRecognition() {

}

object OpticalRecognition{
	def main(args: Array[String]): Unit = {
		println("Test")
	}


	/* Constructs numbers out of each line by splitting in groups of 3 */
	def linesToNumbers(lines: Array[Array[Char]]): List[ParsedNumber] = {
		// TODO: check if lines are all same length, log if needed fill with benign values, etc.

		val top = groupedLine(lines.head, 3)
		val middle = groupedLine(lines(1), 3)
		val bottom = groupedLine(lines.last, 3)


		for (topGroup <- top; middleGroup <- middle; bottomGroup <- bottom) {


		}


		???

		//TODO: Finish
	}
}
