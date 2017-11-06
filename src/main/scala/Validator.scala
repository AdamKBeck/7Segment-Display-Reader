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
}

object Validator {
	private val _instance = Validator()

	def instance = _instance
}
