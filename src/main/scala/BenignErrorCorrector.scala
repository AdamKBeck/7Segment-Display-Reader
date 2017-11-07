package number_recognition

case class BenignErrorCorrector private (){

	/* Corrects the benign errors in the lines. A benign error is if a non |, _, or space is encountered.
	 * For robustness, this character will change into a space, but the error logger will make note of this.
	 * Named after as a procedure as the main goal of this method is to correct the benign errors in the lines.
	 * This is more apparent if there is deemed to be many types of benign errors. If this is true, this method
	 * will be extremely long, consisting of correcting for these errors, hence why it is named procedurally */
	def correct(lines: Array[Array[Char]]): Array[Array[Char]] = {
		var linesCopy = lines.clone

		val validCharacters = " |_"

		val benignLines = linesCopy.map(line => {
			line.map(char => {
				if (!validCharacters.contains(char)){
					Logger.instance.log("Benign value " + char.toString + " was changed to a space");
					' '
				}
				else {
					char
				}
			})
		})

		benignLines
	}
}

object BenignErrorCorrector {
	private val _instance = BenignErrorCorrector()

	def instance = _instance
}
