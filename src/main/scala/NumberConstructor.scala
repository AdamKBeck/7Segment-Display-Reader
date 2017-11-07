package number_recognition

import scala.collection.mutable.ListBuffer

case class NumberConstructor private() {

	/* Reads the input file and constructs numbers from the segments in the file.
	 * Named as a procedure as this bulk and primary purpose of this function is to parse/decode the numbers.
	 * Finally, after all of this, we return the number, or a string representing the type of failure */
	def constructNumbersFrom(parsedNumbers: List[ParsedNumber]): Either[List[Int], String] = {
		val cache = ValidNumbers.numberCache

		// Setup a list of numbers from the file, and a counter for ambiguous and defected numbers
		val numbersFromFile = ListBuffer[Int]()
		var defectedCount = 0
		var ambiguousCount = 0

		// Note if each number is ambiguous, defected, both, or neither (append this valid number to our list)
		for (parsedNumber <- parsedNumbers) {
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

	// Determines if a number is empty, that is, a number has no segments. Log this, as this is an error
	private def logIfEmpty(number: List[Boolean]): Unit = {
		var isEmpty = !number.contains(true)

		if (isEmpty) {
			Logger.instance.log("A digit is empty (invisible and missing). Error: Not 9 digits long")
			Logger.markSevereError()
		}
	}

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

}

object NumberConstructor {
	private val _instance = NumberConstructor()

	def instance = _instance
}
