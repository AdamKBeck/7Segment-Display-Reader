package number_recognition

/* This class stores only the valid number 0-9. Each number is a list (because it needs to be ordered) of 7 booleans
 * denoting whether or not the number has a certain segment.
 *
 *
 * The possible segments are:
 *  _
 * |_|
 * |_|
 *
 * which has the segments (reading line by line):
 *  1
 * 234
 * 567
 */

case class ValidNumbers() {
	def numberCache = _numberCache

	private final val _numberCache = Map(
		0 -> booleanSegmentedNumber(1, 2, 4, 5, 7),
		1 -> booleanSegmentedNumber(4, 7),
		2 -> booleanSegmentedNumber(1, 3, 4, 5, 6),
		3 -> booleanSegmentedNumber(1, 3, 4, 6, 7),
		4 -> booleanSegmentedNumber(2, 3, 4, 7),
		5 -> booleanSegmentedNumber(1, 2, 3, 6, 7),
		6 -> booleanSegmentedNumber(1, 2, 3, 5, 6, 7),
		7 -> booleanSegmentedNumber(1, 4, 7),
		8 -> booleanSegmentedNumber(1, 2, 3, 4, 5, 6, 7),
		9 -> booleanSegmentedNumber(1, 2, 3, 4, 6, 7)
	)

	/* Returns a 7-segmented number represented as a list of size 7 of booleans denoting which segments are on */
	private def booleanSegmentedNumber(segments:Int*): List[Boolean] = {
		val number= Array[Boolean](false, false, false, false, false, false, false)

		for (i <- segments) {
			number(i) = true
		}

		number.toList
	}


}
