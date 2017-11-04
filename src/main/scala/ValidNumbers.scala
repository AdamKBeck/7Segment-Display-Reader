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
		0 -> booleanRepresentedNumber(1, 2, 4, 5, 7)
		1 -> booleanRepresentedNumber(4, 7)
		2 -> booleanRepresentedNumber(1, 3, 4, 5, 6)
		3 -> booleanRepresentedNumber(1, 3, 4, 6, 7)
		4 -> booleanRepresentedNumber(2, 3, 4, 7)
		5 -> booleanRepresentedNumber(1, 2, 3, 6, 7)
		6 -> booleanRepresentedNumber(1, 2, 3, 5, 6, 7)
		7 -> booleanRepresentedNumber(1, 4, 7)
		8 -> booleanRepresentedNumber(1, 2, 3, 4, 5, 6, 7)
		9 -> booleanRepresentedNumber(1, 2, 3, 4, 6, 7)
	)

	/* Returns a 7-segmented number represented as a list of size 7 of booleans denoting which segments are on */
	private def booleanRepresentedNumber(ints:Int*): List[Boolean] = {
		val number= Array[Boolean](false, false, false, false, false, false, false)

		for (i <- ints) {
			number(i) = true
		}

		number.toList
	}


}
