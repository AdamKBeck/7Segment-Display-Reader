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
}

object ValidNumbers {
	def numberCache = _numberCache

	private final val _numberCache = Map(
		booleanSegmentedNumber(List(1, 2, 4, 5, 7)) -> 0,
		booleanSegmentedNumber(List(4, 7)) -> 1,
		booleanSegmentedNumber(List(1, 3, 4, 5, 6)) -> 2,
		booleanSegmentedNumber(List(1, 3, 4, 6, 7)) -> 3,
		booleanSegmentedNumber(List(2, 3, 4, 7)) -> 4,
		booleanSegmentedNumber(List(1, 2, 3, 6, 7)) -> 5,
		booleanSegmentedNumber(List(1, 2, 3, 5, 6, 7)) -> 6,
		booleanSegmentedNumber(List(1, 4, 7))-> 7,
		booleanSegmentedNumber(List(1, 2, 3, 4, 5, 6, 7)) -> 8,
		booleanSegmentedNumber(List(1, 2, 3, 4, 6, 7)) -> 9
	)

	/* Returns a 7-segmented number represented as a list of size 7 of booleans denoting which segments are on */
	def booleanSegmentedNumber(segments: List[Int]): List[Boolean] = {
		val number = Array[Boolean](false, false, false, false, false, false, false)

		for (i <- segments) {
			number(i-1) = true
		}

		number.toList
	}
}
