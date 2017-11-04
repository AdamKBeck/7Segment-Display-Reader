package number_recognition

/* This class stores parsed number segments from lines from an input text file
 *
 * The possible segments are:
 * _
 *|_|
 *|_|
 *
 * 1
 *234
 *567
 */
case class ParsedNumber(private val _segments: List[Int]) {
	def segments = _segments.sorted.toList
}
