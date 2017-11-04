/* This class stores information on parsed numbers from lines from an input text file
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
case class ParsedNumber(private val _segments: Int*) {
	def segments = _segments.sorted.toList
}
