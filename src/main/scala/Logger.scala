package number_recognition
import java.io._
import java.nio.file._

/* This class is used to log errors or warnings to an output file, write answers, as well as keep track if any of the errors are severe.
 * If an error was severe, the program returns failure, as a severe error violates the many assumptions we hold for the
 * input file as described in the homework pdf */
case class Logger private() {
	private var _severeError = false

	def severeError = _severeError

	def severeError_= (value: Boolean):Unit = _severeError = value

	def log(error: String): Unit = {
		val pw = new PrintWriter(new FileWriter("src/log.txt", true))
		pw.write(error + "\n")
		pw.close()
	}

	def delete(): Unit = {
		Files.deleteIfExists(Paths.get("src/log.txt"))
	}
}

object Logger {
	private val _instance = Logger()

	def instance = _instance

	def markSevereError(): Unit = instance.severeError = true

}
