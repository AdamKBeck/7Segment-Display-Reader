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

	def log(message: String, fileName:String = "log.txt"): Unit = {
		val pw = new PrintWriter(new FileWriter("src/" + fileName, true))
		pw.write(message + "\n")
		pw.close()
	}

	def delete(fileNames: Set[String]): Unit = {
		for (name <- fileNames) {
			Files.deleteIfExists(Paths.get("src/" + name))
		}

	}

	def createFile(input: List[String], fileName: String): Unit = {
		for (line <- input) {
			log(line, fileName)
		}
	}
}

object Logger {
	private val _instance = Logger()
	instance.delete(Set("log.txt", "input.txt", "output.txt"))

	def instance = _instance

	def markSevereError(): Unit = instance.severeError = true

}
