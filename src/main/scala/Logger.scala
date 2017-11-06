package number_recognition
import java.io._
import java.nio.file._

/* This class is used to log errors or warnings to an output file, as well as keep track if any of the errors are severe.
 * If an error was severe, the program returns failure, as a severe error violates the many assumptions we hold for the
 * input file as described in the homework pdf */
case class Logger() {

}

object Logger {
	def log(error: String): Unit = {
		val pw = new PrintWriter(new FileWriter("src/log.txt", true))
		pw.write(error)
		pw.close()
	}

	def delete(fileName: String): Unit = {
		Files.deleteIfExists(Paths.get("src/"+fileName))
	}
}
