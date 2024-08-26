import scala.io.StdIn._

object StudentManage {
    def getStudentInfo(): (String, Int, Int, Double, Char) = {
        def calculateGrade(percentage: Double): Char = {
            if (percentage >= 90) 'A'
            else if (percentage >= 75) 'B'
            else if (percentage >= 50) 'C'
            else 'D'
        }

        def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
            if (name.isEmpty) (false, Some("Name cannot be empty"))
            else if (marks < 0 || marks > totalMarks) (false, Some("Marks should be between 0 and total marks"))
            else if (totalMarks <= 0) (false, Some("Total possible marks should be positive"))
            else (true, None)
        }

        def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
            var isValid = false
            var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

            while (!isValid) {
                def safeReadString(prompt: String): String = {
                    var inputValid = false
                    var input = ""
                    while (!inputValid) {
                        println(prompt)
                        val inputStr = readLine().trim
                        if (inputStr.isEmpty) {
                            println("Input cannot be empty. Please enter a valid string.")
                        } else {
                            input = inputStr
                            inputValid = true
                        }
                    }
                    input
                }

                def safeReadInt(prompt: String): Int = {
                    var inputValid = false
                    var input = 0
                    while (!inputValid) {
                        println(prompt)
                        val inputStr = readLine().trim
                        if (inputStr.isEmpty) {
                            println("Input cannot be empty. Please enter a valid integer.")
                        } else {
                            try {
                                input = inputStr.toInt
                                inputValid = true
                            } catch {
                                case _: NumberFormatException =>
                                    println("Invalid input. Please enter a valid integer.")
                            }
                        }
                    }
                    input
                }

                val name = safeReadString("Enter student name:")
                val marks = safeReadInt("Enter student marks:")
                val totalMarks = safeReadInt("Enter total possible marks:")

                val validation = validateInput(name, marks, totalMarks)
                if (validation._1) {
                    val percentage = (marks.toDouble / totalMarks) * 100
                    val grade = calculateGrade(percentage)
                    studentInfo = (name, marks, totalMarks, percentage, grade)
                    isValid = true
                } else {
                    println(s"Error: ${validation._2.getOrElse("Unknown error")}")
                }
            }
            studentInfo
        }

        getStudentInfoWithRetry()
    }

    def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
        println(s"Student Name: ${record._1}")
        println(s"Marks: ${record._2}")
        println(s"Total Marks: ${record._3}")
        println(f"Percentage: ${record._4}%.2f%%")
        println(s"Grade: ${record._5}")
    }

    def main(args: Array[String]): Unit = {
        val studentRecord = getStudentInfo()
        printStudentRecord(studentRecord)
    }
}
