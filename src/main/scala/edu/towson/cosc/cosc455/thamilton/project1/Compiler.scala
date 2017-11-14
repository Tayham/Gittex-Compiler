package edu.towson.cosc.cosc455.thamilton.project1

object Compiler {

  var filename: String = ""
  var fileContent: String = ""

  var index: Int = -1
  var currentToken: String = ""
  val lexical = new MyLexicalAnalyzer
  val syntax = new MySyntaxAnalyzer


  def main(args: Array[String]): Unit = {
    checkFile(args)
    filename = args(0)
    readFile(args(0))

    //compilation start
    lexical.getNextToken()
    syntax.gittex()
    val semantic = new MySemanticAnalyzer(filename.dropRight(4))
    semantic.run()
  }

  def readFile(file: String): Unit = {
    val source = scala.io.Source.fromFile(file)
    fileContent = try source.mkString finally source.close()
    println(fileContent)
  }

  def checkFile(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
