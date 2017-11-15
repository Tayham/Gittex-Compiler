package edu.towson.cosc.cosc455.thamilton.project1

object Compiler {
  var filename: String = "" //name of the file
  var fileContent: String = "" //content of the file
  var currentToken: String = ""
  val lexical = new MyLexicalAnalyzer
  val syntax = new MySyntaxAnalyzer


  def main(args: Array[String]): Unit = {
    checkFile(args)
//    filename = "test/" + args(0) //so it can be used later in semantic and only the file name needs to be passed
    filename = args(0)
    readFile(filename)

    //compilation start
    lexical.getNextToken()
    syntax.gittex()
    val semantic = new MySemanticAnalyzer(filename.dropRight(4)) // removed ".gtx" from file name
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