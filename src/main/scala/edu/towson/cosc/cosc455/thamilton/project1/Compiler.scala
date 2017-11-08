package edu.towson.cosc.cosc455.thamilton.project1

import scala.io.Source._

object Compiler{

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()
    println(lines)
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }


}
