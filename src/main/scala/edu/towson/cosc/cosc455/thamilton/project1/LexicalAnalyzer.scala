package edu.towson.cosc.cosc455.thamilton.project1


trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup(token : String) : Boolean
}
