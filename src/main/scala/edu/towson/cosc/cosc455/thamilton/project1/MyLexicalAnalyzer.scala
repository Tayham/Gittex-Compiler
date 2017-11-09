package edu.towson.cosc.cosc455.thamilton.project1

import edu.towson.cosc.cosc455.thamilton.project1.CONSTANTS._
import edu.towson.cosc.cosc455.thamilton.project1.Compiler._

class MyLexicalAnalyzer extends LexicalAnalyzer {

  var pos: Int = -1
  var next: Char = ' '
  var token: String = ""
  var reachedEnd: Boolean = false

  override def addChar(): Unit = {
    token = token + next
  }

  override def getChar(): Char = {
    pos = pos + 1
    if (pos < fileContent.length) {
      next = fileContent.charAt(pos)
      next
    }
    else {
      reachedEnd = true
      ' '
    }
  }

  override def getNextToken(): Unit = {
    token = ""
    getChar()
    findNextText()

    if (reachedEnd == true) {}
    else if (SPECIALSYMBOLS.contains(next)) {
      token = isValid().map(_.toUpper)
      if (token.endsWith("\n")) {
        token = token.substring(0, token.length - 1) //trims off new line characters
      }
      if (lookup(token)) { //sees if token is valid
        currentToken = token
      }
      else {
        println("[LEXICAL ERROR] '" + token + "' is an invalid token")
        System.exit(1)
      }
    }
    else if (next.isLetterOrDigit || next.equals(':') || next.equals('.') || next.equals(',')) { // If it is text
      addChar()
      token = token + readText()
      if (next.equals(BRACKETE(0)) || next.equals(ADDRESSE(0)) || next.equals(EQSIGN(0)) || next.equals(NEWLINE(0))) {
        pos = pos - 1
      }
      currentToken = token
    }
    else if (ENDLINE.contains(next)) {
      getNextToken()
    }
    else {
      println("[LEXICAL ERROR] '" + next + "' is an invalid character")
      System.exit(1)
    }
  }

  def lookup(token: String): Boolean = { //Returns true if the token is legal
    return KEYWORDS contains token.toUpperCase
  }

  def isValid(): String = { //Determines what type the token is
    if (next.equals(BOLD(0))) bold() // *
    else if (next equals (LISTITEM(0))) listItem() // +
    else if (next equals (NEWLINE(0))) slash() // \
    else if (next equals (HEADING(0))) heading() // #
    else if (next equals (IMAGEB(0))) image() // ! then [
    else if ((next equals BRACKETE(0)) || (next equals LINKB(0)) || (next equals ADDRESSE(0)) || (next equals ADDRESSB(0)) || (next equals EQSIGN(0))) addChar() // other symbols
    token
  }

  def image() = {
    addChar()
    getChar()
    if (next.equals(IMAGEB(1))) {
      addChar()
    }
    else {
      println("[LEXICAL ERROR] '" + next + "' is an invalid character after '!'")
      System.exit(1)
    }
  }

  def heading() = {
    addChar()
    token = token + readText()
  }

  def bold() = {
    addChar()
    getChar()
    pos = pos - 1 // fix for when characters after end bold are eaten
  }

  def listItem() = {
    addChar()
    token = token + readText()
  }

  def slash() = {
    addChar()
    token = token + readText()
    if (next.equals(LINKB(0))) {
      addChar()
    }
    if (token.equalsIgnoreCase(DOCE)) {
      findNextText()
      // if there is text after the \END tag
      if (pos equals fileContent.length != 0) {
        pos = pos - 1
        getNextToken()
        println("[SYNTAX ERROR] '" + currentToken + " is placed after the document end'")
        System.exit(1)
      }
    }
  }

  def readText(): String = { //Gets characters until next token or end
    var text: String = ""
    getChar()

    while (!next.isSpaceChar && !(ENDLINE contains next) && !(SPECIALSYMBOLS contains next) && !reachedEnd) {
      text = text + next
      getChar()
    }
    if (next.equals('\n')) {
      text = text + next
    }
    if (next.equals('\r')) {
      getChar()
      if (next.equals('\n')) {
        text = text + next
      }
    }

    return text
  }

  def findNextText(): Unit = { //Calls get char until a non space character is found
    while ((WHITESPACE contains next) && !reachedEnd) {
      getChar()
    }
  }
}