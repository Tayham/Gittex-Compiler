package edu.towson.cosc.cosc455.thamilton.project1

import edu.towson.cosc.cosc455.thamilton.project1.CONSTANTS._
import edu.towson.cosc.cosc455.thamilton.project1.Compiler._

class MyLexicalAnalyzer extends LexicalAnalyzer {

  var pos: Int = -1 //position the analyzer is at in the file content
  var next: Char = ' '
  var token: String = ""
  var reachedEnd: Boolean = false

  // adds char to the current token
  override def addChar(): Unit = {
    token = token + next
  }

  // gets next char while file has not reached the end
  override def getChar(): Char = {
    pos = pos + 1 //inc pos
    if (pos < fileContent.length) {
      next = fileContent.charAt(pos)
      next //returns next
    }
    else {
      reachedEnd = true
      '\0' //placeholder
    }
  }

  // gets the next token located in the file
  override def getNextToken(): Unit = {
    token = ""
    getChar()
    findNextText()

    if (reachedEnd) {} //do nothing
    else if (SPECIALSYMBOLS.contains(next)) {
      token = isValid.map(_.toUpper) // standardize to upper case
      if (token.endsWith("\n")) {
        token = token.substring(0, token.length - 1) //trims off new line characters
      }
      if (lookup(token)) { //sees if token is valid
        currentToken = token
      }
      else {
        error(token, "token")
      }
    }
    else if (next.isLetterOrDigit || PUNCTUATION.contains(next)) { // If it is text
      addChar()
      token = token + readText()
      if (next.equals(BRACKETE(0)) || next.equals(ADDRESSE(0)) || next.equals(EQSIGN(0)) || next.equals(NEWLINE(0))) { //moves pos back one to stop skipping
        pos = pos - 1
      }
      currentToken = token
    }
    else if (ENDLINE.contains(next)) { //if next is an end line
      getNextToken()
    }
    else {
      error(next.toString, "character")
    }
  }

  def lookup(token: String): Boolean = { //Returns true if the token is legal
    KEYWORDS contains token.toUpperCase
  }

  def isValid: String = { //Determines what type the token is
    if (next.equals(BOLD(0))) bold() // *
    else if (next equals LISTITEM(0)) listItem() // +
    else if (next equals NEWLINE(0)) slash() // \
    else if (next equals HEADING(0)) heading() // #
    else if (next equals IMAGEB(0)) image() // ! then [
    else if ((next equals BRACKETE(0)) || (next equals LINKB(0)) || (next equals ADDRESSE(0)) || (next equals ADDRESSB(0)) || (next equals EQSIGN(0))) addChar() // other symbols
    token
  }

  def image(): Unit = {
    addChar()
    getChar()
    if (next.equals(IMAGEB(1))) {
      addChar()
    }
    else {
      error(next.toString, "character after \"!\"")
    }
  }

  def heading(): Unit = {
    addChar()
    token = token + readText()
  }

  def bold(): Unit = {
    addChar()
  }

  def listItem(): Unit = {
    addChar()
    token = token + readText()
  }

  def slash(): Unit = {
    addChar()
    token = token + readText()
    if (next.equals(NEWLINE(1))) {
      addChar()
    }
    if (next.equals(LINKB(0))) {
      addChar()
    }
    if (token.equalsIgnoreCase(DOCE)) {
      findNextText()
      // if there is text after the \END tag
      if (pos != fileContent.length) {
        pos = pos - 1
        getNextToken()
        println("[SYNTAX ERROR]: " + currentToken + " was found after the document end") // doesn't work without syntax analyzer
        System.exit(1)
      }
    }
  }

  def readText(): String = { //Gets characters until next token or end
    var text: String = ""
    getChar()
    while (!(WHITESPACE contains next) && !(SPECIALSYMBOLS contains next) && !reachedEnd) { // while it is text
      text = text + next
      getChar()
    }
    if (next.equals('\n')) { // stops errors on new line
      text = text + next
    }
    if (next.equals('\r')) { // stops errors on new line
      getChar()
      if (next.equals('\n')) {
        text = text + next
      }
    }
    text
  }

  //Calls get char until a non space character is found
  def findNextText(): Unit = {
    while ((WHITESPACE contains next) && !reachedEnd) {
      getChar()
    }
  }

  // simple method that prints error and exits
  def error(invalid: String, kind: String): Unit = {
    println("[LEXICAL ERROR] \"" + invalid + "\" is an invalid " + kind)
    System.exit(1)
  }
}