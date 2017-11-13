package edu.towson.cosc.cosc455.thamilton.project1

import edu.towson.cosc.cosc455.thamilton.project1.CONSTANTS._
import edu.towson.cosc.cosc455.thamilton.project1.Compiler._

import scala.collection.mutable

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var parser = new mutable.Stack[String]

  override def gittex(): Unit = {
    if (currentToken.equalsIgnoreCase(DOCB)) {
      pushGet()
      variableDefine()
      title()
      body()
      if (currentToken.equalsIgnoreCase(DOCE)) parser.push(currentToken)
      else error(DOCE)
    }
    else error(DOCB)
  }

  override def title(): Unit = {
    if (currentToken.equalsIgnoreCase(TITLEB)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(BRACKETE)) pushGet()
      else error(BRACKETE)
    }
    else error(TITLEB)
  }

  override def body(): Unit = {
    if (currentToken.equalsIgnoreCase(PARAB) || currentToken.equalsIgnoreCase(PARAE)) {
      paragraph()
      body()
    }
    else if (currentToken.equalsIgnoreCase(NEWLINE)) {
      newline()
      body()
    }
    else if (lexical.reachedEnd) {}
    else {
      innerText()
      body()
    }
  }

  override def paragraph(): Unit = {
    if (currentToken.equalsIgnoreCase(PARAB)) {
      pushGet()
      if (currentToken.equalsIgnoreCase(DEFB)) {
        variableDefine()
      }
      innerText()

      if (currentToken.equalsIgnoreCase(PARAE)) pushGet()
      else error(PARAE)
    }
    else error(PARAB)

  }

  override def innerText(): Unit = {
    if (currentToken.equalsIgnoreCase(USEB)) {
      variableUse()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(HEADING)) {
      heading()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(BOLD)) {
      bold()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(LISTITEM)) {
      listItem()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(IMAGEB)) {
      image()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(LINKB)) {
      link()
      innerText()
    }
    else if (currentToken.equalsIgnoreCase(NEWLINE)) {
      newline()
      innerText()
    }
    else if (lexical.reachedEnd) {}
    else if (validText()) {
      pushGet()
      innerText()
    }
  }

  override def heading(): Unit = {
    if (currentToken.equalsIgnoreCase(HEADING)) {
      pushGet()
      text()
    }
  }

  override def variableDefine(): Unit = {
    if (currentToken.equalsIgnoreCase(DEFB)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(EQSIGN)) {
        pushGet()
        text()
        if (currentToken.equalsIgnoreCase(BRACKETE)) {
          pushGet()
          variableDefine()
        }
        else error(BRACKETE)
      }
      else error(EQSIGN)
    }
  }

  override def variableUse(): Unit = {
    if (currentToken.equalsIgnoreCase(USEB)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(BRACKETE)) {
        parser.push(currentToken)
        lexical.getNextToken()
      }
      else error(BRACKETE)
    }
  }

  override def bold(): Unit = {
    if (currentToken.equalsIgnoreCase(BOLD)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(BOLD)) pushGet()
      else error(BOLD)
    }
  }

  override def listItem(): Unit = {
    if (currentToken.equalsIgnoreCase(LISTITEM)) pushGet()
  }

  override def innerItem(): Unit = {
    if (currentToken.equalsIgnoreCase(USEB)) {
      variableUse()
      innerItem()
    }
    else if (currentToken.equalsIgnoreCase(BOLD)) {
      bold()
      innerItem()
    }
    else if (currentToken.equalsIgnoreCase(LINKB)) {
      link()
      innerItem()
    }
    else if (lexical.reachedEnd) {}
    else if (validText()) {
      pushGet()
      innerItem()
    }
  }

  override def link(): Unit = {
    if (currentToken.equalsIgnoreCase(LINKB)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(BRACKETE)) {
        pushGet()
        if (currentToken.equalsIgnoreCase(ADDRESSB)) {
          pushGet()
          pushGet()
          if (currentToken.equalsIgnoreCase(ADDRESSE)) pushGet()
          else error(ADDRESSE)
        }
        else error(ADDRESSB)
      }
      else error(BRACKETE)
    }
  }

  override def image(): Unit = {
    if (currentToken.equalsIgnoreCase(IMAGEB)) {
      pushGet()
      text()
      if (currentToken.equalsIgnoreCase(BRACKETE)) {
        pushGet()
        if (currentToken.equalsIgnoreCase(ADDRESSB)) {
          pushGet()
          pushGet()
          if (currentToken.equalsIgnoreCase(ADDRESSE)) pushGet()
          else error(ADDRESSE)
        }
        else error(ADDRESSB)
      }
      else error(BRACKETE)
    }
  }

  override def newline(): Unit = {
    if (currentToken.equalsIgnoreCase(NEWLINE)) pushGet()
  }

  def text(): Unit = {
    if (validText()) {
      pushGet()
      text()
    }
    else if (lexical.reachedEnd) {}
  }

  def validText(): Boolean = { //Compares filter length with non-filtered length to determine if the selection is valid text
    currentToken.length == currentToken.filter(x => x.isLetterOrDigit || (ENDLINE contains x) || (PUNCTUATION contains x)).length
  }

  def error(expected: String): Unit = {
    println("[SYNTAX ERROR] Found \"" + currentToken + "\" but was expecting \"" + expected + "\"")
    System.exit(1)
  }

  def pushGet(): Unit = { //pushes current token to parse tree and gets next token
    parser.push(currentToken)
    lexical.getNextToken()
  }
}
