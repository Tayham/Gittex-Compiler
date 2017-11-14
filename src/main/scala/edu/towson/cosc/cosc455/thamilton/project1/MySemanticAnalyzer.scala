package edu.towson.cosc.cosc455.thamilton.project1

import edu.towson.cosc.cosc455.thamilton.project1.CONSTANTS._
import edu.towson.cosc.cosc455.thamilton.project1.Compiler._

import scala.collection.mutable
import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}


class MySemanticAnalyzer(fName: String) {

  var tokenStack = new mutable.Stack[String]
  var varCheck = new mutable.Stack[String]
  var variables = new mutable.Queue[variable]
  var compScope: Int = 0 // the "depth of the scope" i.e. how many blocks in the variable is scoped
  var token: String = ""
  var temp: String = ""
  var html = new PrintWriter(new File(fName + ".html"))


  def semCheck(): Unit = {
    while (!token.equalsIgnoreCase(DOCB) && syntax.parser.nonEmpty) {
      token = syntax.parser.pop()
      token match {
        case DOCE => tokenStack.push(token)
        case DOCB => tokenStack.push(token)
        case DEFB => varDefCheck()
        case USEB => varUseCheck()
        case _ => tokenStack.push(token)
      }
    }
    if (token.equalsIgnoreCase(DOCB) && !tokenStack.top.equalsIgnoreCase(DOCB)) {
      tokenStack.push(token) // Ensures DOCB is on stack
    }
  }

  def varDefCheck(): Unit = {
    varCheck.push(tokenStack.top)
    tokenStack.push(token)
  }

  def varUseCheck(): Unit = {
    temp = tokenStack.top
    tokenStack.push(token)
    while (!varCheck.contains(temp)) {
      if (syntax.parser.isEmpty && !varCheck.contains(temp)) {
        println("[STATIC SEMANTIC ERROR]: \"" + temp + "\" was never defined")
        System.exit(1)
      }
      semCheck()
    }
  }

  def interpreter() = { // interprets each token in the stack to HTML
    while (!tokenStack.isEmpty) {
      token = tokenStack.pop()
      token match {
        case DOCB => html.append("<html>\n")
        case DOCE => html.append("\n</html>")
        case TITLEB => title()
        case HEADING => heading()
        case PARAB => parab()
        case PARAE => parae()
        case LINKB => linkb()
        case LISTITEM => listitem()
        case NEWLINE => html.append("<br>\n")
        case IMAGEB => imageb()
        case BOLD => bold()
        case DEFB => defb()
        case USEB => useb()
        case _ => html.append(token + " ")
      }
    }
    html.close()
    openHTMLFileInBrowser(fName + ".html")
  }

  def title() = {
    html.append("<head>\n<title> ")
    token = tokenStack.pop()
    while (!token.equalsIgnoreCase(BRACKETE)) {
      html.append(token + " ")
      token = tokenStack.pop()
    }
    html.append(" </title>\n</head>\n")
  }

  def heading() = {
    html.append("<h1> ")
    token = tokenStack.pop()
    while (!KEYWORDS.contains(token)) {
      html.append(token + " ")
      token = tokenStack.pop()
    }
    tokenStack.push(token)
    html.append(" </h1>\n")
  }

  def parab() = {
    html.append("<p> ")
    compScope = compScope + 1
  }

  def parae() = {
    html.append(" </p>\n")
    compScope = compScope - 1
  }

  def linkb(): Unit = {
    temp = ""
    var link: String = ""
    token = tokenStack.pop()
    while (!token.equalsIgnoreCase(BRACKETE)) {
      temp = temp + token + " "
      token = tokenStack.pop()
    }
    tokenStack.pop()
    link = link + tokenStack.pop()
    tokenStack.pop()

    html.append("<a href=\"" + link + "\">" + temp + "</a> ")
  }

  def listitem() = {
    html.append("\n<li> ")
    token = tokenStack.pop()
    if (token.contains("\n")) {
      html.append(token + " </li>")
    }
    else if (token.equalsIgnoreCase(USEB)) {
      tokenStack.push(token)
    }
    else {
      if (!token.equalsIgnoreCase(USEB)) {
        html.append(token + " ")
        token = tokenStack.top
        if (token.contains("\n")) {
          token = tokenStack.pop()
          html.append(token + " </li>")
        }
      }
    }
  }

  def imageb() = {
    temp = ""
    var link: String = ""
    token = tokenStack.pop()
    while (!token.equalsIgnoreCase(BRACKETE)) {
      temp = temp + token + " "
      token = tokenStack.pop()
    }
    tokenStack.pop()
    link = link + tokenStack.pop()
    tokenStack.pop()
    html.append("<img src=\"" + link + "\" alt=" + temp + "\">")
  }

  def bold() = {
    temp = ""
    token = tokenStack.pop()
    while (!token.equalsIgnoreCase(BOLD)) {
      temp = temp + token
      token = tokenStack.pop()
    }
    html.append("<b> " + temp + " </b>")
  }

  def defb() = {
    var variable = new variable()
    variable.scope = compScope
    variable.name = tokenStack.pop()
    tokenStack.pop() //eats =
    variable.value = tokenStack.pop()
    variables.enqueue(variable)
    tokenStack.pop() //eats ]
  }

  def useb() = {
    var temp: String = tokenStack.pop()
    html.append(variables(variables.indexWhere { x => x.name.equalsIgnoreCase(temp) && (x.scope == compScope) }).value + " ")
    tokenStack.pop() // eats ]
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browser. */
  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)

    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")
    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
    }
  }

  def run() = {
    semCheck()
    interpreter()
  }

  class variable {
    var name = ""
    var value = ""
    var scope = 0
  }
}