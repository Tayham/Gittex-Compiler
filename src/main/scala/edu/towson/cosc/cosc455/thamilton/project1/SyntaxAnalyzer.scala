package edu.towson.cosc.cosc455.thamilton.project1


trait SyntaxAnalyzer {
  def gittex(): Unit

  def title(): Unit

  def body(): Unit

  def paragraph(): Unit

  def innerText(): Unit

  def heading(): Unit

  def variableDefine(): Unit

  def variableUse(): Unit

  def bold(): Unit

  def listItem(): Unit

  def innerItem(): Unit

  def link(): Unit

  def image(): Unit

  def newline(): Unit
}
