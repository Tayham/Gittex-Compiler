package edu.towson.cosc.cosc455.thamilton.project1
object CONSTANTS {
  val DOCB        : String =  "\\BEGIN"
  val DOCE        : String =  "\\END"
  val TITLEB      : String =  "\\TITLE["
  val BRACKETE    : String =  "]"
  val HEADING     : String =  "#"
  val PARAB       : String =  "\\PARAB"
  val PARAE       : String =  "\\PARAE"
  val BOLD        : String =  "*"
  val LISTITEM    : String =  "+"
  val NEWLINE     : String =  "\\\\"
  val LINKB       : String =  "["
  val ADDRESSB    : String =  "("
  val ADDRESSE    : String =  ")"
  val IMAGEB      : String =  "!["
  val DEFB        : String =  "\\DEF["
  val EQSIGN      : String =  "="
  val USEB        : String =  "\\USE["

  val SPECIALSYMBOLS: Array[Char] = Array('*', '+', '=', '\\', '!', '#', '[', ']', '(', ')')
  val KEYWORDS: Array[String] = Array(DOCB, DOCE, TITLEB, BRACKETE, HEADING, PARAB, PARAE, BOLD, LISTITEM, NEWLINE, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)
  val ENDLINE: Array[Char] = Array('\t', '\n', '\b','\f','\r')
  val WHITESPACE : Array[Char] = Array(' ', '\t', '\n', '\b','\f','\r')
  val PUNCTUATION: Array[Char] = Array(',' , '.', '\"', ':', '?', '_', '/')

}
