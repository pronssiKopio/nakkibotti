package s1


package object telegrambots {


  private def formatHTML(tag: String, text: String, attributes: Option[Seq[(String, String)]]= None) = {
    val attributeList = attributes.getOrElse(List())
    var attrString = ""
    for(attr <- attributeList){
      attrString += attr._1 +"=\"" + attr._2 + "\" "
    }
    s"<$tag $attrString>$text</$tag>"
  }

  /** Italicizes the text given to it*/
  def italicize(text: String)    = formatHTML("i", text)

  /** Bolds the text given to it*/
  def bold(text: String)         = formatHTML("b", text)

  /** Formats the given text as program code*/
  def formatAsCode(text: String) = formatHTML("code", text)

  /** Renders the text as preformatted, see HTML "pre"-tag*/
  def preformatted(text: String) = formatHTML("pre", text)

  /** Creates a link to given url with the text given to it*/
  def link(text: String, url:String)      = formatHTML("a", text, Some(Seq(("href", url))))


}
