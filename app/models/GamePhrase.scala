package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._

abstract class BaseGamePhrase(id: Integer, pharse: String) {  
  def id: Integer
  def phrase: String  
}

case class PhraseWithBlank(id: Integer, phrase: String) extends BaseGamePhrase(id, phrase)
case class Phrase(id: Integer, phrase: String) extends BaseGamePhrase(id, phrase)

object GamePhrase {

  val TYPE_WITH_BLANK_PHRASE = "1"
  val TYPE_PHRASE = "2"
  
  def rowParser = {
    
    (int("id") ~ str("phrase")).map(f => {      
      f match {    
	  	case i~p => PhraseWithBlank(i, p)
      }
    })
    
  }
    
  def getRandomBlankPhrase: PhraseWithBlank = {
          
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM golf_phrase WHERE phrase_type = {type} ORDER BY RAND() LIMIT 1")
      	 .on("type" -> TYPE_WITH_BLANK_PHRASE)
      	 .as(rowParser.single)
    })    
    
  }
  
}