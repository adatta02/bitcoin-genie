package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import play.api.libs.Codecs
import java.security.MessageDigest

abstract class BaseGamePhrase(id: Integer, pharse: String) {  
  def id: Integer
  def phrase: String  
}

case class PhraseWithBlank(id: Integer, phrase: String) extends BaseGamePhrase(id, phrase)
case class Phrase(id: Integer, phrase: String) extends BaseGamePhrase(id, phrase)

object GamePhrase {

  val TYPE_WITH_BLANK_PHRASE = 1
  val TYPE_PHRASE = 2
  
  def calculatePhraseHash(phrase: String): String = {
    val sha = MessageDigest.getInstance("SHA-256")    
    Codecs.toHexString(sha.digest(Codecs.toHexString(sha.digest(phrase.getBytes())).getBytes()))    
  }
  
  def doesHashMatch(hash: String, key: AvailableKey): Boolean = {
    key.publicKey.last == hash.last
  }
  
  def rowParser() = {
    
    (int("id") ~ str("phrase") ~ int("phrase_type")).map(f => {      
      f match {
	  	case i~p~t => 
	  	  if( t == TYPE_WITH_BLANK_PHRASE ){ 
	  	    PhraseWithBlank(i, p)
	  	  }else{
	  	    Phrase(i, p)
	  	  }
      }
    })
    
  }
    
  def getRandomPhrases: List[Phrase] = {
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM golf_phrase WHERE phrase_type = {type} ORDER BY RAND() LIMIT 6")
      	 .on("type" -> TYPE_PHRASE)
      	 .as(rowParser *)
      	 .map( a => a.asInstanceOf[Phrase] )
    })    
  }
  
  def getRandomBlankPhrase: PhraseWithBlank = {
          
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM golf_phrase WHERE phrase_type = {type} ORDER BY RAND() LIMIT 1")
      	 .on("type" -> TYPE_WITH_BLANK_PHRASE)
      	 .as(rowParser.single).asInstanceOf[PhraseWithBlank]
    })
    
  }
  
}