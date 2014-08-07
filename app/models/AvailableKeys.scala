package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._

case class AvailableKey(id: Int, publicKey: String, amount: Double, game: String){
  def getReadableGame: String = {
    this.game match {
      case "golf" => "Hash Golf"
      case "photobooth" => "Photobooth Fun!"
      case "craps" => "Craps"
      case _=> "N/A"
    }
  }
}

object AvailableKeys {
  
  def rowParser = {
    (int("id") ~ str("public_key") ~ get[Double]("amount") ~ str("game"))
    .map(f => {      
      f match {    
	  	case i~pk~a~g => AvailableKey(i, pk, a, g)
      }
    })
  }
    
  def find(id: Int): Option[AvailableKey] = {
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM available_keys WHERE id = {id} LIMIT 1").on("id" -> id).as(rowParser.singleOpt)
    })    
  }
  
  def getAllAvailableKeys: List[AvailableKey] = {
    
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM available_keys ORDER BY id ASC").as(rowParser *).toList
    })
    
  }
  
}