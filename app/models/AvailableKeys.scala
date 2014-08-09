package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._

case class AvailableKey(id: Int, publicKey: String, amount: Double, game: String, redeemKey: String){
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
    (int("id") ~ str("public_key") ~ get[Double]("amount") ~ str("game") ~ str("redeem_key"))
    .map(f => {      
      f match {    
	  	case i~pk~a~g~k => AvailableKey(i, pk, a, g, k)
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