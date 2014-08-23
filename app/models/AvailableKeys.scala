package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import play.api.libs.json._

case class AvailableKey(id: Int, publicKey: String, amount: Double, 
						game: String, redeemKey: String, dealBoard: String){
  
  def getReadableGame: String = {
    this.game match {
      case "golf" => "Hash Golf"
      case "photobooth" => "Photobooth Fun!"
      case "craps" => "Craps"
      case _=> "Deal or No Deal"
    }
  }
  
  def getDealOrNoDealBoard: DealOrNoDealBoard = {
    val json = Json.parse( this.dealBoard )
    json.validate[DealOrNoDealBoard].get
  }
  
}

object AvailableKeys {
  
  def rowParser = {
    (int("id") ~ str("public_key") ~ get[java.math.BigDecimal]("amount") ~ str("game") ~ str("redeem_key") ~ str("deal_board"))
    .map(f => {      
      f match { 
	  	case i~pk~a~g~k~b => AvailableKey(i, pk, a.toString().toDouble, g, k, b)
      }
    })
  }
  
  def findBy(params: Tuple2[String, String]*): Option[AvailableKey] = {
    val sql = "SELECT * FROM available_key WHERE " + params.map(a => a._1.concat(" = {" + a._1 + "}" )).mkString(" AND ")
    
    DB.withConnection(implicit c => {
      SQL(sql).on( 
          params.map( a => (a._1, toParameterValue(a._2)) ): _*
      ).as(rowParser.singleOpt)
    })
    
  }
  
  def find(id: Int): Option[AvailableKey] = {
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM available_key WHERE id = {id} LIMIT 1").on("id" -> id).as(rowParser.singleOpt)
    })
  }
  
  def getAllAvailableKeys: List[AvailableKey] = {
    
    DB.withConnection(implicit c => {
      SQL("SELECT * FROM available_key ORDER BY id ASC").as(rowParser *).toList
    })
    
  }
  
}