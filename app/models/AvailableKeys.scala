package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import play.api.libs.json._

case class AvailableKey(id: Int, publicKey: String, amount: Option[Double], 
						game: String, redeemKey: String, dealBoard: String, isRedeemed: Boolean){
  
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
    (int("id") ~ str("public_key") ~ get[Option[java.math.BigDecimal]]("amount") 
        ~ str("game") ~ str("redeem_key") ~ str("deal_board") ~ get[Boolean]("is_redeemed"))
    .map(f => {      
      f match { 
	  	case i~pk~a~g~k~b~ir => {
	  	  val amnt = if( a.isDefined ){
	  		  Option[Double]( a.get.doubleValue() )
	  	  }else{
	  	    None
	  	  }	  	  
	  	  AvailableKey(i, pk, amnt, g, k, b, ir)
	  	}
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
  
  def update(game: AvailableKey, params: Tuple2[String, String]*): AvailableKey = {
    val sql = "UPDATE available_key SET " + params.map(a => {a._1 + " = " + "{" + a._1 + "}"}).mkString(", ") + " WHERE id = {id}"
    val idParams = params ++ Seq( ("id" -> game.id) )
    
    DB.withConnection(implicit c => {
      SQL(sql).on( idParams.map( a => (a._1, toParameterValue(a._2)) ): _* ).executeUpdate
    })
   
    this.find( game.id ).get
  }
  
}