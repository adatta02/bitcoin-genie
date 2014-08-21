package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import scala.util.Random
import play.api.libs.json._

case class DealOrNoDealBoard(pos: Int, isPlayed: Boolean, amount: Double)

object DealOrNoDealBoard {
  implicit def boardToJson = Json.format[DealOrNoDealBoard]
}

object DealOrNoDeal {  
  
  val availableAmounts = List(.01, 1, 5, 10, 25, 50, 75, 100, 200, 300, 
		  					   400, 500, 750, 1000, 5000, 10000, 25000, 
		  					   50000, 75000, 100000, 200000, 300000, 400000, 
		  					   500000, 750000, 1000000)
  
  def getNewBoard(): List[DealOrNoDealBoard] = {    
    Random.shuffle(availableAmounts)
    	  .zipWithIndex
    	  .map( {case(amount, pos) => DealOrNoDealBoard(pos, false, amount)} )
  }
  
}