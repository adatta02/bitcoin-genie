package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import scala.util.Random
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class DealOrNoDealBox(pos: Int, isPlayed: Boolean, amount: Double)

object DealOrNoDealBox {
  implicit def boxToJson = Json.format[DealOrNoDealBox]
}

object DealOrNoDealBoard {
    
  implicit def boardToJson = new Writes[DealOrNoDealBoard] {
    def writes(board: DealOrNoDealBoard) = { 
      Json.obj("currentOffer" -> board.currentOffer,
    		   "selectedBox" -> board.selectedBox,
    		   "boxes" -> Json.toJson(board.boxes), 
    		   "pastOffers" -> Json.toJson(board.pastOffers))
    }
  }  
  
  implicit val jsonToBoard: Reads[DealOrNoDealBoard] = (
     (__ \"boxes").read[ List[DealOrNoDealBox] ] 
     and (__ \ "currentOffer").read[Option[Double]]
     and (__ \ "selectedBox").read[Option[Int]]     
     and (__ \"pastOffers").read[List[Double]]      
  )(DealOrNoDealBoard.apply _)
  
}

case class DealOrNoDealBoard(boxes: List[DealOrNoDealBox], currentOffer: Option[Double], selectedBox: Option[Int], pastOffers: List[Double]) {
  
  def getJsonForView: JsValue = {
    val boxes = Json.toJson( this )
    				.\("boxes").as[ Seq[JsObject] ]
    				.map(a => {a ++ Json.obj( "amount" -> "" )})
    				
    val amountList = this.boxes.sortBy(el => el.amount)
    amountList.map(a => {      
      val isPlayed = if ( this.selectedBox.isDefined && a.pos == this.selectedBox.get ) {
        false
      }else{
        a.isPlayed
      }
      
      Json.obj( "amount" -> a.amount, "available" -> a.isPlayed ) 
    })
    
    Json.obj( "boxes" -> boxes,
    		  "selectedBox" -> this.selectedBox,
    		  "amounts" -> amountList,
    		  "currentOffer" -> this.currentOffer )    
  }
  
}

object DealOrNoDeal {  
  
  val availableAmounts = List(.01, 1, 5, 10, 25, 50, 75, 100, 200, 300, 
		  					   400, 500, 750, 1000, 5000, 10000, 25000, 
		  					   50000, 75000, 100000, 200000, 300000, 400000, 
		  					   500000, 750000, 1000000)
  
  
  def getNewBoard(): DealOrNoDealBoard = {    
    val boxes = Random.shuffle(availableAmounts).zipWithIndex
    				  .map( {case(amount, pos) => DealOrNoDealBox(pos, false, amount)} )
    				  
    DealOrNoDealBoard(boxes, None, None, List())
  }
  
}