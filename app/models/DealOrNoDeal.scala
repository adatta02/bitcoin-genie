package models

import anorm._
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.current
import scala.collection._
import scala.util.Random
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
6 boxes (20)
= banker offer
5 boxes (14)
= banker offer
4 boxes (10)
= banker offer
3 boxes (7)
= banker offer
2 boxes (5)
= banker offer
1 box (4)
= banker offer
1 box (3)
= banker offer
1 box (2)
= banker offer
1 box (1)
== Final case reveal ==
 */

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

case class DealOrNoDealBoard(boxes: List[DealOrNoDealBox], currentOffer: Option[Double], 
							 selectedBox: Option[Int], pastOffers: List[Double]) {
  
  def seletBox(pos: Int): DealOrNoDealBoard = {
    if( selectedBox.isDefined ){
      throw new Exception("Your case is already selected.")
    }
    
    val updatedBox = DealOrNoDealBoard(this.boxes, this.currentOffer, Option[Int](pos), this.pastOffers)
    updatedBox.openBox(pos)
  }
  
  def openBox(pos: Int): DealOrNoDealBoard = {    
    val newBoxes = this.boxes.map(a => {
      if( a.pos == pos ){
        DealOrNoDealBox( a.pos, true, a.amount )
      }else{
        a
      }
    })
    
    DealOrNoDealBoard(newBoxes, this.currentOffer, this.selectedBox, this.pastOffers)
  }
  
  def getJsonForView: JsValue = {
    val boxes = Json.toJson( this )
    				.\("boxes").as[ Seq[JsObject] ]
    				.map(a => {a ++ Json.obj( "amount" -> "" )})
    				
    val amountList = this.boxes.sortBy(el => el.amount).map(a => {
      val isPlayed = if ( this.selectedBox.isDefined && a.pos == this.selectedBox.get ) {
        true
      }else{
        !a.isPlayed
      }      
      Json.obj( "amount" -> a.amount, "available" -> isPlayed ) 
    })
    
    Json.obj( "boxes" -> boxes,
    		  "amounts" -> amountList,
    		  "selectedBox" -> this.selectedBox,    		  
    		  "currentOffer" -> this.currentOffer,
    		  "boxesToPick" -> this.getBoxesToPick )    
  }
  
  def getBoxesToPick: Int = {
    val numAvailable = this.boxes.filter(a => !a.isPlayed).length
    
    val num = if( numAvailable > 20 ){
      6 - (26 - numAvailable)
    }else if( numAvailable <= 20 && numAvailable > 15 ){
      5 - (20 - numAvailable)
    }else if( numAvailable <= 15 && numAvailable > 11 ){
      4 - (14 - numAvailable)
    }else if( numAvailable <= 11 && numAvailable > 8 ){
      3 - (11 - numAvailable)
    }else if( numAvailable <= 8 && numAvailable > 6 ){
      2 - (8 - numAvailable)
    }else if( numAvailable <= 6 && numAvailable >= 2 ){
      1
    }else{
      -1
    }
    
    println( numAvailable )
    println( num )
    
    num
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