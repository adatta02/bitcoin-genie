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

case class DealOrNoDealBox(pos: Int, isPlayed: Boolean, amount: Double, playIndex: Option[Int]) 

object DealOrNoDealBox {
  implicit def boxToJson = Json.format[DealOrNoDealBox]
}

object DealOrNoDealBoard {
    
  implicit def boardToJson = new Writes[DealOrNoDealBoard] {
    def writes(board: DealOrNoDealBoard) = { 
      Json.obj("currentOffer" -> board.getBankerOffer,
    		   "selectedBox" -> board.selectedBox,
    		   "boxes" -> Json.toJson(board.boxes), 
    		   "pastOffers" -> Json.toJson(board.pastOffers),
    		   "didSwitch" -> board.didSwitch)
    }
  }  
  
  implicit val jsonToBoard: Reads[DealOrNoDealBoard] = (
     (__ \"boxes").read[ List[DealOrNoDealBox] ] 
     and (__ \ "currentOffer").read[Option[Double]]
     and (__ \ "selectedBox").read[Option[Int]]     
     and (__ \"pastOffers").read[List[Double]]
     and (__ \ "didSwitch").read[Option[Boolean]]
  )(DealOrNoDealBoard.apply _)
  
}

case class DealOrNoDealBoard(boxes: List[DealOrNoDealBox], currentOffer: Option[Double], 
							 selectedBox: Option[Int], pastOffers: List[Double], didSwitch: Option[Boolean])  {
    
  def seletBox(pos: Int): DealOrNoDealBoard = {
    if( selectedBox.isDefined ){
      throw new Exception("Your case is already selected.")
    }
    
    val updatedBox = DealOrNoDealBoard(this.boxes, this.currentOffer, Option[Int](pos), this.pastOffers, None)
    updatedBox.openBox(pos)
  }
  
  def openBox(pos: Int): DealOrNoDealBoard = {
    if(this.getNumAvailable == 1){
      this
    }else{
      this.doBox(pos)
    }
  }
  
  def doBox(pos: Int): DealOrNoDealBoard = {    
    val numPlayed = this.boxes.filter(a => a.isPlayed).length        
    val newBoxes = this.boxes.map(a => {
      if( a.pos == pos ){
        DealOrNoDealBox( a.pos, true, a.amount, Option[Int](numPlayed) )
      }else{
        a
      }
    })
    
    val updatedBoard = DealOrNoDealBoard(newBoxes, this.getBankerOffer, this.selectedBox, this.pastOffers, None)
    
    val finalBoard = if( updatedBoard.getBankerOffer.isDefined ){      
      DealOrNoDealBoard(newBoxes, updatedBoard.getBankerOffer, updatedBoard.selectedBox, 
    		  			updatedBoard.pastOffers ::: List(updatedBoard.getBankerOffer.get), None)
    }else{
      updatedBoard
    }
    
    finalBoard
  }
  
  def getJsonForView: JsValue = {
    val boxes = Json.toJson( this )
    				.\("boxes").as[ Seq[JsObject] ]
    				.map(a => {
    				  if( a.\("isPlayed").as[Boolean] ){
    				    a
    				  }else{
    				   a ++ Json.obj( "amount" -> "" ) 
    				  }  				  
    				})
    				
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
    		  "currentOffer" -> this.getBankerOffer,    		  
    		  "boxesToPick" -> this.getBoxesToPick,    		  
    		  "lastAmount" -> this.getLastAmount,
    		  "canSwitch" -> this.canSwitch,
    		  "yourBoxValue" -> this.getSelectedBoxValue,
    		  "pastOffers" -> this.pastOffers )    
  }
  
  def getLastAmount: Option[Double] = {
    val playedBoxes = this.boxes.filter(a => a.isPlayed).sortBy( a => a.playIndex )
    
    if( playedBoxes.length > 1 ){
      Option[Double](playedBoxes.last.amount)
    }else{
      None
    }
  }
  
  def getBankerOffer: Option[Double] = {
        
    val numAvailable = this.getNumAvailable
    
    if( List(20, 15, 11, 8, 6).contains(numAvailable) || ( numAvailable <= 6 && numAvailable >= 2 ) ){
      
      val offer = this.boxes.filter(a => !a.isPlayed)
        .foldLeft(0.0)((total, el) => {
          total + ( el.amount * (1.0 / numAvailable.toDouble) )
      })      
     
      Option[Double]( offer.formatted("%.2f").toDouble )
    }else{
      None
    }
    
  }
  
  def getBoxesToPick: Int = {
    val numAvailable = this.getNumAvailable
    
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
    
    num
  }
  
  def canSwitch: Boolean = {
    if( this.didSwitch.isDefined == false && this.getNumAvailable == 1 ){
      true
    }else{
      false
    }
  }
  
  def getSelectedBoxValue: Option[Double] = {    
    if(this.didSwitch.isEmpty){
      None
    }else{
      Option[Double](this.boxes.find(a => a.pos == this.selectedBox.get).get.amount)
    }
  }
  
  def switchCase: DealOrNoDealBoard = {
    
    if( !this.canSwitch ){
      return this
    }
    
    val switchedCase = this.boxes.find(a => !a.isPlayed)
    
    if(switchedCase.isEmpty){
      throw new Exception("Could not find an unplayed box?")
    }
    
    val boxes = this.boxes.map(a => {
      if(a.pos == this.selectedBox.get){
        DealOrNoDealBox(a.pos, false, a.amount, a.playIndex)
      }else if(a.pos == switchedCase.get.pos){
        DealOrNoDealBox(a.pos, true, a.amount, a.playIndex)
      }else{
        DealOrNoDealBox(a.pos, a.isPlayed, a.amount, a.playIndex)
      }
    })
    
    DealOrNoDealBoard(boxes, this.currentOffer, Option[Int](switchedCase.get.pos), this.pastOffers, Option[Boolean](true))
  }
  
  def declineSwitchCase: DealOrNoDealBoard = {
    if( !this.canSwitch ){
      this
    }
    
    DealOrNoDealBoard(this.boxes, this.currentOffer, this.selectedBox, this.pastOffers, Option[Boolean](false))
  }
  
  def getNumAvailable: Int = {
    this.boxes.filter(a => !a.isPlayed).length
  }
  
}

object DealOrNoDeal {  
  
  val availableAmounts = List(.01, 1, 5, 10, 25, 50, 75, 100, 200, 300, 
		  					   400, 500, 750, 1000, 5000, 10000, 25000, 
		  					   50000, 75000, 100000, 200000, 300000, 400000, 
		  					   500000, 750000, 1000000)
  
  
  def getNewBoard(): DealOrNoDealBoard = {    
    val boxes = Random.shuffle(availableAmounts).zipWithIndex
    				  .map( {case(amount, pos) => DealOrNoDealBox(pos, false, amount, None)} )
    				  
    DealOrNoDealBoard(boxes, None, None, List(), None)
  }
  
}