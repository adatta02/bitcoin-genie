package controllers

import models._
import play.api.mvc.{Action, Controller, Result}
import play.api.libs.json._

object Golf extends Controller {

  def getHash = Action(parse.json) {request => {
    
    val jsonResult = request.body match {
	    case JsObject(fields) => {
	      val phrase = fields.find(a => a._1 == "phrase").get._2.toString
	      val id = Integer.valueOf(fields.find(a => a._1 == "id").get._2.toString)
	      
	      val hash = GamePhrase.calculatePhraseHash(phrase)
	      val availableGame = AvailableKeys.find(id).get
	      val isMatch = GamePhrase.doesHashMatch(hash, availableGame)	      
	      
	      val redeemUrl = if( isMatch ){
	        routes.Application.redeem( availableGame.redeemKey ).url
	      }else{
	        ""
	      }
	      
	      Json.toJson( Json.obj("error" -> false, "calculatedHash" -> hash, "redeemUrl" -> redeemUrl, "isMatch" -> isMatch) )
	    }
	    case _ => Json.toJson( Json.obj("error" -> true, "msg" -> "Sorry! That isn't valid JSON.") )
	  }
    
    Ok( jsonResult )
  }}   
  
  def play(game: AvailableKey): Result = {
    val availableWords = GamePhrase.getRandomPhrases
    val gamePhrase = GamePhrase.getRandomBlankPhrase
    
    val targetPhrase = {
      if( gamePhrase.phrase.split("%").length == 1 ){         
        gamePhrase.phrase.split("%").toList :+ "."
      }else{
        gamePhrase.phrase.split("%").toList  
	  }
    }
    
    Ok( views.html.game(game, targetPhrase, availableWords) )
  }
  
  def getRandomPhrases() = Action {
    Ok( views.html.renderPhrases(GamePhrase.getRandomPhrases) )
  }
  
}