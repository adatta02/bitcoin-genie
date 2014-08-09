package controllers

import models._
import anorm._
import play.api.mvc.{Action, Controller}
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json._

object Application extends Controller {
  
  def getHash = Action(parse.json) {request => {
    
    val jsonResult = request.body match {
	    case JsObject(fields) => {
	      val phrase = fields.find(a => a._1 == "phrase").get._2.toString
	      val hash = GamePhrase.calculatePhraseHash(phrase)
	      val id = Integer.valueOf(fields.find(a => a._1 == "id").get._2.toString)	      
	      
	      Json.toJson( Json.obj("error" -> false, "calculatedHash" -> hash, 
	    		  				"isMatch" -> GamePhrase.doesHashMatch(hash, AvailableKeys.find(id).get)) )
	    }
	    case _ => Json.toJson( Json.obj("error" -> true, "msg" -> "Sorry! That isn't valid JSON.") )
	  }
    
    Ok( jsonResult )
  }} 
  
  def game(id: Int) = Action {
    
    val game = AvailableKeys.find(id)    
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
    
    val availableWords = GamePhrase.getRandomPhrases
    val gamePhrase = GamePhrase.getRandomBlankPhrase
    
    val targetPhrase = {
      if( gamePhrase.phrase.split("%").length == 1 ){         
        gamePhrase.phrase.split("%").toList :+ "."
      }else{
        gamePhrase.phrase.split("%").toList  
	  }
    }    
    
    Ok( views.html.game(game.get, targetPhrase, availableWords) )
  }
  
  def index = Action {                
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }
  
}