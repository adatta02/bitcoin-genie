package controllers

import util._
import models._
import anorm._
import play.api.mvc.{Action, Controller, Result}
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json._

object Application extends Controller {  
  
  def util(key: String) = Action {
    
    key match {
      case "initKeys" => SeedDb.seedKeys
      case "seedPhrases" => SeedDb.seedPhrases
      case _ => 
    }
    
    Ok("Ok")
  }
  
  def redeem(key: String) = Action {
    
    val game = AvailableKeys.findBy( ("redeem_key" -> key) )
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }    
        
    Ok( views.html.redeem(game.get) )
  }
    
  def game(id: Int) = Action {
    
    val game = AvailableKeys.find(id)
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
    
    val viewResult = game.get.game match {
      case "golf" => playGolf(game.get)
      case "dealnodeal" => playDealOrNoDeal(game.get)
      case _ => sys.error("Unrecognized game!")
    }
    
    viewResult
  }
  
  def index = Action {                
    val availableKeys = AvailableKeys.getAllAvailableKeys    
    Ok( views.html.index(availableKeys) )
  }  
  
  def playDealOrNoDeal(game: AvailableKey): Result = {        
        
    val dealBoard = game.getDealOrNoDealBoard
    val htmlJson = dealBoard.getJsonForView
    
    Ok( views.html.dealornodeal(game, htmlJson) )  
  }
  
  def playDeal(id: Int) = Action(parse.json) {request => {
    
    val game = AvailableKeys.find(id).get
    val dealBoard = game.getDealOrNoDealBoard    
    
    val updatedBoard = request.body.\("action").as[String] match {
      
      case "declineSwitchBox" => {
        dealBoard.declineSwitchCase
      }
      
      case "switchBox" => {
        dealBoard.switchCase
      }
      
      case "selectCase" => {
        val pos = request.body.\("pos").asOpt[Int].get
        dealBoard.seletBox(pos)
      }
      
      case "openBox" => {
        val pos = request.body.\("pos").asOpt[Int].get
        dealBoard.openBox(pos)
      }
      
      case _ => sys.error("Unrecognized action")
    }
    
    val result = AvailableKeys
    				.update(game, ("deal_board" -> Json.toJson(updatedBoard).toString))
    				.getDealOrNoDealBoard
    				.getJsonForView        
    
    Ok( Json.toJson(result) )
  }}
  
  def getRandomPhrases() = Action {
    Ok( views.html.renderPhrases(GamePhrase.getRandomPhrases) )
  }
  
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
  
  def playGolf(game: AvailableKey): Result = {
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
    
}