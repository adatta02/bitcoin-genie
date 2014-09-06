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
  
  def doRedeem(publicKey: String) = Action(parse.json) {implicit request => {
    
    Ok( Json.toJson( Json.obj("error" -> false)) )
  }}
  
  def redeem(key: String) = Action { implicit request => {
    
    val game = AvailableKeys.findBy( ("redeem_key" -> key) )
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }        
        
    Ok( views.html.redeem(game.get) )
  }}
    
  def game(publicKey: String) = Action { implicit request =>
            
    val game = AvailableKeys.findBy("public_key" -> publicKey)
    
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
    
    val viewResult = if( game.get.isRedeemed ){
      Ok( views.html.dealover() )
    }else{
	    game.get.game match {
	      case "golf" => Golf.play(game.get)
	      case "dealnodeal" => DealOrNoDeal.render(game.get)
	      case _ => sys.error("Unrecognized game!")
	    }
    }
    
    viewResult
  }
  
  def index = Action {                
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }  
    
}