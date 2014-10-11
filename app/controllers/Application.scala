package controllers

import plugins.BtcWalletPlugin
import util._
import models._
import anorm._
import play.api.mvc.{Action, Controller, Result}
import play.api.db.DB
import play.api.Play.current
import play.api.libs.json._
import play.api.Play
import play.api.Play.current

object Application extends Controller {  
  
  def fbLogin = Action { implicit request => {    
    
    Ok( views.html.fbLogin() )
  }}
  
  def util(key: String) = Action {
    
    key match {
      case "initKeys" => SeedDb.seedKeys
      case "seedPhrases" => SeedDb.seedPhrases
      case _ => 
    }
    
    Ok("Ok")
  }
  
  def doRedeem(redeemKey: String) = Action(parse.json) {implicit request => {
    
    val game = AvailableKeys.findBy( ("redeem_key" -> redeemKey) )
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }               
    
    val email = request.body.\("email").as[String]
    val address = request.body.\("key").as[String]
    val isSend = request.body.\("isSend").as[Boolean]
    
    // val addressError = Play.application.plugin[BtcWalletPlugin].get.checkIsAddressValid(address)
    val addressError = (false, false)
    
    val emailError = if( """\b[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""".r.findFirstIn(email) == None ){
      true
    }else{
      false
    }
    
    val transactionBlock = if( emailError == false && addressError._1 == false && isSend == true ){
      AvailableKeys.update(game.get, ("user_email" -> email))
    	// Play.application.plugin[BtcWalletPlugin].get.sendAmountToAddress(address, game.get.amount.get)
      "TEST MODE"
    }else{
      ""
    }
    
    Ok( Json.toJson( Json.obj("emailError" -> emailError, "transactionBlock" -> transactionBlock,
    						  "addressError" -> addressError._1, 
    						  "addressException" -> addressError._2)) )
  }}
  
  def redeem(key: String) = Action { implicit request => {
    
    val game = AvailableKeys.findBy( ("redeem_key" -> key) )
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }               
        
    Ok( views.html.redeem(game.get) )
  }}
    
  def game(publicKey: String) = Action {implicit request =>
            
    val game = AvailableKeys.findBy("public_key" -> publicKey)
    
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
                    
    val viewResult = 
      if( Facebook.isFacebookCookieValid(request.cookies) == false ){
        Redirect( routes.Application.fbLogin )
      }else if( game.get.amount.isDefined ){
        Ok( views.html.dealover(game.get) )
      }else if( game.get.fbUserId.get != Facebook.getUserId(request.cookies) ){
        Redirect( routes.DealOrNoDeal.startGame )
      }else{
        DealOrNoDeal.render(request, game.get)
    }
    
    viewResult
  }
  
  def index = Action {                
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }  
    
}