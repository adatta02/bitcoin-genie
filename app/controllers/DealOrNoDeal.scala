package controllers

import util.Facebook
import models._
import play.api.mvc.{Action, Controller, Result, Cookie, Request, AnyContent}
import play.api.libs.json._

object DealOrNoDeal extends Controller {

  def startGame = Action {implicit request => {
    
    if( Facebook.isFacebookCookieValid(request.cookies) ){
      val fbId = Facebook.getUserId(request.cookies)
      val game = AvailableKeys.getGameForFBUserId( fbId )
      
      Redirect( routes.Application.game(game.publicKey) )      
    }else{
      Redirect( routes.Application.fbLogin )
    }         
        
  }}
  
  def render(request: Request[AnyContent], game: AvailableKey): Result = {     
        
    val dealBoard = game.getDealOrNoDealBoard
    val htmlJson = dealBoard.getJsonForView
    
    Ok( views.html.dealornodeal(game, htmlJson) )
  }
  
  def getRedeemUrl(publicKey: String) = Action(parse.json) {implicit request => {
    val game = AvailableKeys.findBy("public_key" -> publicKey).get
    val dealBoard = game.getDealOrNoDealBoard
    
    val url = if( game.amount.isDefined ){            
      routes.Application.redeem(game.redeemKey).absoluteURL(false)
    }else{
      ""
    }
        
    Ok( Json.toJson(Json.obj("url" -> url)) )
  }}
  
  def takeDeal(publicKey: String) = Action(parse.json) {implicit request => {
    val game = AvailableKeys.findBy("public_key" -> publicKey).get
    val dealBoard = game.getDealOrNoDealBoard
    
    if(dealBoard.currentOffer.isDefined){
    	AvailableKeys.update(game, ("amount" -> dealBoard.currentOffer.get.toString))
    }else{
      sys.error("Sorry! The game has no offer.")
    }
    
    val redeemUrl = routes.Application.redeem(game.redeemKey).absoluteURL(false)
    Ok( Json.toJson(Json.obj("url" -> redeemUrl)) )    
  }}
  
  def playDeal(publicKey: String) = Action(parse.json) {request => {
            
    val game = AvailableKeys.findBy("public_key" -> publicKey).get
    val dealBoard = game.getDealOrNoDealBoard    
            
    if( game.amount.isDefined ){
      sys.error("Sorry! This game has already been played.")
    }
    
    val action = request.body.\("action").as[String]
    val updatedBoard = action match {
      
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
    
    if( dealBoard.getSelectedBoxValue.isDefined ){
      AvailableKeys.update(game, ("amount" -> updatedBoard.getSelectedBoxValue.get.toString))
    }
    
    val result = AvailableKeys
    				.update(game, ("deal_board" -> Json.toJson(updatedBoard).toString))
    				.getDealOrNoDealBoard
    				.getJsonForView        
	
    Ok( Json.toJson(result) )
  }}  
  
}