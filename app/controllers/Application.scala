package controllers

import play.api.mvc.{Action, Controller}
import anorm._
import play.api.db.DB
import play.api.Play.current
import models._

object Application extends Controller {
    
  def game(id: Int) = Action {
    val game = AvailableKeys.find(id)
    
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
    
    val gamePhrase = GamePhrase.getRandomBlankPhrase
    val targetPhrase = {
      if( gamePhrase.phrase.split("%").length == 1 ){         
        gamePhrase.phrase.split("%").toList :+ "."
      }else{
        gamePhrase.phrase.split("%").toList  
	  }
    }
    
    val availableWords = Array("Horse meat", "Just the tip", "One thousand Slim Jims", "Sexual humiliation").toList
    
    Ok( views.html.game(game.get, targetPhrase, availableWords) )
  }
  
  def index = Action {        
        
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }
  
}