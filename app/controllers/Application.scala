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
    
    Ok( views.html.game(game.get) )
  }
  
  def index = Action {        
        
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }
  
}