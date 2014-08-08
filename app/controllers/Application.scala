package controllers

import play.api.mvc.{Action, Controller}
import anorm._
import play.api.db.DB
import play.api.Play.current
import java.security.MessageDigest
import models._

object Application extends Controller {
    
  def game(id: Int) = Action {
    val game = AvailableKeys.find(id)
    
    if( game.isEmpty ){
      sys.error("Sorry! That key doesn't exist")
    }
    
    val targetPhrase = ("Maybe she's born with it. Maybe it's %.").split("%").toList
    val availableWords = Array("Horse meat", "Just the tip", "One thousand Slim Jims", "Sexual humiliation").toList
       
    val str = "This is a test"
    println( play.api.libs.Codecs.toHexString(MessageDigest.getInstance("SHA-256").digest(str.getBytes())) )
    
    Ok( views.html.game(game.get, targetPhrase, availableWords) )
  }
  
  def index = Action {        
        
    val availableKeys = AvailableKeys.getAllAvailableKeys
    
    Ok( views.html.index(availableKeys) )
  }
  
}