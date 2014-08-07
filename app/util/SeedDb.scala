package util

import anorm._
import play.api.db.DB
import play.api.Play.current

object SeedDb {
  
  def seedKeys: Unit = {

    DB.withConnection(implicit c => {
      
      val games = Array("golf", "photobooth", "craps")
      
      for(i <- 1 to 50){
        SQL("INSERT INTO available_keys (public_key, amount, game) VALUES ({key}, {amount}, {game})")
          .on("key" -> "not_real_key", 
              "amount" -> (scala.util.Random.nextDouble * 10).formatted("%.2f"),
              "game" -> games( scala.util.Random.nextInt(3) )
         ).executeInsert()
      }      
      
    })    
    
  }
  
}