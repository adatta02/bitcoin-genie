package util

import anorm._
import play.api.db.DB
import play.api.Play.current
import play.api.libs.Codecs
import java.security.MessageDigest
import scala.util.Random

/*
 * To use this:
 * ashish@ashish:~/workspace_java/bitcoin-fountain$ ./activator play console
 * scala> new play.core.StaticApplication(new java.io.File("."))
 * [info] play - database [default] connected at jdbc:h2:file:/home/ashish/test
 * [info] play - Application started (Prod)
 * res0: play.core.StaticApplication = play.core.StaticApplication@57e3ead3
 * 
 * scala> import util.SeedDb
 * import util.SeedDb
 * scala> SeedDb.seedKeys
 * 
 */

object SeedDb {
  
  def getRandomKey(len: Integer): String = {
    val fakeKey = Codecs.toHexString(MessageDigest.getInstance("SHA-256").digest( Random.nextInt.toString.getBytes() ))
    val finalKey = Range.inclusive(0, len)
    
    "1" + finalKey.map( a => {
	    			if( Random.nextInt % 3 == 0 ){
	    			  fakeKey.charAt(Random.nextInt(fakeKey.length())).toString
	    			}else{
	    			  fakeKey.charAt(Random.nextInt(fakeKey.length())).toString.toUpperCase()
	    			}
    			  })
    			  .foldLeft("")((collect, element) => {collect.concat(element)})
  }  
  
  def seedKeys: Unit = {

    DB.withConnection(implicit c => {
      
      SQL("DELETE FROM available_keys").execute
      
      val games = Array("golf", "photobooth", "craps")
      
      for(i <- 1 to 50){
                
        SQL("INSERT INTO available_keys (public_key, amount, game, redeem_key) VALUES ({key}, {amount}, {game}, {redeemKey})")
          .on("key" -> this.getRandomKey(31), 
              "redeemKey" -> this.getRandomKey(12),
              "amount" -> (scala.util.Random.nextDouble * 10).formatted("%.2f"),
              "game" -> games(scala.util.Random.nextInt(3))                         
         ).executeInsert()
         
      }      
      
    })    
    
  }
  
  def seedPhrases: Unit = {
    
    DB.withConnection(implicit c => {
      
        SQL("DELETE FROM golf_phrase").execute
        
    	io.Source.fromFile("game_data/golf_phrases.txt").getLines.foreach(a => {
    	  SQL("INSERT INTO golf_phrase (phrase, phrase_type) VALUES ({phrase}, 1)").on("phrase" -> a).executeInsert()
    	})
    	
    	io.Source.fromFile("game_data/golf_words.txt").getLines.foreach(a => {
    	  SQL("INSERT INTO golf_phrase (phrase, phrase_type) VALUES ({phrase}, 2)").on("phrase" -> a).executeInsert()
    	})    	
    	
    })    
    
  }
  
}