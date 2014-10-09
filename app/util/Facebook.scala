package util

import org.apache.commons.codec.binary.Base64
import scala.collection.JavaConversions._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.mvc.Cookies

case class FacebookCookie(algorithm: String, code: String, issuedAt: Int, userId: String);

object Facebook {

  def getFbSecret: String = {
    val confFilePath = play.Play.application().path().getAbsolutePath() + "/config.ini"
    
    io.Source.fromFile(confFilePath).getLines
      .map(a => a.split("\\="))
      .filter(a => a(0) == "facebook_secret")
      .map(a => a(1)).mkString("")
  }
    
  def getUserId(requestCookies: Cookies): String = {    
    val Array(signature, payload) = this.getFbCookie(requestCookies).split("\\.")
    val json = Json.parse( new Base64(true).decode(payload).map(a => a.toChar).mkString("") )
    
    json.validate[FacebookCookie].get.userId
  }
  
  private def validateFacebookCookie(fbCookie: String): Boolean = {
    
    val decoder = new Base64(true)
        
    val Array(signature, payload) = fbCookie.split("\\.")           
    val rawSig = decoder.decode( signature.replaceAll("-", "+").replaceAll("_", "/").getBytes )
        
    val key = this.getFbSecret    
    val mac = Mac.getInstance("hmacSHA256");    
    
    mac.init( new SecretKeySpec(key.getBytes, "hmacSHA256") )
    
    val hmacData = mac.doFinal( payload.getBytes )
    
    rawSig.deep == hmacData.deep
  }
  
  private def getFbCookie(requestCookies: Cookies): String = {
    requestCookies
    	.filter(a => {a.name.indexOf("fbsr_") > -1})
    	.map(a => a.value).mkString("")
  }
  
  def isFacebookCookieValid(requestCookies: Cookies): Boolean = {
    
    val fbCookie = this.getFbCookie(requestCookies)    
    
    if( fbCookie.length == 0 || fbCookie.indexOf(".") == -1 ){
      false
    }else{
      this.validateFacebookCookie(fbCookie)
    }
    
  } 
  
  
  implicit val jsonToCookie: Reads[FacebookCookie] = (
     (__ \ "algorithm").read[String]
     and (__ \ "code").read[String]     
     and (__ \"issued_at").read[Int]
     and (__ \ "user_id").read[String]
  )(FacebookCookie.apply _)  
}