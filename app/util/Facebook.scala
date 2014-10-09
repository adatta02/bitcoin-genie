package util

import org.apache.commons.codec.binary.Base64
import scala.collection.JavaConversions._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object Facebook {

  private def validateFacebookCookie(fbCookie: String): Boolean = {
               
    val decoder = new Base64(true)
        
    val Array(signature, payload) = fbCookie.split("\\.")           
    val rawSig = decoder.decode( signature.replaceAll("-", "+").replaceAll("_", "/").getBytes )
        
    val key = ""      
    val mac = Mac.getInstance("hmacSHA256");    
    
    mac.init( new SecretKeySpec(key.getBytes, "hmacSHA256") )
    
    val hmacData = mac.doFinal( payload.getBytes )
    
    rawSig.deep == hmacData.deep
  }
  
  def isFacebookCookieValid(fbCookie: String): Boolean = {
        
    if( fbCookie.indexOf(".") == -1 ){
      false
    }else{
      this.validateFacebookCookie(fbCookie)
    }
    
  }  
  
}