package plugins

import play.api.{Plugin, Application}
import java.io.File
import scala.io._
import com.google.bitcoin.params.TestNet3Params
import com.google.bitcoin.params.MainNetParams
import com.google.bitcoin.store.SPVBlockStore 
import com.google.bitcoin.core._
import com.google.bitcoin.script.Script
import com.google.bitcoin.kits.WalletAppKit
import scala.collection.JavaConverters._
import com.google.common.util.concurrent.MoreExecutors
import org.slf4j.LoggerFactory
import play.api.libs.json._

class SfWallet(params: NetworkParameters, directory: File, filePrefix: String) extends WalletAppKit(params, directory, filePrefix) {
  this.setUserAgent("Setfive-BTC", "0.0.1")
  
  override def onSetupCompleted: Unit = {
    
  }
  
}

class BtcWalletPlugin(app: Application) extends Plugin {
  
  val kit = new SfWallet(MainNetParams.get(), new File("."), "btc-deal")
  
  override def enabled = true
  
  override def onStart() = {
    val log = LoggerFactory.getLogger(this.getClass());
    
    this.kit.startAsync()
    this.kit.awaitRunning()    
    
    println( "BALANCE: " + kit.wallet.getBalance )
  }

  override def onStop() = {
    this.kit.stopAsync()
  }   
  
  def getRecieveAddress: JsObject = {
    Json.obj("address" -> this.kit.wallet.currentReceiveAddress.toString)
  }
  
  def getWalletAsJson: JsObject = {
    val addresses = this.kit.wallet.getWatchedAddresses.asScala.map( a => a.toString )
    val transactions = this.kit.wallet.getTransactions(false).asScala.map( a => a.getHashAsString() )
    
    Json.obj( "balance" -> this.kit.wallet.getBalance.toFriendlyString, 
    		  "addresses" -> addresses, "transactions" -> transactions )
  }
  
  def checkIsAddressValid(address: String): Tuple2[Boolean, String] = {
    
    try {
      val targetAddress = new Address(kit.params(), address)
      (false, "")
    }catch{
      case e:Throwable => {
        (true, e.getMessage())
      }
    }
    
  }
  
  def sendAmountToAddress(address: String, amount: Double): String = {
    
    println( "BALANCE: " + kit.wallet.getBalance )
    
    val targetAddress = new Address(kit.params(), address)       
    val coinAmount = Coin.parseCoin( amount.toString )
    
    try {
    	val res = this.kit.wallet.sendCoins(kit.peerGroup(), targetAddress, Coin.valueOf(amount.longValue()))
    	
	    res.broadcastComplete.addListener(new Runnable{
	      def run: Unit = {
	        println("The transaction is complete!")
	      }
	    }, MoreExecutors.sameThreadExecutor())
	    
	    res.tx.getHashAsString
    }catch{
      case _:Throwable => {""} 
    }
    
  }
  
}