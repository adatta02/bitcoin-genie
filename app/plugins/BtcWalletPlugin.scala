package plugins

import play.api.{Plugin, Application}
import java.io.File
import scala.io._
import com.google.bitcoin.params.TestNet3Params    
import com.google.bitcoin.store.SPVBlockStore 
import com.google.bitcoin.core._
import com.google.bitcoin.script.Script
import com.google.bitcoin.kits.WalletAppKit
import scala.collection.JavaConverters._
import com.google.common.util.concurrent.MoreExecutors
import org.slf4j.LoggerFactory

class SfWallet(params: NetworkParameters, directory: File, filePrefix: String) extends WalletAppKit(params, directory, filePrefix) {
  this.setUserAgent("Setfive-BTC", "0.0.1")
  
  override def onSetupCompleted: Unit = {
    
  }
  
}

class BtcWalletPlugin(app: Application) extends Plugin {
  
  val kit = new SfWallet(TestNet3Params.get(), new File("."), "wallet-kit")
  
  override def enabled = true
  
  override def onStart() = {
    val log = LoggerFactory.getLogger(this.getClass());
    
    this.kit.startAsync()
    this.kit.awaitRunning()
    
    val oldWallet = Wallet.loadFromFile( new File("/home/ashish/workspace_java/bitcoinj-runner/app_wallet.data") )
    val oldKey = oldWallet.getImportedKeys.asScala.find(a => a.toAddress( TestNet3Params.get() ).toString() == "n1vvwD5VKBGsB3yhc9kW746Fb31hiKbZM6")
    
    kit.wallet.importKey( oldKey.get )
    
    println( "BALANCE: " + kit.wallet.getBalance )
  }

  override def onStop() = {
    this.kit.stopAsync()
  }   
  
  def sendAmountToAddress(address: String, amount: Double): Unit = {
    
    val targetAddress = new Address(kit.params(), address)
    val res = this.kit.wallet.sendCoins(kit.peerGroup(), targetAddress, Coin.valueOf(amount.longValue()))
        
    res.broadcastComplete.addListener(new Runnable{
      def run: Unit = {
        println("The transaction is complete!")
      }
    }, MoreExecutors.sameThreadExecutor())    
    
  }
  
}