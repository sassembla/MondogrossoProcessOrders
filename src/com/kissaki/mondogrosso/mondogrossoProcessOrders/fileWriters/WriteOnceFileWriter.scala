package com.kissaki.mondogrosso.mondogrossoProcessOrders.fileWriters
import com.kissaki.Messenger
import com.kissaki.MessengerProtocol
import com.kissaki.TagValue
import com.kissaki.mondogrosso.mondogrossoProcessOrders.ProcessOrdersMasterMessages
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.PrintWriter
import com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoContextController


class WriteOnceFileWriter(name: String, masterName:String) extends MessengerProtocol {
  val messenger = new Messenger(this, name)
  val sb :ListBuffer[String] = new ListBuffer()
  
  messenger.inputParent(masterName)

  def receiver(exec: String, tagValues: Array[TagValue]) = {
    if (exec.equals("addLog")) {
      val status = messenger.get("status", tagValues).asInstanceOf[String]
      val messagesTagValues = messenger.get("tagValues", tagValues).asInstanceOf[Array[TagValue]]
      addLog(status, messagesTagValues)
    }

    if (exec.equals("wtiteOut")) {
      val filePath = messenger.get("fileName", tagValues).asInstanceOf[String]
      writeoutLog(filePath)
    }
  }

  /**
   * ログ追記
   */
  def addLog(status: String, tagValues: Array[TagValue]) = {
    sb += status + "\n"
    
    val line = tagValues.toSeq
    for (tagValue <- tagValues) yield tagValue
    tagValues.foreach { v => sb += status + "*" + v.toString + "\n" }

    sb += "/"+ status + "\n"
  }

  /**
   * ログ書き出し
   */
  def writeoutLog(filePath: String) = {
    val file : File = new File(filePath)
  	val writer = new PrintWriter(file)
    sb.foreach {writer.write}
    writer.close
  }

}