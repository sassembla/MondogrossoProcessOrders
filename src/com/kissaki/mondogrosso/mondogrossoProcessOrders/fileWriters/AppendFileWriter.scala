package com.kissaki.mondogrosso.mondogrossoProcessOrders.fileWriters
import com.kissaki.Messenger
import com.kissaki.MessengerProtocol
import com.kissaki.TagValue
import com.kissaki.mondogrosso.mondogrossoProcessOrders.ProcessOrdersMasterMessages
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.PrintWriter
import com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoContextController

class AppendFileWriter(name: String, masterName: String, targetFilePath: String) extends MessengerProtocol {
  val messenger = new Messenger(this, name)
  val sb: ListBuffer[String] = new ListBuffer()

  messenger.inputParent(masterName)

  //すでにファイルが有れば、消す
  val file: File = new File(targetFilePath)
  file.delete

  assert(file.createNewFile, "failed to create :" + targetFilePath)

  val toFileOutput = new PrintWriter(targetFilePath)

  def receiver(exec: String, tagValues: Array[TagValue]) = {

    if (exec.equals("addLog")) {
      val status = messenger.get("status", tagValues).asInstanceOf[String]
      val messagesTagValues = messenger.get("tagValues", tagValues).asInstanceOf[Array[TagValue]]

      appendLine(status, messagesTagValues)
    }

    if (exec.equals("wtiteOut")) toFileOutput.close

  }

  /**
   * ログ書き込み
   */
  def appendLine(status: String, tagValues: Array[TagValue]) = {
    println("targetFilePath " + targetFilePath)

    sb += status + "\n"
    tagValues.foreach { v => sb += status + "*" + v.toString + "\n" }
    val line = tagValues.toSeq
    for (tagValue <- tagValues) yield tagValue

    sb += "/" + status + "\n"

    sb.foreach(thing => toFileOutput.write(thing.toString))

    //空にする
    sb.clear
  }

}