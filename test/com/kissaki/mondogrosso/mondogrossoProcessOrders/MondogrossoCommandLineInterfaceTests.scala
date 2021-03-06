package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.io.Source
import java.io.File
import fileWriters.AppendFileWriter
import com.kissaki.TagValue

@RunWith(classOf[JUnitRunner])
class MondogrossoCommandLineInterfaceTests extends Specification {
  
  /**
    待ち
  */
  def whileAlive (identity:String, interface:MondogrossoCommandLineInterface, limit: Int = 10) = {
  	val output = new AppendFileWriter(targetFilePath = "MondogrossoCommandLineInterfaceTests/"+identity+".txt")
    var i = 0
    println("whileAlive開始  " + identity)
    while (interface.isAlive == 0 && i < limit) {
      i += 1
      println("whileAlive waiting  " + identity + "  /" + i + " of " + limit)

      val tagValues = interface.messenger.tagValues(new TagValue("i/limit", i+"/"+limit))
      
      output.appendLine(identity, tagValues)
      Thread.sleep(100)
    }

    
    if (limit == i) {
    	val tagValues = interface.messenger.tagValues(new TagValue("late", "whileAlive 回数超過 " + identity))
      output.appendLine(identity, tagValues)
      sys.error("whileAlive 回数超過 " + identity)
      sys.exit(-1)
    }

    //突破したのでログを吐く
    val message = "whileAlive  " + identity + "/状態は "+interface.isAlive
    val tagValues = interface.messenger.tagValues(new TagValue("message", message))
    
    output.appendLine(identity, tagValues)
    
    println(message)
  }


  val standardInput = "A!Z"
  val standardJSON = """
		{"A": 
			{
				"_kind": "sh",
				"_main": "echo",
				"a":"something",
				"a2":"else",
				"a3":"other",
			},
    "B": 
      {
        "_kind": "jar",
        "_main": "TestProject",
        "-i" : "確実にfinallyタイムアウトになる値 in MondogrossoProcessOrdersTests",
        "-t" : "10000"
      },
    "C": 
      {
        "_kind": "jar",
        "_main": "TestProject",
        "-i" : "このJavaプロセス自身がタイムアウトになる in MondogrossoProcessOrdersTests",
        "-t" : "10000",
        "__timeout":"50"
      },
		"Z": 
			{
				"_kind": "sh",
        "_main": "open",
        "-a":"Safari.app file:///Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersTests.html",
				"__finallyTimeout":"2000"
			}
		}
	"""

  if (false) {
    "-p　プロセス入力 と、-s　JSONソース入力 で、プロセスをアタッチして即実行する" should {

      "A!Z" in {
        //ファイルを消す
        for {
          files <- Option(new File("./").listFiles)
          file <- files if file.getName.startsWith("testStandardOut")
        } file.delete()

        val input = Array(
          "-i", "AtoZ",
          "-p", standardInput,
          "-s", standardJSON,
          "-o", "./testStandardOut.txt")

        val interface = new MondogrossoCommandLineInterface("A!Z", "A!Z")
        interface.input(input)
        
        while (interface.isAlive == 0) {
          Thread.sleep(100)
          println("A!Z")
        }


        //結果のファイルが出来ているはず
        val source = Source.fromFile("testStandardOut.txt")
        var last = ""
        source.getLines.foreach{line => 
        	last = line
        }
        
        last must be_==("/AtoZ@MESSAGE_DONE")
      }

      "B!Z Bが終わらない事によるfinallyのタイムアウト" in {
        
        //ファイルを消す
        for {
          files <- Option(new File("./").listFiles)
          file <- files if file.getName.startsWith("testFinallyTimeout")
        } file.delete()

        val input = Array(
          "-i", "BtoZ",
          "-p", "B!Z",
          "-s", standardJSON,
          "-o", "./testFinallyTimeout.txt")

        val interface = new MondogrossoCommandLineInterface("B!Z Bが終わらない事によるfinallyのタイムアウト", "B!Z Bが終わらない事によるfinallyのタイムアウト")
        interface.input(input)
        
        whileAlive("B!Z Bが終わらない事によるfinallyのタイムアウト", interface)

        //結果のファイルが出来ているはず
        val source = Source.fromFile("testFinallyTimeout.txt")
        var last = ""
        source.getLines.foreach{line => 
        	last = line
        }
        
        last must be_==("/BtoZ@MESSAGE_TIMEOUTED")
      }

      "C!Z Cが自分自身でタイムアウト" in {

        //ファイルを消す
        for {
          files <- Option(new File("./").listFiles)
          file <- files if file.getName.startsWith("testTimeout")
        } file.delete()

        val input = Array(
          "-i", "CtoZ",
          "-p", "C!Z",
          "-s", standardJSON,
          "-o", "./testTimeout.txt")

        val interface = new MondogrossoCommandLineInterface("C!Z Cが自分自身でタイムアウト", "C!Z Cが自分自身でタイムアウト")
        interface.input(input)
        
        while (interface.isAlive == 0) {
          Thread.sleep(100)
        }
        
        //結果のファイルが出来ているはず
        val source = Source.fromFile("testTimeout.txt")
        var last = ""
        source.getLines.foreach{line => 
        	last = line
        }
        
        last must be_==("/CtoZ@MESSAGE_TIMEOUTED")
      }
    }
  }

  

  if (false) {
    "replモード" should {
      "-p,-s無し" in {
        //やるとしたら、自動的に入力、みたいなこともしないといけない。　要件を纏めよう。このモード危険な気がする。
        "not yet implement" must be_==("")
      }

      "-p,-s有り" in {
        "not yet implement" must be_==("")
      }
    }
  }

  // if (false) {
  //   "output" should {
  //     "出力する" in {
  //       val JSON = """
		// 			{"A": 
		// 				{
		// 					"_kind": "sh",
		// 					"_main": "open",
		// 					"-a":"Safari.app file:///Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersTests.html"
		// 				},
		// 			"Z": 
		// 				{
		// 					"_kind": "sh",
		// 					"_main": "pwd",
		// 					"__finallyTimeout":"2000"
		// 				}
		// 			}
		// 		"""

  //       val result = MondogrossoProcessOrders.main(Array(
  //         "-p", standardInput,
  //         "-s", JSON))

  //       println("result2	" + result)
  //       "" must be_==("")
  //     }
  //   }
  // }
}