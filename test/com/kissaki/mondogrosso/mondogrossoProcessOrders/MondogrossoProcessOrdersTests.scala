package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {

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
		"Z": 
			{
				"_kind": "sh",
        "_main": "open",
        "-a":"Safari.app file:///Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersTests.html",
				"__finallyTimeout":"2000"
			}
		}
	"""

  

  if (true) {
    "-p　プロセス入力 と、-s　JSONソース入力 で、プロセスをアタッチして即実行する" should {

       "A!Z" in {

        val input = Array(
          "-i", "AtoZ",
	      "-p", standardInput,
	      "-s", standardJSON,
	      "-o", "./testout.txt"
        )
        
        val result = MondogrossoProcessOrders.main(input)

        //結果のファイルが出来ているはず
        var source = Source.fromFile("testout.txt")
        source.size != 0 must beTrue
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

  if (false) {
    "output" should {
      "出力する" in {
        val JSON = """
					{"A": 
						{
							"_kind": "sh",
							"_main": "open",
							"-a":"Safari.app file:///Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersTests.html"
						},
					"Z": 
						{
							"_kind": "sh",
							"_main": "pwd",
							"__finallyTimeout":"2000"
						}
					}
				"""

        val result = MondogrossoProcessOrders.main(Array(
          "-p", standardInput,
          "-s", JSON))

        println("result2	" + result)
        "" must be_==("")
      }
    }
  }
}