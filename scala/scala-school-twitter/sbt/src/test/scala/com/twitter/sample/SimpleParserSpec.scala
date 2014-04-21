package com.twitter.sample

import org.specs2.mutable._

object SimpleParserSpec extends Specification {
  "SimpleParser" should {
    val parser = new SimpleParser

    "work with basic tweet" in {
      val tweet = """{"id":1,"text":"foo"}"""
      parser.parse(tweet) match {
        case Some(parsed) =>
          parsed.text must be_==("foo")
          parsed.id must be_==(1)
        case _ => failure("didn't parse tweet")
      }
    }

    "reject a non-JSON tweet" in {
      val tweet = """"id":1,"text":"foo""""
      parser.parse(tweet) match {
        case Some(parsed) => failure("didn't reject a non-JSON tweet")
        case e => e must be_==(None)
      }
    }

    "ignore nested content" in {
      val tweet = """{"id":1,"text":"foo","nested":{"id":2}}"""
      parser.parse(tweet) match {
        case Some(parsed) =>
          parsed.text must be_==("foo")
          parsed.id must be_==(1)
        case _ => failure("didn't parse tweet")
      }
    }

    "fail on partial content" in {
      val tweet = """{"id":1}"""
      parser.parse(tweet) match {
        case Some(parsed) => failure("didn't reject a partial tweet")
        case e => e must be_==(None)
      }
    }
  }
}
