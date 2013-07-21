import org.specs2.mutable._
import org.specs2.runner._

import java.security.MessageDigest

package mustache {
object HelperSpecification extends SpecificationWithJUnit {

  object MD5 {
    def apply(key:String) : String = {
      val bytes = key.getBytes("UTF-8")
      val md5 = MessageDigest.getInstance("MD5")
      md5.reset
      md5.update(bytes)
      md5.digest().map(0xFF & _).map("%02x".format(_)).mkString
    } 
  }

  trait GravatarHelper {
    this: GravatarHelper with MustacheHelperSupport =>

    def gravatar =
      context match {
        case m:Map[String,Any] =>
          gravatarForId(
            MD5(m("email").toString.trim.toLowerCase)
          )
      
        case _ => 
          throw new IllegalArgumentException(
            "Invalid context for gravatar rendering: "+context
          )
      }

    def gravatarForId(gid:String, size:Int = 30) =
      gravatarHost + "/avatar/"+gid+"?s="+size

    def gravatarHost =
      if(ssl) "https://secure.gravatar.com"
      else "http://www.gravatar.com"

    val ssl:Boolean
  }

  class GravatarMustacheExample(isSsl:Boolean, template:String) 
    extends Mustache(template) 
    with GravatarHelper {

    val ssl = isSsl

  }

  "mustache" should {

    "render values returned by helper" in {
      new GravatarMustacheExample(true,
      "<ul>" +
        "{{# users}}" +
          "<li><img src=\"{{ gravatar }}\">{{ login }}</li>" +
        "{{/ users}}" +
      "</ul>"
      ).render(Map(
        "users"->List(
            Map("email"->"alice@example.org"
                ,"login"->"alice")
            ,Map("email"->"bob@example.org"
                ,"login"->"bob")
        )
      )).toString must be equalTo("""<ul><li><img src="https://secure.gravatar.com/avatar/fbf7c6aec1d4280b7c2704c1c0478bd6?s=30">alice</li><li><img src="https://secure.gravatar.com/avatar/10ac39056a4b6f1f6804d724518ff2dc?s=30">bob</li></ul>""")
    }

    "render values returned by parent helper" in {
      val userList = new Mustache(
        "<ul>" +
          "{{# users}}" +
            "<li><img src=\"{{ gravatar }}\">{{ login }}</li>" +
          "{{/ users}}" +
        "</ul>"
      )
      val page = new Mustache("<html><body>{{>userList}}</body></html>")
      val root = new GravatarMustacheExample(true, "{{>content}}")

      root.render(Map(
          "users"->List(
            Map("email"->"alice@example.org"
                ,"login"->"alice")
            ,Map("email"->"bob@example.org"
                ,"login"->"bob")
          )
        ), Map("content"->page, "userList"->userList)
      ) must be equalTo("""<html><body><ul><li><img src="https://secure.gravatar.com/avatar/fbf7c6aec1d4280b7c2704c1c0478bd6?s=30">alice</li><li><img src="https://secure.gravatar.com/avatar/10ac39056a4b6f1f6804d724518ff2dc?s=30">bob</li></ul></body></html>""")

    }

  }

}
}


