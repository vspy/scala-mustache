import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object EscapedTokenSpecification extends SpecificationWithJUnit {

  object SampleTemplate extends Mustache("")

  "escaped text token" should {
    "render escaped text" in {
      EscapedToken("foo","{{","}}").render(
        Map("foo"->"\"<>&test\""), Map(), List(SampleTemplate)
      ).toString must be equalTo("&quot;&lt;&gt;&amp;test&quot;")
    }
  }

}
}

