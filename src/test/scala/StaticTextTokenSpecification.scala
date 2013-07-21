import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object StaticTextTokenSpecification extends SpecificationWithJUnit {

  object SampleTemplate extends Mustache("")

  "static text token" should {
    "render static text" in {
      StaticTextToken("Hey!").render(
        null, Map(), List(SampleTemplate)
      ).toString must be equalTo("Hey!")
    }
  }

}
}
