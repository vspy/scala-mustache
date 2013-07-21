import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object DotNotationSpecification extends SpecificationWithJUnit {

  "mustache" should {

    "render {{.}} properly" in {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Map(
        "foo" -> List(4,2)
      )).toString must be equalTo("42")
    }

    "take map value '.' first when rendering {{.}}" in {
      new Mustache(
        "{{#foo}}{{.}}{{/foo}}"
      ).render(Map(
        "foo" -> Map("."->"bar")
      )).toString must be equalTo("bar")
    }

  }

}
}



