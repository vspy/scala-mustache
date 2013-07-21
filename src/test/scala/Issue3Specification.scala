import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object Issue3Specification extends SpecificationWithJUnit {

  "mustache" should {

    "dive into Some[T] when rendering section token" in {
      new Mustache(
        "{{#foo}}{{value}}{{/foo}}"
      ).render(Map(
        "foo" -> Some(Map("value"->"bar"))
      )).toString must be equalTo("bar")
    }

    "properly handle Some[T] when rendering inverse section token" in {
      new Mustache(
        "{{^foo}}this string should't be rendered{{/foo}}"
      ).render(Map(
        "foo" -> Some(Map("value"->"bar"))
      )).toString must be equalTo("")
    }

    "properly handle None[T] when rendering section token" in {
      new Mustache(
        "{{#foo}}this string shouldn't be rendered{{/foo}}"
      ).render(Map(
        "foo" -> None
      )).toString must be equalTo("")
    }

    "properly handle None[T] when rendering inverse section token" in {
      new Mustache(
        "{{^foo}}42{{/foo}}"
      ).render(Map(
        "foo" -> None
      )).toString must be equalTo("42")
    }

    "resolve nested Option[T] when rendering section token" in {
      new Mustache(
        "{{#foo}}{{value}}{{/foo}}"
      ).render(Map(
        "foo" -> Some(Some(Some(Map("value"->"bar"))))
      )).toString must be equalTo("bar")
    }

  }

}
}


