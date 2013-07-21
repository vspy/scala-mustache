import org.specs2.mutable._
import org.specs2.runner._

package mustache {
object Issue2Specification extends SpecificationWithJUnit {

  "mustache" should {

    "replace section content by string returned from lambda context" in {
      new Mustache(
        "{{#foo}}42{{/foo}}"
      ).render(Map(
        "foo"->(()=>{ "bar" })
      )).toString must be equalTo("bar")
    }

    "render strings obtained from context" in {
      new Mustache(
        "{{#name}}shouldn't be displayed{{/name}}"
      ).render(Map(
        "name"->(()=>{ "Chris" })
      )).toString must be equalTo("Chris")
    }

    "display inverted section content if lambda returns None" in {
      new Mustache(
        "{{^foo}}Hey!{{/foo}}"
      ).render(Map(
        "foo"->(()=>{ None })
      )).toString must be equalTo("Hey!")
    }

    "display inverted section content if lambda returns null" in {
      new Mustache(
        "{{^foo}}Hey!{{/foo}}"
      ).render(Map(
        "foo"->(()=>{ null })
      )).toString must be equalTo("Hey!")
    }

    "display inverted section content if lambda returns empty collection" in {
      new Mustache(
        "{{^foo}}Hey!{{/foo}}"
      ).render(Map(
        "foo"->(()=>{ List() })
      )).toString must be equalTo("Hey!")
    }

    "dive into an object when rendering section token" in {
      object SampleObject

      new Mustache(
        "{{#foo}}object is in the context{{/foo}}"
      ).render(Map(
        "foo"->SampleObject
      )).toString must be equalTo("object is in the context")      
    }

    "not display inverted section content when there is an object in context" in {
      object SampleObject

      new Mustache(
        "{{^foo}}shouldn't be rendered{{/foo}}"
      ).render(Map(
        "foo"->SampleObject
      )).toString must be equalTo("")      
    }



  }

}
}



