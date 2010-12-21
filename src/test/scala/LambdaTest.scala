import org.specs._
import org.specs.runner._


package mustache {
object LambdaSpecification extends SpecificationWithJUnit {

  "mustache" should {

    "render values returned by no-args functions" in {
      Mustache(
        "{{value}}"
      ).render(Map(
        "value"->(()=>{"hey!"})
      )).toString must be equalTo("hey!")
    }

    "render values returned by functions with string arg" in {
      Mustache(
        "{{#bold}}some text{{/bold}}"
      ).render(Map(
        "bold"->((str:String)=>{ "<b>"+str+"</b>" })
      )).toString must be equalTo("<b>some text</b>")
    }

    "render values returned by functions with render param" in {
      Mustache(
        "{{#bold}}Hello, {{name}}!{{/bold}}"
      ).render(Map(
        "name"-> "world"
        ,"bold"->((str:String, render:(String)=>String)=>{ "<b>"+render(str)+"</b>" })
      )).toString must be equalTo("<b>Hello, world!</b>")
    }

  }

}
}

