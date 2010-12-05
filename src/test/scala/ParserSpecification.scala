import org.specs._
import org.specs.runner._


package mustache {
object ParserSpecification extends SpecificationWithJUnit {

  "parser" should {

    "handle static text only" in {
      Mustache(
        "Hello, world!"
      ).render().toString must be equalTo(
        "Hello, world!"
      )
    }

    "handle simple values" in {
      Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"world")
      ).toString must be equalTo(
        "Hello, world!"
      )
    }
  
    "handle escaping properly" in {
      Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, &lt;tag&gt;!"
      )
    }

    "handle unescaped values" in {
      Mustache(
        "Hello, {{{name}}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "handle unescaped tags with &" in {
      Mustache(
        "Hello, {{&name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "report error for unbalanced braces in {{{ }}}" in {
      // unbalanced { inside the tag
      Mustache("Hello, {{{name}}!") must throwA[MustacheParseException]
    }

    "ignore incomplete tags" in {
      Mustache(
        "{ { {"
      ).render().toString must be equalTo(
        "{ { {"
      )
      Mustache(
        "} }} }"
      ).render().toString must be equalTo(
        "} }} }"
      )
    }

    "report error for empty tag" in {
      Mustache("{{{}}}") must throwA[MustacheParseException]
      Mustache("{{}}") must throwA[MustacheParseException]
    }

    "report error for unclosed section" in {
      Mustache("some text {{#foo}} some internal text") must throwA[MustacheParseException]
    }

    "report error for unclosed tag" in {
      Mustache("some text {{unclosed tag") must throwA[MustacheParseException]
    }

    "report error for messed up sections" in {
      Mustache("text {{#foo}} {{#bar}} txt {{/foo}} {{/bar}}") must throwA[MustacheParseException]
    }

    "report error for invalid delimiter tag" in {
      Mustache("some text {{=}} some text") must throwA[MustacheParseException]
      Mustache("some text {{==}} some text") must throwA[MustacheParseException]
      Mustache("some text {{= foo =}} some text") must throwA[MustacheParseException]
    }

    "report error for invalid tags" in {
      Mustache("some text {{>}} some text") must throwA[MustacheParseException]
      Mustache("some text {{<}} some text") must throwA[MustacheParseException]
      Mustache("some text {{&}} some text") must throwA[MustacheParseException]
      Mustache("some text {{^}}...{{/}} some text") must throwA[MustacheParseException]
      Mustache("some text {{#}}...{{/}} some text") must throwA[MustacheParseException]
    }

    "report lines properly" in {
      Mustache(
        "some text\nand some more\n{{>}}\nsome text again"
      ) must throwA(MustacheParseException(3, "Empty tag"))
    }

  }

}
}
