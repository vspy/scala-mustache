import org.specs2.mutable._
import org.specs2.runner._


package mustache {
object ParserSpecification extends SpecificationWithJUnit {

  "parser" should {

    "handle static text only" in {
      new Mustache(
        "Hello, world!"
      ).render().toString must be equalTo(
        "Hello, world!"
      )
    }

    "handle simple values" in {
      new Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"world")
      ).toString must be equalTo(
        "Hello, world!"
      )
    }
  
    "handle escaping properly" in {
      new Mustache(
        "Hello, {{name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, &lt;tag&gt;!"
      )
    }

    "handle unescaped values" in {
      new Mustache(
        "Hello, {{{name}}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "handle unescaped tags with &" in {
      new Mustache(
        "Hello, {{&name}}!"
      ).render(
        Map("name"->"<tag>")
      ).toString must be equalTo(
        "Hello, <tag>!"
      )
    }

    "report error for unbalanced braces in {{{ }}}" in {
      // unbalanced { inside the tag
      new Mustache("Hello, {{{name}}!") must throwA[MustacheParseException]
    }

    "ignore incomplete tags" in {
      new Mustache(
        "{ { {"
      ).render().toString must be equalTo(
        "{ { {"
      )
      new Mustache(
        "} }} }"
      ).render().toString must be equalTo(
        "} }} }"
      )
    }

    "report error for empty tag" in {
      new Mustache("{{{}}}") must throwA[MustacheParseException]
      new Mustache("{{}}") must throwA[MustacheParseException]
    }

    "handle sections" in {
      new Mustache(
        "Message: {{#needToGreet}}Hello, {{name}}!{{/needToGreet}}"
      ).render(
        Map("needToGreet"->true, "name"->"world")
      ).toString must be equalTo(
        "Message: Hello, world!"
      )

      new Mustache(
        "Message: {{#needToGreet}}Hello, {{name}}!{{/needToGreet}}"
      ).render(
        Map("needToGreet"->false, "name"->"world")
      ).toString must be equalTo(
        "Message: "
      )
    }

    "handle nested sections" in {
      new Mustache(
        "{{#foo}}>>{{#bar}}Hello, {{name}}!{{/bar}}<<{{/foo}}"
      ).render(
        Map("foo"->Map("bar"->Map("name"->"world")))
      ).toString must be equalTo(
        ">>Hello, world!<<"
      )
    }

    "report error for unclosed section" in {
      new Mustache("some text {{#foo}} some internal text") must throwA[MustacheParseException]
    }

    "report error for unclosed tag" in {
      new Mustache("some text {{unclosed tag") must throwA[MustacheParseException]
    }

    "report error for messed up sections" in {
      new Mustache("text {{#foo}} {{#bar}} txt {{/foo}} {{/bar}}") must throwA[MustacheParseException]
    }

    "report error for invalid delimiter tag" in {
      new Mustache("some text {{=}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{==}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{= foo =}} some text") must throwA[MustacheParseException]
    }

    "report error for invalid tags" in {
      new Mustache("some text {{>}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{<}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{&}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{^}}...{{/}} some text") must throwA[MustacheParseException]
      new Mustache("some text {{#}}...{{/}} some text") must throwA[MustacheParseException]
    }

    "report lines properly" in {
      new Mustache(
        "some text\nand some more\n{{>}}\nsome text again"
      ) must throwA(MustacheParseException(3, "Empty tag"))
    }

  }

}
}
