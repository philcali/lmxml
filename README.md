# LMXML

The Light Markup to XML is a markup intended to make writing
xml or a recursive decent markup, easier.

Let's take a simple html document for example.

__HTML__

```html
<html>
  <head>
    <title>Test</title>
    <script type="text/javascript" src="blah.js"></script>
    <link rel="stylesheet" type="text/css" href="main.css"/>
  </head>
  <body>
    <div class="page">
      <div class="content">
        Blah Blah Blah
      </div>
      <div id="footer">
        This is where the copyright goes.
      </div>
    </div>
  </body>
</html>
```

__Equivalent LMXML__

```
html
  head
    title
      "Test"
    script { type: "text/javascript", src: "blah.js" }
    link { rel: "stylesheet", type: "text/css" href: "main.css" }
  body
    div .page
      div .content
        "Blah Blah Blah"
      div #footer
        "This is where the copyright goes."
```

## The syntax

1. Indention is important
2. JSON-style attributes
3. Class attributes prefixed with "." and Id with "#"
4. Markdown Indirect link style templates
5. Indirect definitions are delimited from main document by `---`

Knock your self out.

__Example__:

```
div #page
  [boilerplate]
  div .content
    div #footer
      [boilerplate]

-----------

[boilerplate]:
  div .signoff
    q
      "And this here is the good life."
    div .author
      "Philip Cali
```
