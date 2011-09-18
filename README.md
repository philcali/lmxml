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
```

