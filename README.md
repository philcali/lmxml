# LMXML

The Light Markup to XML is a markup intended to make writing
xml or a recursive decent markup, easier. Head over to the
[wiki](https://github.com/philcali/lmxml/wiki) for an in-depth
tour of what can be expected from this library.

Interested in the [command-line app](https://github.com/philcali/lmxml/tree/master/app)?

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
    title "Test"
    script @type="text/javascript" @src="blah.js"
    link @rel="stylesheet" @type="text/css" @href="main.css"
  body
    div .page
      div .content "Blah Blah Blah"
      div #footer "This is where the copyright goes."
```

## The syntax

1. Indention is important
2. JSON-style attributes
3. Class attributes prefixed with "." and Id with "#"
4. Markdown Indirect link style templates
5. Indirect definitions are delimited from main document by `---`

__Example__:

```
div #page
  [boilerplate]
  div .content
    div #footer
      [boilerplate]

[boilerplate]:
  div .signoff
    q "And this here is the good life."
    div .author "Philip Cali
```

## Using The Library

Add it to your sbt build:

```
libraryDependencies += "com.github.philcali" %% "lmxml-core" % "0.1.3"
```

## Plugins!

Each LMXML process (parsing, converting, transforming, etc), is
designed to be extensible. Here's a list of each submodule and a link
to each description:

- `lmxml-html`: [HTML Shortcuts](https://github.com/philcali/lmxml/wiki/HTML-Shortcuts)
- `lmxml-template`: [File Templates](https://github.com/philcali/lmxml/wiki/File-Templates)
- `lmxml-json`: [JSON Transforms](https://github.com/philcali/lmxml/wiki/JSON-Transforms)
- `lmxml-cache`: [Compile Cache](https://github.com/philcali/lmxml/wiki/Compile-Cache)
- `lmxml-markdown`: [Markdown](https://github.com/philcali/lmxml/wiki/Markdown-Integration)

That's all _for now_.

## Learn by Example

I have included an example app that uses all listed submodules (except `lmxml-json`).

- Configuring LMXML with submodules [L19-26](https://github.com/philcali/lmxml/blob/master/example/src/main/scala/server.scala#L19)
- Using Configured LMXML [L87](https://github.com/philcali/lmxml/blob/master/example/src/main/scala/server.scala#L87)
- All the lmxml files in [data/templates](https://github.com/philcali/lmxml/tree/master/example/data/templates)

To run, simply:

```
git clone git://github.com/philcali/lmxml.git
cd lmxml
sbt "project lmxml-example" run
```

Try it out at [http://localhost:8080](http://localhost:8080).

## Try it on AppEngine!

You've got nothing to lose, really. Head on over the [GAE try LMXML app][try-lmxml],
to try the thing out. Give you hands a break from writing all those closing
tags.

## License

[MIT]

[MIT]: https://github.com/philcali/lmxml/blob/master/LICENSE
[try-lmxml]: http://try-lmxml.appspot.com/
