# LMXML Command-line App

The application will convert `.lmxml` files to XML in the stdout or
as a specified file.

## Installation

Installation is quick and easy via the [conscript][1] tool. Follow the
installation instructions on the README, and run the following command.

[1]: https://github.com/n8han/conscript

```
cs philcali/lmxml
```

## Usage

```
lmxml test.lmxml
# or
lmxml test.lmxml test.xml
```

## Template Fun

The app plays nicely with other command-line utilities in order to build
pseudo dynamic / static sites.

For example:

- [softprops/unplanned][up]: serving up a directory
- [philcali/monido][monido]: for watching file changes
- philcali/lmxml: to transpile LMXML and template JSON
- the below script called `build.sh` or something similar:

```
#! /bin/sh

lmxml -j src/data.json src/index.lmxml out/index.lmxml
echo Changes found in $1
```

Now do your work in `src`, and launch `up` in `out`. For example:

```
mkdir src
mkdir out
echo "!html html head title name" > src/index.lmxml
```

In one session, start the watcher:

```
monido -e sh build.sh src/
```

In another, start the static web server:

```
cd out/; up -p 8080
```

Finally, write the `data.json`:

```
echo "{\"name\": \"Test LMXML\" }" > src/data.json
```

The output should look like:

```
<!DOCTYPE html><html>
  <head>
    <title>Test LMXML</title>
  </head>
</html>
```

Changing either the json file or the source files will trigger a recompilation.
Pseudo dynamicism!

This kind of work is great for quickly prototyping the views, but for an
end to end Scala web experience, check out the [example app][example].

[example]: https://github.com/philcali/lmxml/tree/master/example
[up]: https://github.com/softprops/unplanned
[monido]: https://github.com/philcali/monido
