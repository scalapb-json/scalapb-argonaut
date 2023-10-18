# scalapb-argonaut
[![scaladoc](https://javadoc.io/badge2/io.github.scalapb-json/scalapb-argonaut_3/javadoc.svg)](https://javadoc.io/doc/io.github.scalapb-json/scalapb-argonaut_3/latest/api/scalapb_argonaut.html)

The structure of this project is hugely inspired by [scalapb-json4s](https://github.com/scalapb/scalapb-json4s)

## Dependency

Include in your `build.sbt` file

### core

```scala
libraryDependencies += "io.github.scalapb-json" %% "scalapb-argonaut" % "0.10.0"
```

for scala-js

```scala
libraryDependencies += "io.github.scalapb-json" %%% "scalapb-argonaut" % "0.10.0"
```

### macros

```scala
libraryDependencies += "io.github.scalapb-json" %%% "scalapb-argonaut-macros" % "0.10.0"
```

## Usage

There are four functions you can use directly to serialize/deserialize your messages:

```scala
JsonFormat.toJsonString(msg) // returns String
JsonFormat.toJson(msg) // returns Json

JsonFormat.fromJsonString(str) // return MessageType
JsonFormat.fromJson(json) // return MessageType
```

### Credits

- https://github.com/whisklabs/scalapb-playjson
- https://github.com/scalapb/scalapb-json4s
