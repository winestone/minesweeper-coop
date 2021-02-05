# Introduction

A coop version of minesweeper written in Scala.

Controls:
- Left click to reveal a block.
- Right click to flag a block.

Building and running:
1. `sbt fastOptJS` to compile the client.
2. `sbt server/run` to run the server.
3. Visit `http://localhost:7777` or equivalent to play (port currently hardcoded in `server/src/main/scala/http/Server.scala`).
