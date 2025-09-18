robots-txt
==========

![Alt logo](logo.png)

[![CI](https://github.com/mwotton/robots/actions/workflows/ci.yml/badge.svg)](https://github.com/mwotton/robots/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/robots-txt.svg)](https://hackage.haskell.org/package/robots-txt)

`robots-txt` is a small Haskell library for parsing [robots.txt](http://www.robotstxt.org/orig.html) files and for checking whether a crawler is allowed to fetch a given path. It comes with a permissive parser that handles the quirks commonly found in real-world files and exposes helpers for evaluating crawler access rules.

Overview
--------

- Parse raw `robots.txt` content into a structured representation via `parseRobots`.
- Inspect or filter unparsable lines for diagnostics.
- Use `canAccess` to determine if a specific user-agent may fetch a given path.
- Respect directives such as `Allow`, `Disallow`, `Crawl-Delay`, `Request-rate`, and more.

Usage Example
-------------

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Robots

main :: IO ()
main = do
  let robotsTxt = BS.unlines
        [ "User-agent: googlebot"
        , "Disallow: /private"
        , "Allow: /private/public-summary.html"
        ]
  case parseRobots robotsTxt of
    Left err -> putStrLn ("Failed to parse robots.txt: " ++ err)
    Right robot -> do
      print (canAccess "googlebot" robot "/private/data.html")
      print (canAccess "googlebot" robot "/private/public-summary.html")
```

Running this program prints `False` for the disallowed path and `True` for the explicitly allowed page, illustrating how the parsed directives influence crawler behaviour.

Installation
------------

Add `robots-txt` to your Cabal file or `stack.yaml`, then build as usual:

- Cabal: run `cabal build` (or `cabal install robots-txt`).
- Stack: add the package to `extra-deps` if required and run `stack build`.

The test suite can be executed with `cabal test` or `stack test`.

```shell
cabal test
```

For more examples, check the modules under `Network.HTTP.Robots` in `src/`.
