# phos

Phos (short for phosphophyllite) is both a Gemini client library for
Common Lisp and an experiment at making a GUI in lisp.

Phos provides three packages:

 - `phos/gemtext` which provides all the functionalities needed to parse
   text/gemini (gemtext),
 - `phos/gemini` which provides functions to make Gemini requests and
   parse the response,
 - `phos/ui` which is an experiment at writing an UI, but it's not
   really maintained.


### phos/gemtext

All the lines types are instance of the `element` base class.  The
`parse` routine is the main function, it takes a stream and produces a
list of lines.  `parse-string` is an helper which parses a string.

The generic function `unparse` turns an element, or a list of elements,
into a text representation that is written in the given stream.

There are also various helpers, like `title-p`, `link-p`, `line-eq`,
etc...


### phos/gemini

The main function is `with-gemini-request` which exposes a stream with
the gemini content and can be used to handle replies by streaming.  The
helper `request` insteads loads all the reply in memory and returns it
as a string if it was of a `text/*` MIME type or binary otherwise.


## License

ISC

