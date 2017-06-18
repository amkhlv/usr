Introduction
============

The purpose is to convert the Markdown files in my writup collection to HTML.

Preprocessing links
===================

An `.md` file may contain a link to another local `.md` file:
 
like this [some other Markdown file](local/some.md)

When I convert to HTML, I preprocess every such link, replacing `.md` with `.html`. More examples:

[File in the same directory](here.md) and [link to a section](local/some.md#Introduction)

The only exception is when URL starts with something like `http://` or `https://` or even `file://`.
In this case, I leave it as it is:

[introduction via https](https://here.md) and [introduction via http](http://here.md) or [in file](file://here.md)

