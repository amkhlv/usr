Links
=====

There is a very nice PDF document [here](http://www.nmt.edu/tcc/help/pubs/nxml/nxml.pdf)


Examples
========

<a name="examples"/>

Look at the examples [here](../../doc/xml/examples/)


A typical sample RNC is:

    element lines {
      element line {
        text,
        attribute aaa { text },
        attribute bbb { text }
      }*
    }

which would match:

    <lines>
      <line aaa="fruit" bbb="tropical">Mango</line>
      <line aaa="vegetable" bbb="Russian">Kartoshka</line>
    </line>


Workflow
========

Existing XML
------------

Suppose that we have a file `example.xml` (XML).

Create `example.rnc` (Relax NG Compact).

Then in the `example.xml` buffer in the Menu, choose: `XML` → `Set schema` → `File...` and choose the file `example.rnc`;
you will be offered to save the choice in the file `schemas.xml`; do accept.

Now `Emacs`, when opening your `XML` file, will look into the file `shemas.xml` (in the same folder as the file) and
see which schema corresponds to your `XML` file.


New XML
-------

First create `example.rnc` describing your structure.
Then create `schemas.xml` following [Examples](#examples) section above; something like this:

    <?xml version="1.0"?>
    <locatingRules xmlns="http://thaiopensource.com/ns/locating-rules/1.0">
      <uri resource="example.xml" uri="example.rnc"/>
    </locatingRules>


Useful key bindings
===================

If you need a block:

    <smth attr="good">
    ...
    </smth>

then start type `<smth attr="good"` and press `C-c C-b`

Similarly, if you need inline: `<smth>...</smth>` then type `<smth` and press `C-c C-i`

