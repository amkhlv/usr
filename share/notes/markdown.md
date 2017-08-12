
<a name="sectionTagsAndLinksWithin"></a>

Links to section within document
================================

As explained [here](https://stackoverflow.com/questions/2822089/how-to-link-to-part-of-the-same-document-in-markdown):

    # Foo

    See [Foo](#foo)

Note: just one `#` for all heading sizes, no space between `#` and anchor name, anchor tag names must be lowercase, and delimited by dashes if multi-word, _e.g._:

    [click on this link](#my-multi-word-header)

    ### My Multi Word Header


Tags and links within document
==============================

Suppose that I want to create a link to a section. Then I should put, at some point near the title:

    <a name="sectionExample"></a>

and then I can refer it later:

    see [Example Section](#sectionExample)

(And, to refer from another page, I will be able to use the address `http://some/page.html#sectionExample`)

see [Tags and Links Section](#sectionTagsAndLinksWithin)
