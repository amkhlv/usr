Hyperlinks
==========

Inserting link
--------------

Right click on the object → `properties` → open __interactivity__ tab at the bottom of the dialogue,
in the line __onclick__ enter:

    window.open("otherfile.html","_top")

--- this is if you want to navigate to another `html` file. Or, if you want to navigate to `google` :

    window.open("https://www.google.com","_top")


Styling
-------

Point to the object, then open the XML editor by pressing `Shift+Ctrl+X`. The corresponding element will be already highlighted.
You have to change the `style` attribute; add:

    text-decoration:underline;
