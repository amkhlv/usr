General comments about Sphinx
*****************************

Configuring Sphinx
==================

In `conf.py`, must have:

.. code-block:: python

   add_module_names = False

and also:

.. code-block:: python

   autoclass_content = 'both'


Docstring for classes
=====================

If a class inherits somewhere, *e.g.* ``class MyButton(Gtk.Button)``, then put it into the ``__init__(self,...)``:

.. code-block:: python

   class MyButton(Gtk.Button):
     def __init__(self, text):
       """
       show the button

       :param str text: text of the button
       """

But if the class does not inherit from anywhere, than put it right below the ``class`` declaration:

.. code-block:: python

   class MyClass:
     """
     this is my class
     
     :param str greeting: my greeting
     """
     def __init__(self, greeting): 
       ...

Nuances of docstrings
=====================

Consider this example:

.. code-block:: python

   def get_row(n):
     """
     gives a Row

     :param int n: number of the row
     :returns: :class:`Row`
     :rtype: Row
     """

This is strange, one would think that just `:returns: linii2.Row` should work. But in fact, the full version with `:class:` is needed.
Moreover, we have also included `:rtype: Row`; this is for the **PyCharm** autocompletion. 

Here is another example:

.. code-block:: python

   def get_results(self,b = None):
        """
        get the list of all the results shown in the bottom panel, return type is `list` of :class:`Results`

        :param b:
        :return: list of :class:`Results`
        :rtype: list[Results]
        """
        return self.results

.. attention::

   Notice that **PyCharm** reads `rtype` while **Sphinx** reads `return`. This is why we cannot say `list[Results]` in `return`, as **Sphinx** does not understand generic type annotations. 


But we do use generic type annotation in `rtype` which is understood
by **PyCharm** according to this link_.

.. _link: http://www.jetbrains.com/pycharm/webhelp/type-hinting-in-pycharm.html