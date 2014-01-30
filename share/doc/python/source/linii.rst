.. amkhlv documentation master file, created by
   sphinx-quickstart on Mon Feb 18 16:24:02 2013.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

linii
=====

Contents:

.. toctree::
   :maxdepth: 2


* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

To use ``linii`` in your script, say, e.g.:\ ::

    import linii
    linii.read_yaml("/home/andrei/a/tech/base/addr.yaml")

Example of the ``yaml`` file:\ ::

    data:
      students:
        columns:
        - [name, name, 1, 1]
        - [email, email, 1, 1]
        - [exam_note, exam_note, 3, 1]
        - [hw1_grade, hw1_grade, 1, 1]
        - [hw1_note, hw1_note, 3, 1]
        tablename: students
        balloons:
          name: Name of student
          email: Email of student
          hw1_note: |
            1.Construct an atlas of 2 maps for $S^2$.
            2.Verify that $(f\circ g)_* = f_*\circ g_*$.
      todo:
        columns:
        - [subject, subject, 1, 1]
        - [description, description, 10, 1]
        tablename: todo
    dbfile: /home/andrei/a/teaching/ift-2012/students.db

.. automodule:: linii
   :members:
