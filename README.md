html2: Support for HTML programming
===================================

This package contains libraries to support HTML programming.
It provides a similar API as package `html`.
In contrast to package `html`, this library implements
dynamic web pages without server processes waiting for answers
form the client (web browser). Thus, it does not need
the Curry Port Name Server (CPNS), the HTML/CGI Registry, etc.
For this purpose, web forms are a bit more restricted:
each web form has an IO action to read the data required for
the form and a result page shown after submitting the form.
To implement the event handlers inside a form without a process,
the read IO action is executed again when a form is submitted.

All operations and smart constructors to implement
interactive web pages are defined in the library `HTML.Base`.
A form inside a web page must be defined as a
**public top-level operation** of type `HtmlFormDef`, like

    myForm :: HtmlFormDef String
    myForm = formDefWithID "Module.myForm" readData viewData

The first argument of a form definition must be the qualified name
of the operation (this will be checked by the script `curry2cgi`
which is used to install such web scripts as CGI executables).
The second argument is an IO action to read some data
used in the form, and the third argument is the actual view
of the form which usually contains buttons with event handlers
that are invoked when a form is submitted.

For convencience, there is also the form constructor `formDef`
where the ID argument can be omitted:

    myForm :: HtmlFormDef String
    myForm = formDef readData viewData

If this constructor is used, the script `curry2cgi` automatically
transforms the intermediate FlatCurry program such that the correct
form ID is added.

Some simple examples for dynamic web pages can be found in the
directory `examples`.

--------------------------------------------------------------------------

Known bugs:

The automatic addition of form IDs with the script `curry2cgi`
does not work completely with KiCS2 due to a problem in the front end
which unecessarily re-compiles TypedFlatCurry files used by KiCS2.
Thus, if PAKCS is not used, the correct IDs should be provided
in all definition of forms, i.e., `formDefWithID` should be used
to define forms (which is also more efficient).
This requirement will be checked by `curry2cgi`, i.e., if `formDefWithID`
is not used or used with a wrong identifier, an error is reported.

--------------------------------------------------------------------------

