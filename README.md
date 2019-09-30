html2: Support for HTML programming
===================================

This package contains libraries to support HTML programming.
It provides a similar API as package `html`.
In contract to package `html`, this library implements
dynamic web pages without server processes waiting for answers
form the client (web browser). Thus, it does not need
the Curry Port Name Server (CPNS), the HTML/CGI Registry, etc.
For this purpose, web forms are a bit more restricted:
each web form has an IO action to read the data required for
the form and a result page shown after submitting the form.
To implement the event handler inside a form without a process,
the read IO action is executed again when a form is submitted.

Some simple examples for dynamic web pages can be found in the
directory `examples`.

--------------------------------------------------------------------------
