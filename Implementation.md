Some implementation details
---------------------------

This small document describes a few details about the implementation
of web forms used in this package.

The basic idea is to combine the actions generating the HTML page
containing the source of the forms with the event handlers processing
a submitted form in one program (see the various examples
in directory `examples`). This has the advantage that input
fields do not require explicit names so that the consistency
of input fields and their event handlers can be controlled
by the Curry compiler. This technique exploits the functional logic
features of Curry and is explained in detail in
[this paper](http://www.informatik.uni-kiel.de/~mh/papers/PADL01.html).

This idea is implemented by starting the same executable for
generating and processing a form. Therefore, the `action`
attribute of a form occurring in a web page has the same URL
as the original page, i.e., each form has the structure

    <form method="post" action="?">
       <input type="hidden" name="FORMID" value="..."/>
       ...
    </form>

If a form is submitted by the client, the hidden field with name `FORMID`
indicates this form submission to the main program. The value associated
to this name is a unique identifier for this form. Usually, this
is the qualified name of the operation in the Curry source code
producing this form (this is checked by the program `curry2cgi`).
This information is used by the main program to invoke the
corresponding event handler. The input fields occurring in
the form (text fields, submit buttons, etc), identified by
logic variables in the source programs, are sequentially numbered
when a form is generated. This allows a simple identification
of the corresponding values submitted by the client.

The auxiliary program `curry2cgi` is used to transform a
Curry program containing forms into an executable.
`curry2cgi` collects all operations defining forms in a program
and generates the main program which is compiled as the executable
invoked by CGI. The generated program defines a main operation
of the following form:

    main :: IO ()
    main = HTML.CGI.Exec.printMainPage
             [ (<formid1>, HTML.CGI.Exec.execFormDef <formdef1>)
             , ...
             , (<formidk>, HTML.CGI.Exec.execFormDef <formdefk>)
             ]
             <mainpage>

Here `<formid1>,...<formidk>` are the identifiers of all form definitions
to be compiled. Thus, the operation `HTML.CGI.Exec.printMainPage`
is responsible to generate the initial HTML page or, if a form
is submitted, invoke the corresponding event handler defined
in the form.
