% AcitveHs User's Guide

> module UsersGuide_en where
>
> import ActiveHs.Base
> import MyPrelude
> import Prelude ()


Introduction
=============

ActiveHs is a Haskell source code presentation tool, developed for
education purposes.

The software is in prototype phase, although it already served more
than 700 000 user requests at Eötvös Loránd University Budapest, Hungary.

Note that this software has many rough edges; you are welcome to
work on it!


Typical Usage
=============

1.  Installation
1.  Start the `activehs` server near an .lhs file
1.  Open http://localhost:8000/*Modulename*.xml in a browser
4.  Modify the .lhs file(s) & reload the page
1.  Interact with the page
1.  Go back to 4. if needed
1.  Present your sources as interactive xhtml slides
1.  Make your activehs server accessible from the internet


Installation
============

ActiveHs installation:  

1.  `cabal update`  
1.  `cabal install activehs`

The activehs package depends on the pandoc and snap-server packages,
so they will be installed automatically.  
However, I propose to install them manually with the following flags:

-   cabal install pandoc -fhighlighting
-   cabal install snap-server -flibev


The following software components are optional, but should be installed separately:

-   [Graphviz](http://www.graphviz.org/)
-   [LaTeX](https://secure.wikimedia.org/wikipedia/en/wiki/LaTeX) and [dvipng](https://savannah.nongnu.org/projects/dvipng/)

***********************

I had some linker error during linking the `activehs` executable.
It was about an undefined symbol in the Hoogle library and
I have made a workaround about it.




Command-Line Options
====================

For basic usage you don't have to give any options;
just run `activehs` in a directory with .lhs files inside.

See `activehs --help` for more help on command-line options.





Markup Overview
===============

Only literate Haskell files are supported (with .lhs extension).

After the module name import the `ActiveHs.Base` module:

    > module Example where
    >
    > import ActiveHs.Base

Note that the module header and the import list should be in
the same code block (no empty lines between them)!


Markup which can be used in top level comments:

-   [pandoc/markdown](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)  
    Use [setext-style headers](http://johnmacfarlane.net/pandoc/README.html#setext-style-headers)
    instead of [atx-style headers](http://johnmacfarlane.net/pandoc/README.html#atx-style-headers),
    because atx-style headers interfere with the GHC .lhs preprocessor.
-   Graphviz and LaTeX code segments (only if graphviz and LaTeX + dvipng is installed)
-   ActiveHs commands


Graphviz Code Segments
======================

Example:

    ~~~~~ {.dot}
    A -> B -> C -> B
    A -> C
    ~~~~~

Output:

~~~~~ {.dot}
A -> B -> C -> B
A -> C
~~~~~


Example:

    ~~~~~ {.neato}
    A -> B -> C -> B
    A -> C
    ~~~~~

Output:

~~~~~ {.neato}
A -> B -> C -> B
A -> C
~~~~~

You can use the `.dot`, `.neato`, `.twopi`, `.circo`, `.fdp`, `.dfdp` segment names.

See the [graphviz gallery](http://www.graphviz.org/Gallery.php) for more complex examples.


LaTeX Code Segments
======================

LaTeX code segments will be converted into .png pictures.

Example:

    ~~~~~~ {.latex}
    \mbox{min}(x,y) = 
    \left\{\begin{array}{ll}
    x&\mbox{if }x\le y,
    y&\mbox{otherwise}.
    \end{array}\right.
    ~~~~~~

Output:

~~~~~~ {.latex}
\mbox{min}(x,y) = 
\left\{\begin{array}{ll}
x&\mbox{if }x\le y, \\
y&\mbox{otherwise}.
\end{array}\right.
~~~~~~


Slides
======

Pressing key ‘a’ in the browser switch between slide and normal mode.

Markup example:


    First Slide
    ===========

    slide content

    *********

    remark


    Second Slide
    ============

    slide content

    *********

    remark


*************

Each level one header starts a new slide.

In slide mode the remarks are not visible.
Remarks are the content after the first horizontal rule after each level one header.





ActiveHs Commands Overview
==========================

Every ActiveHs command begins with a letter followed by a `>` character and a space.

ActiveHs commands:

---- -----------------------------------------------------
`E>` **E**valuation
`F>` **F**olded evaluation
`A>` **A**nswer: show just the evaluated expression
`R>` **R**eply hidden; one should guess it
`H>` **H**idden test case
---- -----------------------------------------------------


Evaluation Command
==================

Example:

    E> [1..]

Output (interactive form):

E> [1..]

Example:

    E> sin . abs

Output:

E> sin . abs

Example:

    E> :t [1..]

Output:

E> :t [1..]


*********************

You may use the following colon-commands, but these are applied automatically too:

-   `:t` -- type inference
-   `:k` -- kind inference
-   `:?` -- hoogle search (in the database given by a command-line option)
-   `:i` -- information by hoogle (in the database given by a command-line option)

For other features see these guides:

-   [Prettyprinted values](Show_en.xml)
-   [Displayed functions](FunctionGraphs_en.xml)
-   [Diagrams](Diagrams_en.xml)


Folded Evaluation and Answer Commands
=========================

Evaluation example:

    E> 1 + 1

Output:

E> 1 + 1

Folded evaluation example:

    F> 1 + 1

Output:

F> 1 + 1

Answer example:

    A> 1 + 1

Output:

A> 1 + 1

********************

Folded evaluation is the same as evaluation, but the result is not shown for the first time.  

Answer is the same as evaluation, but only the result is shown.





Reply Command
=========================================

Example:

    Give an expression which is equal to the number of seconds in a day!

    R> 60 * 60 * 24

Output:

Give an expression which is equal to the number of seconds in a day!

R> 60 * 60 * 24

*******************************

If the equality cannot be checked for several reason, you get a reasonable detailed answer.  
[See the second half of this page.](Show_en.xml)



Exercises
=========================

It is possible to give more complex exercises to the readers.

Example: 

    Define the function `inc` which increments a number.

    > inc :: Num a => a -> a
    > inc = (+1)

    E> inc 3
    E> inc (4+4)

Output:

Define the function `inc` which increments a number.

> inc :: Num a => a -> a
> inc = (+1)

E> inc 3
E> inc (4+4)

******************************

What happens here?

1.  The definition of `inc` was replaced by a form in the output.
    ActiveHs do this if literate Haskell code is immediately followed
    by ActiveHs commands like `E>`.
1.  The user fill in the form (define `inc` here) and click on `Check` below the form.
1.  The server will check the form content.
    The test cases are derived from the ActiveHs commands immediately
    after the literate Haskell code.  
    For example, if the command is `E> inc 3`,
    then the test case will be `inc 3 == Original.inc 3`, where
    `inc` is the function defined by the user and `Original.inc` is
    the definition given in the .lhs file.
1.  If a the test case fails, a from will appear with that test case.  
    For example, if the user defines `inc = (+2)`, then `E> inc 3` fails,
    and `5 /= 4` will be shown because `inc 3` is not equal to `Original.inc 3`.
1.  If all tests cases succeeds, an empty form will be shown in which
    the user can enter more tests.


Exercises / QuickCheck tests
=========================

It is possible to give QuickCheck tests in exercises with the `H>` (**H**idden test) command.

Example:

    Define a function `plus` which adds two numbers.

    > plus :: Int -> Int -> Int
    > plus a b = a + b

    H> QCInt a ;; QCInt b ;; plus a b

Output:

Define a function `plus` which adds two numbers.

> plus :: Int -> Int -> Int
> plus a b = a + b

H> QCInt a ;; QCInt b ;; plus a b

(Try to define ``plus a b = 10 `min` a + b``)

***********************

Syntax of QuickCheck tests: 

pattern~1~ `;;` pattern~2~ ;; ... ;; pattern~n~ ;; expression

`QCInt` is a constructor which helps type checking.
Definition of `QCInt` is in the `ActiveHs.Base` module:

~~~~~ {.haskell}
newtype QCInt = QCInt Int
    deriving (Show, Arbitrary)
~~~~~

`ActiveHs.Base` defines to other constructors `QCBool` and `QCNat`.  
Definition of `QCNat`: 

~~~~~ {.haskell}
newtype QCNat = QCNat Int
    deriving (Show)

instance Arbitrary QCNat where
    ....
~~~~~

You can define your own helper-constructors.  
Just put the definitions in a separate visible module and import that module
in the .lhs file.



Customization
==============

You can customize the generated xml files with templates.  
The template mechanism is the following:

1.  The `actives` server decides the language of the .lhs file.  
    The language is guessed from the end of the filename, before the
    extension and after the last underline.
    For example, if the file name is `Overview_en.lsh`, then the
    language is `en`.  
    If the language cannot be guessed then the default language
    is used which is `en`. The default language can be
    changed by a command-line option.
1.  The server use the template file named *language*`.template`,
    like `en.template`. You can give the directory to search
    template files in with a command-line argument.  
    If you do not specify a template directory,
    the distribution's directory will be used which
    contains the `en.template` and `hu.template` template files.
1.  In the template file you give the skeleton of the generated
    xml files. For example, you can import any custom css files.


Currently there is no decent way to internationalize the messages
of the `activehs` server.
There is built-in support for English and Hungarian though; see
the Lang.hs file in sources.


Internet Access
===============

If your activehs server is accessible from the internet, I
advise the following:

-   Regenerate all pages with a script before publishing them:

	    for A in *.lhs; curl --url http://localhost:8000/${A%.lhs}.xml || exit 1; done;

-   Run the `activehs` server with
    the `--static` flag which prevents it from regenerating pages accidentally.

-   Use the `--magicname` flag, otherwise the exercise solutions
    may be guessed by the users.

-   Run a script which obeys the `activehs` server.
    There is an example script in the distribution (doc/watchserver.sh).

