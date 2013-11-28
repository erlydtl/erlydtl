
TSD Language
============

The TSD language describes a stateful scanner which can be compiled
into an Erlang module for scanning text into tokens.

A TSD file consists of:

  * Attributes describing properties of the scanner.
  * Rules for transforming the input text to tokens.
  * Token transformation definitions.
  * Additional code, not affecting the compilation of the scanner.


Syntax
======

The TSD language has a rather basic syntax. 

Identifier
----------

Starts with a letter, followed by any number of alphanumeric or underscore characters.

Example: ``foo123``.

Number
------

Positive integers only.

Example: ``123``.

String
------

Strings are either quoted, using single or double quote (i.e. ' or "),
or, in case of a "symbols only" string, the quotes can be
omitted. Symbols are those characters that doesn't fit in any other
type.

In symbol strings, a backslash is used to escape the next character. A
few characters has special meaning (e.g. r, n, t and s) and for the
rest it is simply included in the string.

Example strings:

  ``"foo bar"`` ``'another string'`` ``!@#$`` ``{{\ }}``

In the last string, note how the the space is escaped so the string
doesn't end there.

Comment
-------

Comments start with ``%%`` and run until the end of the line.

Example: ``%% this is a comment.``

Keywords
--------

A few identifiers has special meaning, and is treated accordingly.

These are:

  * ``any``: can be used to indicate "any prefix" or "any state".
  * ``skip``: is used to indicate that a rule doesn't affect the
    scanned tokens.
  * ``until``: used to associate a string that ends a state.
  
There are also a number of symbols used as delimiters, and thus has
special meaning. In other words, these needs to be escaped if used in
symbols strings.

These are:

  * ``+``: can indicate either that a rule appends to a scanned token,
    or that a rules prefix should also match the associated string
    that ends the state.
  * ``-``: used to indicate a state that has no associated ending
    string.
  * ``:``: used as delimiter between the different parts of a rule, or
    as the `end of input` prefix marker.
  * ``,``: just a delimiter.
  * ``.``: marks the end of a definition.


Attributes
==========

Attributes are prefixed with a dash ``-``, followed by a identifier
and optional arguments and terminated by a period:

  ``-my_attr foo 123.``


Rules
=====

A rule has five parts:

  1. Priority. The rules are matched in order of their priority
     (lowest number first).
  2. Prefix. The prefix that should match the input text.
  3. Current state. The state of the scanner in order to test the
     rule.
  4. Guard expression. Optional expression to further refine if the
     rule applies.
  5. Rule body. The body is either an expression that implements the
     rule, or is given as actions and new state. Actions is optional
     and when not used indicated either by the ``skip`` keyword, or by
     the presence of only new state. New state is also optional, but
     when provided consists of an identifier optionally followed by
     the ``until`` keyword and a string, specifying the tag closer.


Token post processing
=====================

Rules for post processing tokens.


Erlang code
===========

Erlang code can be supplied in two distinct contexts, at the module
level, as forms, or in a rule, as expressions.

At the module level, it allows to add arbitrary code to the compiled
module, in the form of additional attributes, functions, include
directives and what not.

In rules, expressions can be used to add custom guards or to provide
the implementation body for the rule.

Code can run over multiple lines when newlines are escaped with a
backslash.

Note: there can only be at most one form/expression on a single line
(e.g. only a single ``end`` per line is supported).

Example:

  ``form -record(foo, {bar, baz}) end``
  ``form foo(Bar) -> {baz, Bar} end``
  ``expr H >= $0 andalso H =< $9 end``
