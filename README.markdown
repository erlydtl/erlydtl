ErlyDTL
=======

ErlyDTL compiles Django Template Language to Erlang bytecode.

*Supported tags*: autoescape, block, blocktrans, comment, cycle, extends, filter, firstof, for, if, ifchanged, ifequal, ifnotequal, include, now, regroup, spaceless, ssi, templatetag, trans, verbatim, widthratio, with

_Unsupported tags_: csrf_token, url

*Supported filters*: add, addslashes, capfirst, center, cut, date, default, default_if_none, dictsort, dictsortreversed, divisibleby, escape, escapejs, filesizeformat, first, fix_ampersands, floatformat, force_escape, format_integer, format_number, get_digit, iriencode, join, last, length, length_is, linebreaks, linebreaksbr, linenumbers, ljust, lower, make_list, phonenumeric, pluralize, pprint, random, random_num, random_range, removetags, rjust, safe, safeseq, slice, slugify, stringformat, striptags, time, timesince, timeuntil, title, truncatechars, truncatewords, truncatewords_html, unordered_list, upper, urlencode, urlize, urlizetrunc, wordcount, wordwrap, yesno

_Unsupported filters_: _none_

Project homepage: <http://code.google.com/p/erlydtl/>

Language reference: <http://docs.djangoproject.com/en/dev/ref/templates/builtins/>


Compilation
-----------

To compile ErlyDTL, run 

    make
    
in this directory.


Template compilation
--------------------

Four ways:

    erlydtl:compile("/path/to/template.dtl", my_module_name)

    erlydtl:compile("/path/to/template.dtl", my_module_name, Options)

    erlydtl:compile(<<"<html>{{ foo }}</html>">>, my_module_name)

    erlydtl:compile(<<"<html>{{ foo }}</html>">>, my_module_name, Options)

Options is a proplist possibly containing:

* `out_dir` - Directory to store generated .beam files. If not specified, no
.beam files will be created.

* `doc_root` - Included template paths will be relative to this directory;
defaults to the compiled template's directory.

* `custom_tags_dir` - Directory of DTL files (no extension) includable as tags.
E.g. if $custom_tags_dir/foo contains `<b>{{ bar }}</b>`, then `{% foo bar=100 %}` 
will evaluate to `<b>100</b>`. Get it?

* `custom_tags_modules` - A list of modules to be used for handling custom
tags. The modules will be searched in order and take precedence over
`custom_tags_dir`. Each custom tag should correspond to an exported function
with one of the following signatures: 

    some_tag(TagVars)          -> iolist()
    some_tag(TagVars, Options) -> iolist()

The `TagVars` are variables provided to a custom tag in the template's body
(e.g. `{% foo bar=100 %}` results in `TagVars = [{"bar", 100}]`).
The `Options` are options passed as the second argument to the `render/2` call
at render-time. (These may include any options, not just `locale` and
`translation_fun`.)

* `custom_filters_modules` - A list of modules to be used for handling custom
filters. The modules will be searched in order and take precedence over the
built-in filters. Each custom filter should correspond to an exported filter,
e.g.

    some_filter(Value) -> iolist()

If the filter takes an argument (e.g. "foo:2"), the argument will be also be
passed in:

    some_filter(Value, Arg) -> iolist()

* `vars` - Variables (and their values) to evaluate at compile-time rather than
render-time. 

* `reader` - {module, function} tuple that takes a path to a template and returns
a binary with the file contents. Defaults to `{file, read_file}`. Useful
for reading templates from a network resource.

* `compiler_options` - Proplist passed directly to `compiler:forms/2`

* `force_recompile` - Recompile the module even if the source's checksum has not
changed. Useful for debugging.

* `locale` - The locale used for template compile. Requires erlang_gettext. It
will ask gettext_server for the string value on the provided locale.
For example, adding {locale, "en_US"} will call {key2str, Key, "en_US"}
for all string marked as trans (`{% trans "StringValue" %}` on templates).
See README_I18N.

* `blocktrans_fun` - A two-argument fun to use for translating `blocktrans`
blocks. This will be called once for each pair of `blocktrans` block and locale
specified in `blocktrans_locales`. The fun should take the form:

    Fun(Block::string(), Locale::string()) -> <<"ErlyDTL code">> | default

* `blocktrans_locales` - A list of locales to be passed to `blocktrans_fun`.
Defaults to [].

* `binary_strings` - Whether to compile strings as binary terms (rather than
lists). Defaults to `true`.

* `verbose` - Enable verbose printing of compilation results.

Helper compilation
------------------

Helpers provide additional templating functionality and can be used in
conjunction with the `custom_tags_module` option above. They can be created
from a directory of templates thusly:

    erlydtl:compile_dir("/path/to/dir", my_helper_module_name)
    
    erlydtl:compile_dir("/path/to/dir", my_helper_module_name, Options)

The resulting module will export a function for each template appearing
in the specified directory. Options is the same as for compile/3.

Compiling a helper module can be more efficient than using `custom_tags_dir`
because the helper functions will be compiled only once (rather than once
per template).


Usage (of a compiled template)
------------------------------ 

    my_compiled_template:render(Variables) -> {ok, IOList} | {error, Err}

Variables is a proplist, dict, gb_tree, or a parameterized module
(whose method names correspond to variable names). The variable 
values can be atoms, strings, binaries, or (nested) variables.

IOList is the rendered template.

    my_compiled_template:render(Variables, Options) -> 
            {ok, IOList} | {error, Err}

Same as `render/1`, but with the following options:

* `translation_fun` - A fun/1 that will be used to translate strings appearing
inside `{% trans %}` tags. The simplest TranslationFun would be `fun(Val) ->
Val end`

* `locale` - A string specifying the current locale, for use with the
`blocktrans_fun` compile-time option.

    my_compiled_template:translatable_strings() -> [String]

List of strings appearing in `{% trans %}` tags that can be overridden with
a dictionary passed to `render/2`.

    my_compiled_template:translated_blocks() -> [String]

List of strings appearing in `{% blocktrans %}...{% endblocktrans %}` blocks;
the translations (which can contain ErlyDTL code) are hard-coded into the
module and appear at render-time. To get a list of translatable blocks before
compile-time, use the provided `blocktrans_extractor` module.

    my_compiled_template:source() -> {FileName, CheckSum}

Name and checksum of the original template file.

    my_compiled_template:dependencies() -> [{FileName, CheckSum}]

List of names/checksums of templates included by the original template
file. Useful for frameworks that recompile a template only when the
template's dependencies change.

    my_compiled_template:variables() -> [Variable::atom()]

Sorted list of unique variables used in the template's body. The list can
be used for determining which variable bindings need to be passed to the
render/3 function.

Differences from standard Django Template Language
--------------------------------------------------

The "regroup" tag must have an ending "endregroup" tag.


Tests
-----

From a Unix shell, run:

    make test

Note that the tests will create some output in tests/output.
