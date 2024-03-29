# ErlyDTL NEWS file

This file records noteworthy changes and additions to erlydtl as
suggested by the [GNU Coding
Standards](http://www.gnu.org/prep/standards/html_node/NEWS-File.html#NEWS-File).


## 0.14.0 (2021-03-19)

* #270 Added nowarn flag to export all in erlydtl_runtime
* #271 Hex wrong vsn and support OTP 23


## 0.13.0 (2020-02-15)

* #254 Semantical issue in rebar.config.script
* #255 OTP20 compatibility
* #261 OTP21 compatibility
* #263 Ensure modules are loaded when checking function_exported
* #267 Fix erlang:get_stacktrace() warning

## 0.12.1 (2016-09-21)

* #248 support filters in `{{ _(var) }}`.


## 0.12.0 (2016-08-08)

* #230 Use phash2/1 explicitly. It's not in compat layer

* #240 custom tags parameters parsing not handling newlines

* #242 Support for 19

* #246 make the 'join' filter work with numbers


## 0.11.1 (2015-10-26)

Fix build issues and missed update version info in .app-file.


## 0.11.0 (2015-10-25)

I've failed to keep track. Please check resolved issues on [github](https://github.com/erlydtl/erlydtl).


## 0.10.0 (2014-12-20)

* Fix issue with generated code for `for` loops (#167).

* Fix issue with using keywords as attributes (#177), or as variables (#194).

* Fix issue when including multiple templates extending a common base template (#176).

* New `w` option for toggling compile time warnings. Currently, there is only one, `non_block_tag`,
  which is triggered on any non-block data in an extends-template.

* Add missing features to the `cycle` tag (#195) (still missing
  independently stepping the cycle value).

* Support records in regroup tag (#191).

* Support for maps (#196).


## 0.9.4 (2014-04-15)

* Fix compile time variables and constants (#61)

* The `vars` compile time option has been deprecated in favor of
  `default_vars`.

* Support for translation contexts (#131)

  `context` is now a reserved keyword.

* Support for plural forms in `blocktrans` blocks (#131)

  As a side effect of the this, `count` and `plural` are now reserved
  keywords (the latter only as the tag name).

* Renamed compile options for `translation_fun` and `locales` to align
  with the render options counter parts.

* Support `_` prefix on template variables to avoid unused variable
  warnings, Erlang style (#164).

* Switched to `eunit_formatters` by @seancribbs for improved eunit reporting.

* All tests pass on Erlang 17.0! :)


## 0.9.3 (2014-03-27)

* Fix release process to work for non-git installations (#154).
* Fix list indexing (#155).
* New option for 0-based list/tuple access (#156) (see README for details).


## 0.9.2 (2014-03-22)

* Added NEWS file.
* Fixed broken `compile_dir` (#146).
* The backwards incompatible change in 0.9.1 for directory-compiled
  templates has been reverted. A new `render` function has been added
  instead (#148).
* Fixed broken escape feature (#150).
* Added translator comments (#152, #127) (@seriyps).


## 0.9.1 (2014-03-02)

This release brings a row of major internal improvements and also a
few new and improved features.

* Replaced most of `erl_syntax` use for merl (#123).
* Refactored compiler and test suite (#134, #136).
* Updated result value for directory-compiled (`compile_dir`)
  templates to return `{ok, Rendered} | {error, Reason}` just as for
  normally compiled templates. Please note that this is a **backwards
  incompatible change**.
* Regroup tag is now Django compatible! (the formerly required
  `endregroup` tag has been made optional) (#101).
* The `trans` tag now supports the `{% trans "text string" as Varname
  %}` construct to catch the translated text rather than output it in
  place.
* `{{ block.super }}` is now supported in child blocks (#18).
* `Template:variables()` now only lists referenced render variables
  (#118).
* New `erlydtl_library` behaviour for adding custom filters and tags
  (#137).
* New `load` tag added. Only difference to that of django is that we
  don't support loading a library from a package, as there are no
  notion of packages in Erlang (#100).
* New compile options supporting the load tag: `libraries` and
  `default_libraries`. See the readme. (the previous
  `custom_tags_modules` and `custom_filters_modules` are still
  supported, although being marked deprecated in favour of the new
  options).
* New compile options for aid in debugging erlydtl itself:
  `debug_compiler` and `debug_root` as described in the readme (#139).

See milestone 0.9.1 for a complete list of closed issues in this release.

Happy templating!


## 0.9.0 (2014-02-17)

After some fluctuating stability around *0.8.1*, this release brings a
set of improvements that has been tested not only by the test suite,
but also in a couple of projects.

* API changes. Added `_file` and `_template` suffixes to the compile
  functions to explicitly convey what is to be compiled.
* Autoescape is now enabled by default. Use the relatively new
  `auto_escape` option if you want this disabled by default.
* `blocktrans` blocks can now be translated to multiple locales in
  runtime. (thanks to @seriyps).
* `trans` and `_(...)` tags can now be translated to multiple locales
  at compile time. (thanks to @seriyps).
* Don't want to save the compiled .beam file? Use `{out_dir, false}`
  compile time option (this was possible before as well, when no
  out_dir was specified, but that results in a warning).
* The error reporting has been fixed for certain cases.


## 0.8.2 (2014-01-29)

My sincere apologies, this really should have been in the *0.8.1*
release.

This release brings the new error/warning options and return values
also to the `compile_dir` api function.

**Backwards incompatible notes:**

Please notice that the new error reporting options also introduces
subtle incompatible changes in the API (see #126).

It's not hard to adapt, but I want you all to be aware of this when
upgrading.

These changes were made already in *0.8.1*, but the specs were not, so
dialyzer did not pick up on them.


## 0.8.1 (2014-01-19)

This release is a clean-up release with a bunch of minor issues squeezed out.

* Improved options for error reporting.
* new auto_escape compile option for django purists.
* Cleaned out some warnings given by Dialyzer.
* Major improvements to the generated code for `for`-loops.

**Warning**

The `compile_dir` function is not updated to the new way of reporting
errors and warnings, and is in a semi-workable state. A fix is
committed to *master*, and will be included in the next release.


## 0.8.0 (2014-01-10)

This is a release to bring in all the changes together since *0.7.0*,
and will be the starting point for a lot of future improvements.


## 0.7.0 (2011-03-21

* Support no-argument version of `date` filter.


## 0.6.0 (2010-04-28)

* Implement `trans` tag with support for .po files Includes a .po
  parser and generator, docs, and tests. See README_I18N.

  Many thanks to David Garcia.


## 0.5.3 (2009-08-13)

* Create "ebin" directory if it does not exist.  Thanks to uri.sharf
  for the bug report, ja...@nialscorva.net for the patch.


## 0.5.2 (2009-02-13)

* Do not require a space between a variable name and closing bracket,
  e.g. allow `{{var1}}`.


## 0.5.1 (2008-10-23)

* Applied patch from Dan Milstein to fix a crash when rendering floats.
* Added a baseline failing test case for the problem.


## 0.5.0 (2008-08-07)

* Run functional tests from "make test".
* Clean up the output from all test suites.
* New README file.
* Remove create_parser stuff because "make" handles this.
* Make `erlydtl:compile/2,3` a wrapper for `erlydtl_compiler:compile/2,3`.
