# ErlyDTL NEWS file

This file records noteworthy changes and additions to erlydtl as
suggested by the [GNU Coding
Standards](http://www.gnu.org/prep/standards/html_node/NEWS-File.html#NEWS-File).

## master (upcoming release)

* Fix issue with generated code for `for` loops (#167).

* Fix issue with using keywords as attributes (#177).

* Fix issue when including multiple templates extending a common base template (#176).

* New `w` option for toggling compile time warnings. Currently, there is only one, `non_block_tag`,
  which is triggered on any non-block data in an extends-template.
