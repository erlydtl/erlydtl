{application, erlydtl,
 [{description, "Django like templating library for Erlang"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, crypto]},
  {modules, [ 
              erlydtl
              ,erlydtl_compiler
              ,erlydtl_dateformat
              ,erlydtl_dateformat_tests
              ,erlydtl_deps
              ,erlydtl_example_variable_storage
              ,erlydtl_filters
              ,erlydtl_functional_tests
              ,erlydtl_parser
              ,erlydtl_runtime
              ,erlydtl_scanner
              ,erlydtl_unittests
             ]}
 ]}.
