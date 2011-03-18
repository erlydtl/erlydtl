%% -*- mode: erlang -*-
{application, erlydtl,
 [{description, "ErlyDTL implements most but not all of the Django Template Language"},
  {vsn, "0.7.0"},
  {modules, [
             erlydtl,
             erlydtl_compiler,
             erlydtl_dateformat,
             erlydtl_deps,
             erlydtl_filters,
             erlydtl_parser,
             erlydtl_runtime,
             erlydtl_scanner,
             erlydtl_slice,
             erlydtl_i18n,
             i18n_manager,
             po_generator,
             po_scanner,
             sources_parser,
             gettext,
             sources_parser_unittests
            ]},
  {applications, [kernel, stdlib]},
  {registered, []}
 ]}.
