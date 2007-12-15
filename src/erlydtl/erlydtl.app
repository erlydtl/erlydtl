{application, erlydtl,
 [{description, "ErlyDTL Server"},
  {vsn, "0.1"},
  {modules, [
    erlydtl,
    erlydtl_app,
    erlydtl_sup,
    erlydtl_parser,
    erlydtl_scanner,
    erlydtl_tool
  ]},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {included_applications, []},
  {env, []},
  {mod, {erlydtl_app, []}}]}.