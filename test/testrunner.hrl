-ifndef(noimport).
-import(erlydtl_eunit_testrunner, [run_test/1, run_compile/1, run_render/1]).
-endif.

-record(test, {
          title = "<no title>",
          module = erly_test,
          source,
          renderer = render,
          output,
          compile_opts = [report, return, {auto_escape, false}, force_recompile, {out_dir, false}],
          compile_vars = [],
          render_opts = [],
          render_vars = [],
          warnings = [],
          errors = []
         }).
