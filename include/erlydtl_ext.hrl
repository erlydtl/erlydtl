
-record(dtl_context, {
          local_scopes = [], 
          block_dict = dict:new(), 
          blocktrans_fun = none,
          blocktrans_locales = [],
          auto_escape = off, 
          doc_root = "", 
          parse_trail = [],
          vars = [],
          filter_modules = [],
          custom_tags_dir = [],
          custom_tags_modules = [],
          reader = {file, read_file},
          module = [],
          compiler_options = [verbose, report_errors],
          binary_strings = true,
          force_recompile = false,
          locale = none,
          verbose = false,
          is_compiling_dir = false,
          extension_module = undefined,
          scanned_tokens = []
         }).

-record(ast_info, {
          dependencies = [],
          translatable_strings = [],
          translated_blocks= [],
          custom_tags = [],
          var_names = [],
          pre_render_asts = []}).

-record(treewalker, {
          counter = 0,
          safe = false,
          extension = undefined
         }).    

-record(scanner_state, {
          template=[],
          scanned=[],
          pos={1,1},
          state=in_text
        }).
