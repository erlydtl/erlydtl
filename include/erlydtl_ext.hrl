
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

-record(tag, {
          tag :: atom(),
          valid_in :: atom(),
          open :: string() | open_callback(),
          close :: string(),
          on_open :: tag_event() | list(tag_event()),
          on_close :: tag_event() | list(tag_event()),
          on_scan :: scan_callback()
         }).

-record(scanner_state, {
          template=[] :: string(),
          pos={1, 1} :: token_pos(),
          scanned=[] :: [token()],
          scope=[] :: [#tag{}],
          tags=[] :: [#tag{}]
         }).

-type token_type() :: atom().
-type token_pos() :: {Row::pos_integer(), Column::pos_integer()}.
-type token_value() :: term().
-type token() :: {token_type(), token_pos()}
               | {token_type(), token_pos(), token_value()}.
-type tag_event() :: token_type() | {token_type(), token_value() | no_value}
                   | fun((token(), #scanner_state{}) -> token()).
-type scan_callback() :: fun((#scanner_state{}) -> #scanner_state{})
                       | fun((char(), #scanner_state{}) -> #scanner_state{}).
-type open_callback() :: fun((test|open, #scanner_state{}) -> boolean()|#scanner_state{}).
