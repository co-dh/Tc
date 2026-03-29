-- Command matrix: single source of truth for all tv commands.
-- handler: dispatch name used by Lean code (decouples keys from logic).
-- key: physical key binding (single char). Handler names used for socket/programmatic dispatch.
-- Dotted names route to domain modules: nav.*, stk.*, folder.*, meta.*, freq.*, plot.*, filter.*.
-- arg_handler: for arg shortcuts, the target handler name (e.g. stk join shortcuts → 'join').

DROP TABLE IF EXISTS tv_commands;
CREATE TABLE tv_commands (
  obj      VARCHAR,
  verb     VARCHAR,
  label    VARCHAR,
  handler  VARCHAR DEFAULT 'nav.rowInc',
  key      VARCHAR DEFAULT '',
  resets_vs BOOLEAN DEFAULT false,
  arg_handler VARCHAR DEFAULT '',
  arg_default VARCHAR,
  view_ctx VARCHAR DEFAULT ''
);

INSERT INTO tv_commands VALUES
  -- row navigation (hjkl and arrows handled in Key.lean, not here)
  ('r', '>', 'Move cursor down',              'nav.rowInc',    '', false, '', NULL, ''),
  ('r', '<', 'Move cursor up',                'nav.rowDec',    '', false, '', NULL, ''),
  ('r', '[', 'Page up',                       'nav.rowPgUp',   '', false, '', NULL, ''),
  ('r', ']', 'Page down',                     'nav.rowPgDn',   '', false, '', NULL, ''),
  ('r', '{', 'Jump to top',                   'nav.rowTop',    '', false, '', NULL, ''),
  ('r', '}', 'Jump to bottom',                'nav.rowBot',    '', false, '', NULL, ''),
  ('r', '~', 'Select/deselect current row',   'nav.rowSel',    'T', false, '', NULL, ''),
  -- row search/filter (arg prefix: / = search, \ = filter)
  ('r', '/', 'Search for value in current column', 'filter.rowSearch', '/', true, '', '', ''),
  ('r', '\', 'Filter rows by PRQL expression',     'filter.rowFilter', '\', true, '', '', ''),
  ('r', '+', 'Jump to next search match',     'filter.searchNext', 'n', false, '', NULL, ''),
  ('r', '-', 'Jump to previous search match', 'filter.searchPrev', 'N', false, '', NULL, ''),

  -- col navigation
  ('c', '>', 'Move cursor right',             'nav.colInc',    '', false, '', NULL, ''),
  ('c', '<', 'Move cursor left',              'nav.colDec',    '', false, '', NULL, ''),
  ('c', '{', 'Jump to first column',          'nav.colFirst',  '', false, '', NULL, ''),
  ('c', '}', 'Jump to last column',           'nav.colLast',   '', false, '', NULL, ''),
  ('c', '!', 'Toggle group on current column','nav.colGrp',    '!', false, '', NULL, ''),
  ('c', '~', 'Hide/unhide current column',    'nav.colHide',   'H', false, '', NULL, ''),
  ('c', '\', 'Delete column(s) from query',   'nav.colExclude','x', true,  '', NULL, ''),
  ('c', '-', 'Shift key column left',         'nav.colShiftL', '', false, '', NULL, ''),
  ('c', '+', 'Shift key column right',        'nav.colShiftR', '', false, '', NULL, ''),
  -- col sort
  ('c', '[', 'Sort ascending',                'sort.asc',      '[', true,  '', NULL, ''),
  ('c', ']', 'Sort descending',               'sort.desc',     ']', true,  '', NULL, ''),
  -- col arg commands (prefix: : = split, = = derive, g = col jump)
  ('c', ':', 'Split column by delimiter',     'split',         ':', false, '', '', ''),
  ('c', '=', 'Derive new column (name = expr)', 'derive',      '=', false, '', '', ''),
  ('c', '/', 'Jump to column by name',        'filter.colSearch', 'g', true, '', '', ''),
  -- col plot
  ('c', '0', 'Plot: area chart',              'plot.area',     '', false, '', NULL, ''),
  ('c', '1', 'Plot: line chart',              'plot.line',     '', false, '', NULL, ''),
  ('c', '2', 'Plot: scatter plot',            'plot.scatter',  '', false, '', NULL, ''),
  ('c', '3', 'Plot: bar chart',               'plot.bar',      '', false, '', NULL, ''),
  ('c', '4', 'Plot: boxplot',                 'plot.box',      '', false, '', NULL, ''),
  ('c', '5', 'Plot: step chart',              'plot.step',     '', false, '', NULL, ''),
  ('c', '6', 'Plot: histogram',               'plot.hist',     '', false, '', NULL, ''),
  ('c', '7', 'Plot: density plot',            'plot.density',  '', false, '', NULL, ''),
  ('c', '8', 'Plot: violin plot',             'plot.violin',   '', false, '', NULL, ''),

  -- stk: view stack operations
  ('s', '/', 'Open command menu',              'menu',         ' ', false, '', NULL, ''),
  ('s', '~', 'Swap top two views',             'stk.swap',     'S', false, '', NULL, ''),
  ('s', '<', 'Close current view',             'stk.pop',      'q', true,  '', NULL, ''),
  ('s', '>', 'Duplicate current view',         'stk.dup',      '', false, '', NULL, ''),
  ('s', '{', 'Quit application',               'quit',         '', false, '', NULL, ''),
  ('s', '1', 'Transpose table (rows <-> columns)', 'xpose',   'X', false, '', NULL, ''),
  ('s', '2', 'Diff top two views',             'diff',         'd', false, '', NULL, ''),
  -- stk join shortcuts (verb→arg: bypass Effect, call join handler directly)
  ('s', '}', 'Join: inner',                    'stk.dup',      '', false, 'join', '0', ''),
  ('s', '[', 'Join: left',                     'stk.dup',      '', false, 'join', '1', ''),
  ('s', ']', 'Join: right',                    'stk.dup',      '', false, 'join', '2', ''),
  ('s', '+', 'Join: union',                    'stk.dup',      '', false, 'join', '3', ''),
  ('s', '-', 'Join: set difference',           'stk.dup',      '', false, 'join', '4', ''),

  -- info: precision, heatmap, scroll
  ('i', '~', 'Toggle info overlay',            'infoTog',      'I', false, '', NULL, ''),
  ('i', '<', 'Decrease decimal precision',     'precDec',      '', false, '', NULL, ''),
  ('i', '>', 'Increase decimal precision',     'precInc',      '', false, '', NULL, ''),
  ('i', '{', 'Set precision to 0 decimals',    'prec0',        '', false, '', NULL, ''),
  ('i', '}', 'Set precision to max (17)',       'precMax',      '', false, '', NULL, ''),
  ('i', '[', 'Scroll cell preview up',         'scrollUp',     '{', false, '', NULL, ''),
  ('i', ']', 'Scroll cell preview down',       'scrollDn',     '}', false, '', NULL, ''),
  ('i', '0', 'Heatmap: off',                   'heat.0',       '', false, '', NULL, ''),
  ('i', '1', 'Heatmap: numeric columns',       'heat.1',       '', false, '', NULL, ''),
  ('i', '2', 'Heatmap: categorical columns',   'heat.2',       '', false, '', NULL, ''),
  ('i', '3', 'Heatmap: all columns',           'heat.3',       '', false, '', NULL, ''),

  -- metaV: column metadata view
  ('M', '+', 'Open column metadata view',      'meta.push',    'M', true, '', NULL, ''),
  ('M', '~', 'Set selected rows as key columns','meta.setKey', '', true, '', NULL, 'colMeta'),
  ('M', '0', 'Select columns with null values', 'meta.selNull', '0', true, '', NULL, ''),
  ('M', '1', 'Select columns with single value','meta.selSingle','1', true, '', NULL, ''),

  -- freq: frequency table
  ('F', '+', 'Open frequency view',            'freq.open',    'F', true, '', NULL, ''),
  ('F', '~', 'Filter parent table by current row','freq.filter','', true, '', NULL, 'freqV'),

  -- fld: folder/file browser
  ('D', '+', 'Browse folder',                  'folder.push',  'D', true, '', NULL, ''),
  ('D', '~', 'Open file or enter directory',   'folder.enter', '', true, '', NULL, ''),
  ('D', '{', 'Go to parent directory',         'folder.parent','', true, '', NULL, ''),
  ('D', '-', 'Move to trash',                  'folder.del',   '', true, '', NULL, ''),
  ('D', '<', 'Decrease folder depth',          'folder.depthDec','', true, '', NULL, ''),
  ('D', '>', 'Increase folder depth',          'folder.depthInc','', true, '', NULL, ''),

  -- arg-only commands (prefix+payload via socket/test)
  ('e', '~', 'Export table (csv/parquet/json/ndjson)', 'export', 'e', false, '', '', ''),
  ('W', '~', 'Save session',                   'sessSave',     'W', false, '', '', ''),
  ('L', '~', 'Load session',                   'sessLoad',     '', false, '', '', ''),
  ('J', '~', 'Join tables',                    'join',         'J', false, '', '', '');
