-- ## movement mapping
-- Navigate Windows with '<Ctrl-hjkl>' instead of '<Ctrl-w>' followed by 'hjkl'
vim.api.nvim_set_keymap('n', '<c-h>', '<c-w><c-h>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<c-j>', '<c-w><c-j>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<c-k>', '<c-w><c-k>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<c-l>', '<c-w><c-l>', { nowait = true, noremap = true })

-- respect 'hjkl' keys
vim.api.nvim_set_keymap('n', '<Up>', '<Nop>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Down>', '<Nop>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Left>', '<Nop>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Right>', '<Nop>', { nowait = true, noremap = true })

-- movement by screen line instead of file line (for text wrap)
vim.api.nvim_set_keymap('n', 'j', 'gj', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('v', 'j', 'gj', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('v', 'k', 'gk', { nowait = true, noremap = true })

-- automatically 'ESC'. really uncommon to type 'jj', 'jk', etc
vim.api.nvim_set_keymap('i', 'jj', '<ESC>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('i', 'kk', '<ESC>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('i', 'hh', '<ESC>', { nowait = true, noremap = true })
--vim.api.nvim_set_keymap('i', 'll', '<ESC>', { nowait = true, noremap = true })

-- faster scrolling
vim.api.nvim_set_keymap('n', '<C-e>', '3<C-e>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-y>', '3<C-y>', { nowait = true, noremap = true })

-- ## visual mapping
-- most often
vim.api.nvim_set_keymap('n', 'v', '<C-v>', { nowait = true, noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-v>', 'v', { nowait = true, noremap = true, silent = true })
vim.api.nvim_set_keymap('v', 'v', '<C-v>', { nowait = true, noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-v>', 'v', { nowait = true, noremap = true, silent = true })

-- ## EFFECTIVENESS
-- Duplicate lines, similar to Eclipse
vim.api.nvim_set_keymap('n', '<C-S-Up>', 'YP', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-S-Down>', 'YP', { nowait = true, noremap = true })

-- saving file and quit
vim.api.nvim_set_keymap('n', '<leader>w', ':write<CR>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>q', ':quit<CR>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', 'W', ':write<CR>', { nowait = true, noremap = true })

-- do not allow to 'Ex mode'
vim.api.nvim_set_keymap('n', 'Q', '<Nop>', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', 'Q', ':quit<CR>', { nowait = true, noremap = true })

-- ## SPLIT
-- split right
vim.api.nvim_set_keymap('n', '<leader>sr', ':vsplit<CR>', { nowait = true, noremap = true })
-- split bottom
vim.api.nvim_set_keymap('n', '<leader>sb', ':split<CR>', { nowait = true, noremap = true })

-- ## TOGGLE
-- toggle highlight search
vim.api.nvim_set_keymap('n', '\\', ':setlocal hlsearch!<CR>', { nowait = true, noremap = true })

-- toggle showing 'tabs' and trailing spaces
vim.api.nvim_set_keymap('n', '<leader>\\', ':setlocal nolist!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle spell check
vim.api.nvim_set_keymap('n', '<leader>sp', ':setlocal spell!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle wrap
vim.api.nvim_set_keymap('n', '<leader>tw', ':setlocal wrap!<CR>', { nowait = true, noremap = true, silent = true })

-- wrapping text; hard wrap paragraph text (similar to TextMate ^Q)
--vim.api.nvim_set_keymap('n', '<leader>hw', 'gqip', { nowait = true, noremap = true })

-- toggle paste
vim.api.nvim_set_keymap('n', '<leader>ps', ':setlocal paste!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle cursorline
vim.api.nvim_set_keymap('n', '<leader>cl', ':setlocal cursorline!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle cursorcolumn
vim.api.nvim_set_keymap('n', '<leader>cc', ':setlocal cursorcolumn!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle number
vim.api.nvim_set_keymap('n', '<leader>n', ':setlocal number!<CR>', { nowait = true, noremap = true, silent = true })

-- toggle relativenumber
vim.api.nvim_set_keymap('n', '<leader>rn', ':setlocal relativenumber!<CR>', { nowait = true, noremap = true, silent = true })

-- reindent code
vim.api.nvim_set_keymap('n', '<leader>rf', 'gg=G', { nowait = true, noremap = true })

-- source the code
vim.api.nvim_set_keymap('n', '<leader>so', ':source ~/.config/nvim/init.lua<CR>', { nowait = true, noremap = true })

-- change all 'tabs' to 'space'
vim.api.nvim_set_keymap('n', '<leader>rt', ':retab!<CR>', { nowait = true, noremap = true })

-- visually select the text was last edited/pasted
vim.api.nvim_set_keymap('n', '<leader>v', '`[v`]', { nowait = true, noremap = true })

-- Move windows around (only work on same row)
vim.api.nvim_set_keymap('n', '<C-S-Right>', '<C-w>r', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-S-Left>', '<C-w>R', { nowait = true, noremap = true })

-- Bubble single lines
vim.api.nvim_set_keymap('n', '<C-Up>', 'ddkP', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-Down>', 'ddp', { nowait = true, noremap = true })

-- Bubble multiple lines
vim.api.nvim_set_keymap('v', '<C-Up>', 'xkP`[V`]', { nowait = true, noremap = true })
vim.api.nvim_set_keymap('v', '<C-Down>', 'xp`[V`]', { nowait = true, noremap = true })

-- resize splits (http://vim.wikia.com/wiki/Resize_splits_more_quickly)
vim.api.nvim_set_keymap('n', '<leader>+', ':exe "resize " . (winheight(0) * 3/2)<CR> ', { nowait = true, noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>-', ':exe "resize " . (winheight(0) * 2/3)<CR> ', { nowait = true, noremap = true, silent = true })

-- PARENTHESIS
-- add space inside curly braces
vim.api.nvim_set_keymap('n', '<leader>)', 'vi(xi <Esc>pa <Esc>', { nowait = true })
-- add space inside current brackets
vim.api.nvim_set_keymap('n', '<leader>}', 'vi{xi <Esc>pa <Esc>', { nowait = true })
-- add space inside current <>
vim.api.nvim_set_keymap('n', '<leader>>', 'vi<xi <Esc>pa <Esc>', { nowait = true })

-- ROUNDING WORD
-- Put brackets around word
--vim.api.nvim_set_keymap('n', '<leader>{', 'vexi{<Esc>pea}<Esc>', { nowait = true })
vim.api.nvim_set_keymap('n', '<leader>{', 'i{<Esc>ea}<Esc>', { nowait = true })
-- Put curly braces around word
--vim.api.nvim_set_keymap('n', '<leader>(', 'vexi(<Esc>pea)<Esc>', { nowait = true })
vim.api.nvim_set_keymap('n', '<leader>(', 'i(<Esc>ea)<Esc>', { nowait = true })
-- Put <> around word
--vim.api.nvim_set_keymap('n', '<leader><', 'vexi<<Esc>pea><Esc>', { nowait = true })
vim.api.nvim_set_keymap('n', '<leader><', 'i<<Esc>ea><Esc>', { nowait = true })

-- DELETE BRACKETS
-- delete brackets around word
vim.api.nvim_set_keymap('n', '<leader>{{', 'di{pF{xx<Esc>', { nowait = true })
-- delete curly braces around word
vim.api.nvim_set_keymap('n', '<leader>((', 'di(pF(xx<Esc>', { nowait = true })
-- delete <> around word
vim.api.nvim_set_keymap('n', '<leader><<', 'di<pF<xx<Esc>', { nowait = true })
