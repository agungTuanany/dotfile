vim.g.mapleader = ","
local conf = {nowait = true, noremap = true}
local s_conf = {nowait = true, noremap = true, silent = true}

-----
-- base template:
-- vim.keymap.set("n", "", "", conf)
-----

-- ## movement mapping
-- navigate windows with "< Ctrl-hjkl >" instead of "<Ctrl-w>" followed by "hjkl"
vim.keymap.set("n", "<c-h>", "<c-w><c-h>", conf)
vim.keymap.set("n", "<c-j>", "<c-w><c-j>", conf)
vim.keymap.set("n", "<c-k>", "<c-w><c-k>", conf)
vim.keymap.set("n", "<c-l>", "<c-w><c-l>", conf)

-- respect "hjkl" keys
vim.keymap.set("n", "<Up>", "<Nop>", conf)
vim.keymap.set("n", "<Down>", "<Nop>", conf)
vim.keymap.set("n", "<Left>", "<Nop>", conf)
vim.keymap.set("n", "<Right>", "<Nop>", conf)

-- movement by screen line instead of file line (for text wrap)
vim.keymap.set("n", "j", "gj", conf)
vim.keymap.set("n", "j", "gj", conf)
vim.keymap.set("n", "k", "gk", conf)
vim.keymap.set("n", "k", "gk", conf)

-- ## EFFECTIVENESS
-- faster scrolling 
vim.keymap.set("n", "<C-e>", "3<C-e>", conf)
vim.keymap.set("n", "<C-y>", "3<C-y>", conf)

-- duplicate lines, similar to eclipse
vim.keymap.set("n", "<C-S-Up>", "YP", conf)
vim.keymap.set("n", "<C-S-Down>", "YP", conf)

-- automatically "Esc". really uncommon type "jj", "kk", etc
vim.keymap.set("i", "<C-c>", "<Esc>", conf)
vim.keymap.set("i", "jj", "<Esc>", conf)
vim.keymap.set("i", "kk", "<Esc>", conf)
--vim.keymap.set("i", "hh", "<Esc>", conf)
--vim.keymap.set("i", "ll", "<Esc>", conf)

-- saving file and quit
vim.keymap.set("n", "<leader>w", vim.cmd.w, conf)
vim.keymap.set("n", "W", vim.cmd.w, conf)
vim.keymap.set("n", "<leader>q", vim.cmd.q, conf)

-- visually select the text was last edited/pasted
vim.keymap.set("n", "<leader>v", "`[v`]", conf)

-- move windows around (only work on same row)
vim.keymap.set("n", "<C-S-Right>", "<C-w>r", conf)
vim.keymap.set("n", "<C-S-Left>", "<C-w>R", conf)

-- bubble single lines
vim.keymap.set("n","<C-Up>", "ddkP", conf)
vim.keymap.set("n","<C-Down>", "ddp", conf)

-- bubble multiple lines
vim.keymap.set("v", "<C-Up>", "xkP`[V`]", conf)
vim.keymap.set("v", "<C-Down>", "xp`[V`]", conf)



-- ### SPLIT
vim.keymap.set("n", "<leader>sr", vim.cmd.vs, conf)     -- split right
vim.keymap.set("n", "<leader>vs", vim.cmd.vs, conf)     -- split right
vim.keymap.set("n", "<leader>sb", vim.cmd.sp, conf)     -- split below

-- ## TOGGLE things
vim.keymap.set("n", "\\", ":setlocal hlsearch!<CR>", conf)     -- toggle highlight search
vim.keymap.set("n", "<leader>\\", ":set list!<CR>", conf)      -- show tabs and trailing spaces
vim.keymap.set("n", "<leader>sp", ":set spell!<CR>", conf)     -- toggle spelling
vim.keymap.set("n", "<leader>n", ":set nu!<CR>", conf)         -- toggle numberline
vim.keymap.set("n", "<leader>nr", ":set rnu!<CR>", conf)       -- toggle relativenumber
vim.keymap.set("n", "<leader>cl", ":set cul!<CR>", conf)       -- toggle cursorline
vim.keymap.set("n", "<leader>cc", ":set cuc!<CR>", conf)       -- toggle cursorcolumn
vim.keymap.set("n", "<leader>wr", ":set wrap!<CR>", conf)      -- toggle wrap
vim.keymap.set("n", "<leader>rt", ":retab<CR>", conf)          -- change all "tabs" to "space"

vim.keymap.set("n", "<leader>rf", "gg=G g;", conf)              -- reindent code

-- resize splits (http://vim.wikia.com/wiki/Resize_splits_more_quickly)
vim.keymap.set("n", "<leader>+", ":exe 'resize '  . (winheight(0) * 3/2)<CR> ", conf)
vim.keymap.set("n", "<leader>-", ":exe 'resize '  . (winheight(0) * 2/3)<CR> ", conf)

--vim.keymap.set("n", "<leader>_", ":exe 'resize '  . (winwidth(0) * 2/3)<CR> ", conf)
--vim.keymap.set("n", "<leader>|", ":exe 'resize '  . (winwidth(0) * 3/2)<CR> ", conf)

-- PARENTHESIS
-- add space inside curly braces
vim.keymap.set("n", "<leader>)", "vi(xi <Esc>pa <Esc>", conf)
-- add space inside current brackets
vim.keymap.set("n", "<leader>}", "vi{xi <Esc>pa <Esc>", conf)
-- add space inside current <>
vim.keymap.set("n", "<leader>>", "vi<xi <Esc>pa <Esc>", conf)
-- add space inside current []
vim.keymap.set("n", "<leader>]", "vi[xi <Esc>pa <Esc>", conf)

-- ROUNDING WORD
-- Put brackets around word
--vim.keymap.set("n", "<leader>{", "vexi{<Esc>pea}<Esc>", { nowait = true })
--vim.keymap.set("n", "<leader>{", "i{<Esc>ea}<Esc>", {})
-- Put curly braces around word
--vim.keymap.set("n", "<leader>(", "vexi(<Esc>pea)<Esc>", { nowait = true })
--vim.keymap.set("n", "<leader>(", "i(<Esc>ea)<Esc>", {})
-- Put <> around word
--vim.keymap.set("n", "<leader><", "vexi<<Esc>pea><Esc>", { nowait = true })
--vim.keymap.set("n", "<leader><", "i<<Esc>ea><Esc>", {})
-- Put [] around word
--vim.keymap.set("n", "<leader>[", "i[<Esc>ea]<Esc>", {})



-- ## VISUAL MAPPING
vim.keymap.set("n", "v", "<C-v>", conf)
vim.keymap.set("n", "<C-v>", "v", conf)
vim.keymap.set("v", "v", "<C-v>", conf)
vim.keymap.set("v", "<C-v>", "v", conf)

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, conf)                             -- goto netrw
vim.keymap.set("n", "<leader>so", vim.cmd.so, conf)                             -- reload local code
vim.keymap.set("n", "<leader>00", ":so ~/.config/nvim/init.lua<CR>", conf)      -- reload local code



-- do not allow to "Ex mode"
vim.keymap.set("n", "Q", "<nop>")

print("'remap.lua: reloaded without any errors'")
