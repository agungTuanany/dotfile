local builtin = require("telescope.builtin")

local conf = {nowait = true, noremap = true}
local s_conf = {nowait = true, noremap = true, silent = true}
-- {cwd = vim.fn.expand("%:p:h")}

vim.keymap.set("n", "<leader>tf", builtin.find_files, conf)
vim.keymap.set("n", "<C-p>", builtin.git_files, conf)
vim.keymap.set("n", "<leader>h", builtin.help_tags, conf)
vim.keymap.set("n", "<leader>tk", builtin.keymaps, conf)

vim.keymap.set("n", "<leader>ts", function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") });
end)
