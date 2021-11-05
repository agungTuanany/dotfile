local tele = require "telescope"

local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"

tele.comment = {
    print ("tele has been initialized from lua/agung/telescope/init.lua")
}

tele.setup {
    defaults = {
        prompt_prefix = "$ ",
        -- selection_caret = "> ",

        -- winblend = 0,
        mappings = {
            i = {
                -- ["<C-s>"] =  actions.select_horizontal,
                -- ["<C-n>"] =  actions.move_selection_next,
                -- ["<C-p>"] =  actions.move_selection_previous,
                ["<leader>as"] = function() print("action state get selected entry()",  Inspect(action_state.get_selected_entry())) end
            }
        },
    },
}

require "telescope".load_extension("fzf")

local tele_opts = { noremap = true, nowait = true }
vim.api.nvim_set_keymap('n', 'tf',  '<cmd>lua require("agung.telescope.tele_mapping").find_files()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'tb',  '<cmd>lua require("agung.telescope.tele_mapping").buffers()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'th',  '<cmd>lua require("agung.telescope.tele_mapping").help_tags()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'tlg', '<cmd>lua require("agung.telescope.tele_mapping").live_grep()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'tgs', '<cmd>lua require("agung.telescope.tele_mapping").grep_string()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'tct', '<cmd>lua require("agung.telescope.tele_mapping").current_buffer_tags()<CR>', tele_opts )
vim.api.nvim_set_keymap('n', 'tcf', '<cmd>lua require("agung.telescope.tele_mapping").current_fuzz_buffer() <CR>', tele_opts)
