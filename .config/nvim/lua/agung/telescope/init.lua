local tele = require "telescope"

local actions = require "telescope.actions"
local action_state = require "telescope.actions.state"
-- local action_state = require "telescope.actions.state"

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
