local M = {}        -- M is equal to "Module"

-- {{ === HELPER
local o = vim.o 	-- same as ':set' 
local b = vim.bo 	-- same as ':setlocal'
local g = vim.g 	-- get or set an 'option' like ':set' only 'global_value'
local w = vim.wo	-- same as ':setlocal'
local cmd = vim.cmd	
local opt = vim.opt 	-- same as 'set-option'

local api = vim.api
--}}


g.mapleader = ","       -- set mapleader-key


-- membuat 'default'-function untuk memanggil dan mengatur semua mapping.
function M.default()
	--M.set_mapping()
	M.saving()
	M.split()
    M.search()

end


function M.set_mapping()
	print ("set mapping function has called")

end

function M.maps(mapdict, opts)
    for m = 1, #mapdict do
        local mode = mapdict[m][1]
        local lhs = mapdict[m][2]
        local rhs = mapdict[m][3]

        api.nvim_set_keymap(mode, lhs, rhs, opts)
    end
end




function M.saving()
    local opt = { nowait = true, noremap = true }
    local maps = {
        {"n", "<leader>w", ":w<CR>"},
        {"n", "<leader>q", ":q<CR>"},
        {"n", "<leader>rl", ":so ~/.config/nvim/init.lua<CR>"},
    }

    M.maps(maps, opt)
end

function M.split()
    local opt = {noremap = true}
    local maps = {
        {"n", "<leader>sr", ":vsplit<CR>"},
        {"n", "<leader>sb", ":split<CR>"}
    }

    M.maps(maps, opt)
end

function M.search()
    local opt = { nowaot = true, noremap = true }
    local maps = {
        {"n", "<leader>h", ":Helptags<CR>"}
    }
end

return M
