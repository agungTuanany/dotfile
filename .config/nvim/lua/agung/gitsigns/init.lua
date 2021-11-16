local M = {}


M.setup = function()
    require("gitsigns").setup {
        signs = {
            add = { hl = 'GitGutterAdd', text = '+' },
            change = { hl = 'GitGutterChange', text = '~' },
            delete = { hl = 'GitGutterDelete', text = '_' },
            topdelete = { hl = 'GitGutterDelete', text = '‾' },
            changedelete = { hl = 'GitGutterChange', text = '~' },
        }
    }
end

return M

-- local ok, gitsigns = pcall(require, 'gitsigns')
-- if not ok then
--     return {
--         print('gitsigns not installed')
--     }
-- end
