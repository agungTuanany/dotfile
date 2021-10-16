-- CREDIT to: https://github.com/wincent/wincent/blob/main/aspects/nvim/files/.config/nvim/lua/wincent/init.lua
local autoload = require'agung.autoload'

local agung = autoload('agung')

-- Using a real global here to make sure anything stashed in here (and
-- in `agung.g`) survives even after the last reference to it goes away.
_G.agung = agung

return agung
