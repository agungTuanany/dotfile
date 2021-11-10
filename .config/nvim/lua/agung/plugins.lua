local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

local use = require('packer').use
require('packer').startup({
    function()
        use 'wbthomason/packer.nvim'            -- backbone Package manager

        -- LSP CONFIG
        use 'neovim/nvim-lspconfig'             -- collection of configurations for built-in LSP client
        use 'hrsh7th/nvim-cmp'                  -- autocompletion plugin
        use 'hrsh7th/cmp-buffer'                -- nvim-cmp source for buffer words
        use 'hrsh7th/cmp-path'                  -- nvim-cmp source for path
        use 'hrsh7th/cmp-nvim-lua'              -- nvim-cmp source for neovim LUA API
        use 'hrsh7th/cmp-nvim-lsp'              -- nvim-cmp source for neovim builtin LSP client

        use 'onsails/lspkind-nvim'              -- vscode-like pictograms for neovim lsp completion items

        -- Snippets
        use 'saadparwaiz1/cmp_luasnip'          -- snippets source for nvim-cmp
        use 'L3MON4D3/LuaSnip'                  -- snippet plugin

        use 'tpope/vim-commentary'              -- comment stuff out
        use 'tpope/vim-unimpaired'              -- pairs of handy bracket mappings
        use 'tpope/vim-vinegar'                 -- combine netrw
        use 'tpope/vim-surround'                -- delete/change/add parentheses
        use 'tpope/vim-fugitive'                -- Git command in vim ":G"

        -- TREESITTER
        use 'nvim-treesitter/nvim-treesitter'   -- nvim Treesitter configurations and abstraction layer
        use 'kyazdani42/nvim-web-devicons'      -- lua 'fork' of vim-web-devicons for neovim for 'nvim-treesitter'

        -- TELESCOPE
        use 'nvim-telescope/telescope.nvim'     -- find, filter, preview, pick, all lua, all the time

        use 'nvim-lua/plenary.nvim'             -- full; complete; entire; absolute; unqualified. all the lua functions
        use 'nvim-lua/popup.nvim'               -- an implementation of the Popup API from vim in Neovim

        use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

        use 'lewis6991/gitsigns.nvim'           -- git integration for buffers
    end,
        display = {
    config = {
            -- open_fn = require('packer.util').float,
        }
    }
})
