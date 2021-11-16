local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

local use = require('packer').use
require('packer').startup({
    function()
        use 'wbthomason/packer.nvim'            -- backbone Package manager

        -- LSP CONFIG
        use {
            'neovim/nvim-lspconfig',            -- collection of configurations for built-in LSP client
            'hrsh7th/nvim-cmp',                 -- autocompletion plugin
            'hrsh7th/cmp-buffer',               -- nvim-cmp source for buffer words
            'hrsh7th/cmp-path',                 -- nvim-cmp source for path
            'hrsh7th/cmp-nvim-lua',             -- nvim-cmp source for neovim LUA API
            'hrsh7th/cmp-nvim-lsp',             -- nvim-cmp source for neovim builtin LSP client

            'onsails/lspkind-nvim',             -- vscode-like pictograms for neovim lsp completion items

            -- SNIPPETS
            'saadparwaiz1/cmp_luasnip',         -- snippets source for nvim-cmp
            'L3MON4D3/LuaSnip',                 -- snippet plugin
    }


        use {
            'tpope/vim-commentary',             -- comment stuff out
            'tpope/vim-unimpaired',             -- pairs of handy bracket mappings
            'tpope/vim-vinegar',                -- combine netrw
            'tpope/vim-surround',               -- delete/change/add parentheses
            'tpope/vim-fugitive',               -- Git command in vim ":G"
    }

        -- TREESITTER
        use {
            'nvim-treesitter/nvim-treesitter',               -- nvim Treesitter configurations and abstraction layer
            'nvim-treesitter/nvim-treesitter-textobjects',   -- Collection of configurations for built-in LSP client
            'nvim-treesitter/playground',                    -- view treesitter information directly in Neovim

            'kyazdani42/nvim-web-devicons',                  -- lua 'fork' of vim-web-devicons for neovim for 'nvim-treesitter'
    }

        -- TELESCOPE
        use {
            "nvim-telescope/telescope.nvim",                -- find, filter, preview, pick, all lua, all the time
            requires = {
                "nvim-lua/plenary.nvim",
                "nvim-telescope/telescope-project.nvim",
                { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
            },
        }

        -- MISCELLANEOUS
        use {
            'nvim-lua/popup.nvim'               -- an implementation of the Popup API from vim in Neovim
        }

        -- GIT
        use {
            "lewis6991/gitsigns.nvim",
            requires = { "nvim-lua/plenary.nvim" },
            config = function()
                require('agung.gitsigns').setup()
            end
        }
    end,
    display = {
        config = {
            -- open_fn = require('packer.util').float,
        }
    }
})
