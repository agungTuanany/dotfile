local tele_mappings = {}


tele_mappings.find_files = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").find_files(opts)
end

tele_mappings.buffers = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").buffers(opts)
end

tele_mappings.help_tags = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").help_tags(opts)
end

tele_mappings.live_grep = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").live_grep(opts)
end

tele_mappings.grep_string = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").grep_string(opts)
end

tele_mappings.current_buffer_tags = function()
    local opts = { sorting_strategy = "ascending" }
    require("telescope.builtin").current_buffer_tags(opts)
end

tele_mappings.current_fuzz_buffer = function()

    local opt = { sorting_strategy = "ascending" }
    require("telescope.builtin").current_buffer_fuzzy_find(opt)
end

return tele_mappings

