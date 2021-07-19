local packer = require("packer")

local nmap = function(binding, command, opts)
    vim.api.nvim_set_keymap("n", binding, command, opts)
end
local vmap = function(binding, command, opts)
    vim.api.nvim_set_keymap("v", binding, command, opts)
end

return packer.startup(
    function(use)
        use {"wbthomason/packer.nvim"}

        -- ======================================
        -- ===   Show keybindings when lost   ===
        -- ======================================
        nmap("<leader>", ":<c-u>WhichKey '<leader>'<CR>", {noremap = true, silent = true})
        nmap("<leader>?", ":<c-u>WhichKey '<leader>'<CR>", {noremap = true, silent = true})
        vmap("<leader>", ":<c-u>WhichKeyVisual '<leader>'<CR>", {noremap = true, silent = true})
        vim.o.timeoutlen = 500
        vim.g.which_key_flatten = 0
        vim.g.which_key_default_group_name = "which_key_ignore"
        vim.g.which_key_map = {}
        vim.g.which_key_map["d"] = {name = "+debug"}
        vim.g.which_key_map["e"] = {name = "+edit"}
        vim.g.which_key_map["f"] = {name = "+file"}
        vim.g.which_key_map["g"] = {name = "+git"}
        vim.g.which_key_map["l"] = {name = "+language"}
        vim.g.which_key_map["n"] = {name = "+neovim"}
        vim.g.which_key_map["s"] = {name = "+search"}
        vim.g.which_key_map["t"] = {name = "+test"}
        vim.g.which_key_map["w"] = {name = "+tabs"}
        use {"liuchengxu/vim-which-key"}

        -- ===========================
        -- ===   Git Integration   ===
        -- ===========================
        use {
            "pwntester/octo.nvim",
            config = function()
                require "octo".setup()
            end
        }
    end
)
