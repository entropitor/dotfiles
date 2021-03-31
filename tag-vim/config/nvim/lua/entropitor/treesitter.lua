require "nvim-treesitter.configs".setup {
    ensure_installed = "maintained",
    highlight = {
        enable = true,
        disable = {
            "sh",
            "ledger"
        }
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<leader>et",
            node_incremental = "<leader>ej",
            scope_incremental = "<leader>eJ",
            node_decremental = "<leader>ek"
        }
    },
    indent = {
        enable = false
    }
}
