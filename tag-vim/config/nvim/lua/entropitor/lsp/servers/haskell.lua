local util = require "lspconfig/util"
local configs = require "lspconfig/configs"
if not configs.haskell then
    configs.haskell = {
        default_config = {
            cmd = {"haskell-language-server-wrapper", "--lsp"},
            filetypes = {"haskell"},
            root_dir = util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git")
        }
    }
end
