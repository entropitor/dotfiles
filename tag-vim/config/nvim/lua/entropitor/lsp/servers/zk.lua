local util = require "lspconfig/util"
local configs = require "lspconfig/configs"

if not configs.zk then
    configs.zk = {
        default_config = {
            cmd = {"zk", "lsp", "--log", "/tmp/zk-lsp.log"},
            filetypes = {"markdown"},
            root_dir = util.root_pattern(".zk"),
            settings = {}
        }
    }
end
