local dap = require("dap")
dap.adapters.node = {
    type = "executable",
    command = "node",
    args = {
        os.getenv("HOME") .. "/.vim/bundle/vscode-node-debug2/out/src/nodeDebug.js"
    }
}
-- https://code.visualstudio.com/docs/nodejs/nodejs-debugging#_launch-configuration-attributes
dap.configurations.javascript = {
    {
        type = "node",
        request = "launch",
        name = "Launch file",
        program = "${workspaceFolder}/${file}"
    }
}
vim.fn.sign_define("DapBreakpoint", {text = "🛑", texthl = "", linehl = "", numhl = ""})
vim.api.nvim_command("au FileType dap-repl lua require('dap.ext.autocompl').attach()")

require("dap.ext.vscode").load_launchjs()
