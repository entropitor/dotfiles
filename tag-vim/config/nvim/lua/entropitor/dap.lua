local dap = require("dap")
dap.adapters.node2 = {
    type = "executable",
    name = "node2",
    command = "node",
    args = {
        os.getenv("HOME") .. "/.vim/bundle/vscode-node-debug2/out/src/nodeDebug.js"
    }
}
-- dap.adapters["pwa-node"] = {
--     type = "executable",
--     command = "node",
--     args = {
--         os.getenv("HOME") .. "/.vim/bundle/vscode-js-debug/out/src/flatSessionLauncher.js"
--     }
-- }
dap.adapters["pwa-node"] = require("entropitor.dap.js").connect
-- https://code.visualstudio.com/docs/nodejs/nodejs-debugging#_launch-configuration-attributes
dap.configurations.javascript = {
    {
        type = "node2",
        request = "launch",
        name = "Launch file",
        program = "${workspaceFolder}/${file}"
    }
}
dap.configurations.typescript = {
    {
        type = "pwa-node",
        request = "attach",
        name = "Attach",
        continueOnAttach = false,
        attachExistingChildren = false,
        sourceMaps = true,
        skipFiles = {
            "node_modules/**/*",
            "<node_internals>/**"
        },
        port = 9229,
        trace = {
            logFile = "/Users/jens/Desktop/js-dap-log.txt"
        }
    },
    {
        type = "pwa-node",
        request = "launch",
        name = "Launch Program (vscode)",
        skipFiles = {
            "<node_internals>/**"
        },
        program = "${file}",
        preLaunchTask = "tsc= build - tsconfig.json",
        outFiles = {
            "${workspaceFolder}/dist/**/*.js"
        }
    },
    {
        name = "Debug Jest Tests (https://github.com/microsoft/vscode-js-debug/issues/214#issuecomment-572686921)",
        type = "pwa-node",
        request = "launch",
        -- trace = true,
        runtimeArgs = {
            "node_modules/.bin/jest",
            "--runInBand",
            "--no-coverage",
            "-t",
            "^Prospect Controller POST /api/prospect/subscribe-user registers the user to the newsletter$",
            "--",
            "apps/api/src/prospect/ProspectController.spec.ts"
        },
        console = "integratedTerminal",
        internalConsoleOptions = "neverOpen",
        trace = {
            logFile = "/Users/jens/Desktop/js-dap-log.txt"
        }
    },
    {
        type = "pwa-node",
        request = "launch",
        name = "Run jest",
        localRoot = "${workspaceFolder}",
        runtimeArgs = {"--inspect"},
        program = "./node_modules/.bin/jest",
        args = {
            "--no-coverage",
            "-t",
            "^Prospect Controller POST /api/prospect/subscribe-user registers the user to the newsletter$",
            "--",
            "apps/api/src/prospect/ProspectController.spec.ts"
        }
    },
    {
        type = "node2",
        request = "launch",
        name = "Launch file",
        program = function()
            return vim.fn.expand("%:p"):gsub(".ts", ".js")
        end,
        -- runtimeArgs = {"--nolazy", "-r", "ts-node/register"},
        -- outFiles = {"${workspaceFolder}/**/dist/**/*.js"}
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        protocol = "inspector",
        console = "integratedTerminal"
    }
}

table.insert(
    dap.request_handlers["attachedChildSession"],
    function(_ --[[session]], arguments)
        if arguments.config.type == "pwa-chrome" then
            arguments.config.type = "pwa-node"
        end
        arguments.config.sourceMaps = true
        arguments.config.skipFiles = {
            "node_modules/**/*",
            "<node_internals>/**"
        }
        vim.schedule(
            function()
                dap.run(
                    arguments.config,
                    {
                        keepPrevSessionOpen = true,
                        preprocess_request = function(payload)
                            payload.sessionId = arguments.config.__pendingTargetId
                        end
                    }
                )
            end
        )
    end
)
-- function dap.Session:event_loadedSource(_)
--     -- Do nothing
-- end

vim.fn.sign_define("DapBreakpoint", {text = "🛑", texthl = "", linehl = "", numhl = ""})
vim.api.nvim_command("au FileType dap-repl lua require('dap.ext.autocompl').attach()")

-- require("dap.ext.vscode").load_launchjs()
