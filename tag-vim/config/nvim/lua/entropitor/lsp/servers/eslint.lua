local util = require "lspconfig/util"
local configs = require "lspconfig/configs"
if not configs.eslint then
    configs.eslint = {
        default_config = {
            cmd = {"node", vim.env.HOME .. "/.vim/bundle/vscode-eslint/server/out/eslintServer.js", "--stdio"},
            filetypes = {
                "javascript",
                "javascriptreact",
                "javascript.jsx",
                "typescript",
                "typescriptreact",
                "typescript.tsx"
            },
            -- root_dir = util.root_pattern(".eslintrc.json", ".eslintrc.js", "package.json", "tsconfig.json", ".git");
            root_dir = util.root_pattern(".eslintrc.json", ".eslintrc.js"),
            settings = {
                validate = "on",
                packageManager = "yarn",
                codeAction = {
                    disableRuleComment = {
                        enable = true,
                        location = "separateLine"
                    },
                    showDocumentation = {
                        enable = true
                    }
                },
                codeActionOnSave = {
                    enable = false,
                    mode = "all"
                },
                format = false,
                quiet = false,
                onIgnoredFiles = "off",
                options = nil,
                run = "onType",
                nodePath = "/usr/local/bin/node",
                workspaceFolder = nil
            },
            handlers = {
                ["client/registerCapability"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    return {}
                end,
                ["eslint/noConfig"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    print("calling eslint/noConfig")
                    return {}
                end,
                ["eslint/noLibrary"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    print("calling eslint/noLibrary")
                    return {}
                end,
                ["eslint/openDoc"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    vim.cmd("!open " .. result.url)
                    return {}
                end,
                ["eslint/probeFailed"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    print("calling eslint/probeFailed")
                    return {}
                end,
                ["eslint/confirmESLintExecution"] = function(_, _, result, client_id)
                    if not result then
                        return
                    end
                    print("asking for eslint execution")
                    return 4 -- approved
                end
            }
        }
    }
end
