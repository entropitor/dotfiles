require("entropitor.lsp.servers.eslint")
require("entropitor.lsp.servers.haskell")
require("entropitor.lsp.servers.zk")

local on_attach = function(client, _bufnr)
    -- Set autocommands conditional on server_capabilities
    if client.resolved_capabilities.document_highlight then
        vim.api.nvim_exec(
            [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
            false
        )
    end
end
local on_init = function(client)
    client.config.flags = {}
    if client.config.flags then
        client.config.flags.allow_incremental_sync = true
    end
end

require "jot-lsp".setup()
local lspconfig = require "lspconfig"

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
        "documentation",
        "detail",
        "additionalTextEdits"
    }
}

local servers = {
    -- "eslint",
    "clangd",
    "bashls",
    "dockerls",
    -- "flow",
    "gopls",
    -- "hie",
    "haskell",
    "jot",
    "ocamlls",
    "ocamllsp",
    "pylsp",
    -- "rls",
    "rust_analyzer",
    "solargraph",
    "terraformls",
    "vimls",
    "zk"
}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
        capabilities = capabilities,
        on_attach = on_attach,
        on_init = on_init
    }
end

lspconfig.eslint.setup {
    capabilities = capabilities,
    on_attach = function(client, bufnr)
        on_attach(client, bufnr)
        vim.b.ale_enabled = 0
    end,
    on_init = on_init
}

lspconfig.tsserver.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    on_init = on_init
    -- root_dir = function(fname)
    --   return lspconfig.util.find_git_ancestor(fname) -- or lspconfig.util.root_pattern("package.json", "tsconfig.json", ".git")
    -- end;
}

lspconfig.fsautocomplete.setup {
    cmd = {"dotnet", vim.env.HOME .. "/.vim/bundle/Ionide-vim/fsac/fsautocomplete.dll", "--background-service-enabled"},
    capabilities = capabilities,
    on_attach = on_attach,
    on_init = on_init
}
lspconfig.yamlls.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    on_init = on_init,
    filetypes = {"yaml", "yaml.docker-compose"},
    settings = {
        yaml = {
            schemaStore = {
                enable = true,
                url = "https://www.schemastore.org/api/json/catalog.json"
            }
            -- schemas = {
            --  kubernetes = "*"
            --}
        }
    }
}
lspconfig.jsonls.setup {
    on_attach = on_attach,
    on_init = on_init,
    filetypes = {"json", "jsonc"},
    capabilities = capabilities,
    settings = {
        json = {
            schemas = {
                {
                    description = "TypeScript compiler configuration file",
                    fileMatch = {"tsconfig.json", "tsconfig.*.json"},
                    url = "http://json.schemastore.org/tsconfig"
                },
                {
                    description = "NPM package.json",
                    fileMatch = {"package.json"},
                    url = "http://json.schemastore.org/package"
                },
                {
                    description = "Lerna config",
                    fileMatch = {"lerna.json"},
                    url = "http://json.schemastore.org/lerna"
                },
                {
                    description = "Babel configuration",
                    fileMatch = {".babelrc.json", ".babelrc", "babel.config.json"},
                    url = "http://json.schemastore.org/lerna"
                },
                {
                    description = "ESLint config",
                    fileMatch = {".eslintrc.json", ".eslintrc"},
                    url = "http://json.schemastore.org/eslintrc"
                },
                {
                    description = "Bucklescript config",
                    fileMatch = {"bsconfig.json"},
                    url = "https://bucklescript.github.io/bucklescript/docson/build-schema.json"
                },
                {
                    description = "Prettier config",
                    fileMatch = {".prettierrc", ".prettierrc.json", "prettier.config.json"},
                    url = "http://json.schemastore.org/prettierrc"
                },
                {
                    description = "DataCamp ecs.json",
                    fileMatch = {"ecs.json", "ecs.*.json"},
                    url = "https://assets.ops.datacamp.com/ecs.schema.json#"
                }
            }
        }
    }
}

local system_name
if vim.fn.has("mac") == 1 then
    system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
    system_name = "Linux"
elseif vim.fn.has("win32") == 1 then
    system_name = "Windows"
else
    print("Unsupported system for sumneko")
end
-- set the path to the sumneko installation
local sumneko_root_path = vim.fn.stdpath("cache") .. "/lua-language-server"
local sumneko_binary = sumneko_root_path .. "/bin/" .. system_name .. "/lua-language-server"
lspconfig.sumneko_lua.setup {
    on_attach = on_attach,
    on_init = on_init,
    capabilities = capabilities,
    cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
                -- Setup your lua path
                path = vim.split(package.path, ";")
            },
            diagnostics = {
                enable = true,
                -- Get the language server to recognize the `vim` global
                globals = {"vim", "hs"}
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
                    [vim.fn.expand("$HOME/.hammerspoon/build/stubs")] = true
                }
            }
        }
    }
}
