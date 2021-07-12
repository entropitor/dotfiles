local M = {}
local uv = vim.loop

function M.__reload()
    package.loaded["entropitor.dap.js"] = nil
    print("Reloaded entropitor.dap.js")
end

local onexit = nil
function M.stop()
    print("stopping")
    if onexit ~= nil then
        print("exiting")
        onexit()
    end
end

function M.connect(callback, config)
    print(vim.inspect(config))
    if config.__jsDebugChildServer then
        callback(
            {
                type = "server",
                port = tonumber(config.__jsDebugChildServer)
            }
        )
        return
    end

    M.stop()

    local stdin = uv.new_pipe(false)
    local stdout = uv.new_pipe(false)
    local stderr = uv.new_pipe(false)
    local handle, pid_or_err
    onexit = function()
        print("stopping")
        stdin:close()
        stdout:close()
        stderr:close()
        handle:close()
        handle:kill(9)
    end
    handle, pid_or_err =
        uv.spawn(
        "node",
        {
            args = {
                os.getenv("HOME") .. "/.vim/bundle/vscode-js-debug/out/src/vsDebugServer.js"
            },
            stdio = {stdin, stdout, stderr},
            detached = true
        },
        function()
            print("spawn.onExit called")
            M.stop()
        end
    )
    assert(handle, "Error trying to get DAP pid: " .. pid_or_err)
    print(vim.inspect({handle, pid_or_err}))
    stdout:read_start(
        function(_err, chunk)
            local port = chunk:gsub("\n", "")
            print(port)
            callback(
                {
                    type = "server",
                    port = port
                }
            )
        end
    )
end

vim.api.nvim_command("autocmd VimLeavePre * lua require('entropitor.dap.js').stop()")

return M
