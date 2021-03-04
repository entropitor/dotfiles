local M = {}

function M.__reload ()
  package.loaded['entropitor.hello'] = nil
  print('Reloaded entropitor.hello')
end

function M.code_action ()
  local params = vim.lsp.util.make_range_params()
  params.context = {
    diagnostics = vim.lsp.diagnostic.get_line_diagnostics()
  }

  local received = {}
  local requested = { ids = {} }
  local handler = function (_, _, received_actions, received_client_id, bufnr)
    if received_client_id == nil then
      return
    end
    received[received_client_id]=received_actions

    local received_all = true
    for client_id, request_id in pairs(requested.ids) do
      if request_id == nil then
        goto continue
      end

      if received[client_id] == nil then
        received_all = false
        break
      end

      ::continue::
    end

    if not received_all then
      return
    end

    local all = {}
    for _, actions in pairs(received) do
      if actions ~= nil then
        for _, action in ipairs(actions) do
          table.insert(all, action)
        end
      end
    end
    local default_handler = vim.lsp.handlers['textDocument/codeAction']
    default_handler(nil, 'textDocument/codeAction', all, 0, bufnr)
  end

  requested.ids = vim.lsp.buf_request(0, 'textDocument/codeAction', params, handler)
end

function M.all_code_actions ()
  vim.lsp.buf.code_action({
    diagnostics = vim.lsp.diagnostic.get()
  })
end

function M.eslint_fix_all()
  vim.lsp.buf.execute_command({
    command = 'eslint.applyAllFixes';
    arguments = {{
        uri = vim.uri_from_bufnr(0),
        version = vim.lsp.util.buf_versions[vim.fn.bufnr()],
    }}
  })
end

return M
