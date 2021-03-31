local dap = require('dap')
dap.adapters.node = {
  type = 'executable';
  command = 'node';
  args = {
    os.getenv('HOME') .. '/.vim/bundle/vscode-node-debug2/out/src/nodeDebug.js';
  };
}
dap.configurations.javascript = {
  {
    type = 'node';
    request = 'launch';
    name = "Launch file";
    program = "${workspaceFolder}/${file}";
  },
}
