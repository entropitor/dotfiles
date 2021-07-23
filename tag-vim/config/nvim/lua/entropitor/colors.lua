local lush = require("lush")
local hsl = lush.hsl

local M = {}

M.fix_highlighting = function()
    local darkGrey = hsl("#" .. vim.g.base16_gui03)
    local grey = hsl("#" .. vim.g.base16_gui05)
    local white = hsl("#" .. vim.g.base16_gui07)
    local red = hsl("#" .. vim.g.base16_gui08)
    local orange = hsl("#" .. vim.g.base16_gui09)
    local yellow = hsl("#" .. vim.g.base16_gui0A)
    local green = hsl("#" .. vim.g.base16_gui0B)
    local blue = hsl("#" .. vim.g.base16_gui0C)
    local cyan = hsl("#" .. vim.g.base16_gui0D)
    local purple = hsl("#" .. vim.g.base16_gui0E)
    local brown = hsl("#" .. vim.g.base16_gui0F)

    local spec =
        lush.parse(
        function()
            return {
                DiffAdd {bg = green.darken(70)},
                DiffAdded {bg = green.darken(70)},
                DiffNewFile {bg = green.darken(70)},
                DiffDelete {fg = red, bg = red.darken(80)},
                DiffRemoved {fg = red, bg = red.darken(80)},
                DiffChange {bg = darkGrey.darken(60)},
                DiffFile {bg = darkGrey.darken(60)},
                DiffLine {bg = darkGrey.darken(60)},
                DiffText {bg = darkGrey.darken(60)},
                -- Lsp
                LspDiagnosticsUnderlineError {
                    sp = red,
                    gui = "underline"
                },
                LspDiagnosticsSignError {
                    bg = red
                },
                LspDiagnosticsUnderlineWarning {
                    sp = yellow,
                    gui = "underline"
                },
                LspDiagnosticsSignWarning {
                    bg = yellow
                },
                LspDiagnosticsUnderlineInformation {
                    sp = blue,
                    gui = "underline"
                },
                LspDiagnosticsSignInformation {
                    bg = blue
                },
                LspDiagnosticsUnderlineHint {
                    sp = grey,
                    gui = "underline"
                },
                LspDiagnosticsSignHint {
                    bg = grey
                },
                LspReferenceText {
                    sp = white,
                    gui = "undercurl"
                },
                LspReferenceWrite {
                    LspReferenceText
                },
                LspReferenceRead {
                    LspReferenceText
                },
                -- Ultest
                UltestPass {
                    fg = green
                },
                UltestFail {
                    fg = red
                },
                UltestRunning {
                    fg = yellow
                },
                UltestBorder {
                    fg = purple
                },
                UltestSummaryInfo {
                    fg = cyan,
                    gui = "bold"
                },
                UltestSummaryFile {
                    UltestSummaryInfo
                },
                UltestSummaryNamespace {
                    UltestSummaryInfo
                }
            }
        end
    )
    vim.cmd(table.concat(lush.compile(spec), "\n"))
end

return M
