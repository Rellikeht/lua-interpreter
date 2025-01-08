vim.cmd("source ~/.vimrc")
require("functions")

local Plug = vim.fn["plug#"]
NVIM_DIR = vim.call("stdpath", "config")
local lua_dir = NVIM_DIR .. "/lua"
local vim_dir = "~/.vim"
local plug_dir = "plug-handlers/"

vim.call("plug#begin", NVIM_DIR .. "/plugins")
vim.cmd("source " .. vim_dir .. "/common-plugins.vim")

Plug("nvim-lua/plenary.nvim")

Plug(
  "nvim-treesitter/nvim-treesitter",
  {["do"] = ":silent! TSUpdate"}
)
Plug("norcalli/nvim-colorizer.lua")

Plug("nvim-treesitter/nvim-treesitter-textobjects")

Plug("neovim/nvim-lspconfig")
Plug("mfussenegger/nvim-jdtls")
Plug("ray-x/lsp_signature.nvim")

Plug("dcampos/nvim-snippy")
Plug("dcampos/cmp-snippy")
Plug("honza/vim-snippets")
Plug("ckunte/typst-snippets-vim")

Plug("hrsh7th/nvim-cmp")
Plug("hrsh7th/cmp-nvim-lsp")
Plug("hrsh7th/cmp-nvim-lsp-signature-help")
Plug("hrsh7th/cmp-nvim-lua")

Plug("hrsh7th/cmp-buffer")
Plug("hrsh7th/cmp-omni")
Plug("hrsh7th/cmp-path")

Plug("hrsh7th/cmp-cmdline")
Plug("rasulomaroff/cmp-bufname")
Plug("micangl/cmp-vimtex")

Plug("Vimjas/vint")
Plug("nvim-orgmode/orgmode")

Plug("mfussenegger/nvim-lint")
Plug("windwp/nvim-autopairs")
Plug("nvim-treesitter/nvim-treesitter-refactor")

Plug("hedyhli/outline.nvim")

Plug("jgdavey/tslime.vim")
Plug("puremourning/vimspector")

Plug("vim-test/vim-test")
Plug("nvim-neotest/neotest")

Plug("p00f/clangd_extensions.nvim")
Plug("ilyachur/cmake4vim")

Plug("jubnzv/mdeval.nvim")
Plug("AckslD/nvim-FeMaco.lua")

Plug("jakewvincent/mkdnflow.nvim")

Plug("ellisonleao/glow.nvim")

vim.call("plug#end")

local confMods = {
  "motion",
  "cmp",
  "look",
  "treesitter",
  "lspconfig",
  "coding",
  "formats",
  "other",
}

local confVimMods = {"testing"}

for _, i in ipairs(confMods) do require(plug_dir .. i) end
for _, i in ipairs(confVimMods) do
  vim.cmd("source " .. lua_dir .. "/" .. plug_dir .. i .. ".vim")
end
require("additional")

if fileReadable(NVIM_DIR .. "/local.lua") then
  loadfile(NVIM_DIR .. "/local.lua")()
end
