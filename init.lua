local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')
Plug( 'junegunn/fzf', { ['do'] = vim.fn['fzf#install'] } )
Plug( 'junegunn/fzf.vim')
Plug( 'xolox/vim-misc')
Plug( 'xolox/vim-colorscheme-switcher')
Plug( 'mbbill/undotree')
Plug( 'lervag/vimtex')
Plug( 'neovimhaskell/haskell-vim' )
Plug( 'neovim/nvim-lspconfig' )
Plug( 'hrsh7th/cmp-nvim-lsp')
Plug( 'hrsh7th/cmp-buffer')
Plug( 'hrsh7th/cmp-path')
Plug( 'hrsh7th/cmp-cmdline')
Plug( 'hrsh7th/nvim-cmp')
Plug('SirVer/ultisnips')
Plug('quangnguyen30192/cmp-nvim-ultisnips')
Plug( 'honza/vim-snippets')
Plug( 'rafamadriz/friendly-snippets' )
Plug( 'vim-airline/vim-airline' )
Plug( 'vim-airline/vim-airline-themes' )
Plug( 'nvim-lua/plenary.nvim' )
Plug( 'lewis6991/gitsigns.nvim' )
Plug( 'nvim-treesitter/nvim-treesitter', { ['do'] = vim.fn[ ':TSUpdate' ] } )
Plug( 'tree-sitter/tree-sitter-typescript')
Plug( 'tree-sitter/tree-sitter-html')
Plug( 'tree-sitter/tree-sitter-css')
Plug( 'tree-sitter/tree-sitter-java')
Plug( 'davidgranstrom/scnvim', { ['do'] = vim.fn['scnvim#install()'] })
vim.call('plug#end')

vim.cmd([[
	source ~/.config/nvim/lua/keymappings.vim
	source ~/.config/nvim/lua/settings.vim
	source ~/.config/nvim/lua/vim-airline.vim
	source ~/.config/nvim/lua/scnvim.vim
]])

require('nvim-lspconfig')
require('nvim-cmp')
require('utilsnips')
require('gitsigns').setup()
require('nvim-treesitter.configs').setup { highlight = { enable = true } }
require('tree-sitter-typescript').typescript; // TypeScript grammar
require('tree-sitter-html')
require('tree-sitter-css')
require('tree-sitter-java')
