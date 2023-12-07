return {
	{
    'norcalli/nvim-colorizer.lua',
    config = function ()
      require('colorizer').setup {
        '*',
        css = {
          rgb_fn = true,
          hsl_fn = true
        },
        html = {
          names = false
        }
      }
    end
	}
}
