-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Zenburn (base16)'
config.window_background_opacity = .92

-- Fonts
config.font = wezterm.font 'Maple Mono Normal NF CN'
-- config.font = wezterm.font 'FiraCode Nerd Font Mono'

-- No tab bar
config.enable_tab_bar = false

-- Cursor style
-- config.default_cursor_style = 'BlinkingBar'

-- and finally, return the configuration to wezterm
return config
