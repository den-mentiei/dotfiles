local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'Solarized Dark (Gogh)'

config.font = wezterm.font_with_fallback({
	  'Fira Code',
	  "Noto Color Emoji",
	  "Symbols Nerd Font Mono"
})

config.colors = {
   tab_bar = {
	  background = '#0b0022',

	  active_tab = {
		 bg_color = '#2b2042',
		 fg_color = '#c0c0c0',
	  },

	  inactive_tab = {
		 bg_color = '#1b1032',
		 fg_color = '#808080',
	  },
	  inactive_tab_hover = {
		 bg_color = '#3b3052',
		 fg_color = '#909090',
	  },

	  -- the new tab button
	  new_tab = {
		 bg_color = '#1b1032',
		 fg_color = '#808080',
	  },
	  new_tab_hover = {
		 bg_color = '#3b3052',
		 fg_color = '#909090',
	  },
   },
}

config.use_fancy_tab_bar            = false
config.tab_bar_at_bottom            = true
config.hide_tab_bar_if_only_one_tab = true

if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
	config.default_prog = { 'pwsh', '-NoLogo' }
end

-- @Incomplete API lacks proper pane resizing and action-based one
-- works incorrectly. Too lazy to fight it for now. :Smart_Split
-- wezterm.on('smart-split', function(window, _)
--    local tab     = window:active_tab()
--    local panes   = tab:panes()
--    local pinfos  = tab:panes_with_info()
--    local num     = #pinfos
--    local target  = nil
--    local dir     = nil
--    local resizes = {}
--    if num == 1 then
-- 	  target = panes[pinfos[1].index + 1]
-- 	  dir   = 'Right'
--    else
-- 	  local last_top = 0
-- 	  for _, p in pairs(pinfos) do
-- 		 if p.left ~= 0 then
-- 			local pane = panes[p.index + 1]
-- 			table.insert(resizes, pane)
-- 			if p.top >= last_top then
-- 			   last_top = p.top
-- 			   target   = pane
-- 			end
-- 		 end
-- 	  end
-- 	  dir = 'Bottom'
--    end

--    local new_pane = target:split { direction = dir }
--    table.insert(resizes, new_pane)

--    local rows     = tab:get_size().rows
--    local per_pane = math.floor(rows / #resizes)

--    print("adjusting")
--    for _, pane in pairs(resizes) do
-- 	  local size  = pane:get_dimensions().viewport_rows
-- 	  local adj   = size - per_pane
-- 	  local dir   = adj < 0 and 'Down' or 'Up'
-- 	  adj = math.abs(adj)
-- 	  pane:activate()
-- 	  window:perform_action(wezterm.action.AdjustPaneSize { dir, adj }, pane)
--    end

--    new_pane:activate()
-- end)

-- config.disable_default_key_bindings = true
local act  = wezterm.action
config.keys = {
   { key = 'p', mods = 'CTRL', action = act.ActivateCommandPalette },
   -- PANES/TABS
   { key = 't', mods = 'CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
   { key = 'w', mods = 'CTRL', action = act.CloseCurrentPane { confirm = true } },
	{ key = 'o', mods = 'CTRL', action = act.PaneSelect },
	-- SCROLLBACK
	{ key = 'j', mods = 'META', action = act.ScrollByLine(-1) },
	{ key = 'k', mods = 'META', action = act.ScrollByLine( 1) },
	{ key = 'PageUp',   action = act.ScrollByPage(-1) },
	{ key = 'PageDown', action = act.ScrollByPage( 1) },
	{
	   key = 'l',
	   mods = 'META',
	   action = act.Multiple {
		  act.ClearScrollback 'ScrollbackAndViewport',
		  act.SendKey { key = 'l', mods = 'CTRL' },
	   },
	},
	-- CLIPBOARD
	{ key = 'c', mods = 'META', action = act.CopyTo    'ClipboardAndPrimarySelection' },
	{ key = 'v', mods = 'META', action = act.PasteFrom 'Clipboard' },
	-- SPLITS
	{ key = 'Enter', mods = 'META',       action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
	{ key = 'Enter', mods = 'META|SHIFT', action = act.SplitVertical   { domain = 'CurrentPaneDomain' } },
	-- @Incomplete :Smart_Split
	-- { key = 'Enter', mods = 'META', action = act.EmitEvent 'smart-split' },
	{ key = 'h', mods = 'CTRL', action = act.ActivatePaneDirection 'Left' },
	{ key = 'j', mods = 'CTRL', action = act.ActivatePaneDirection 'Down' },
	{ key = 'k', mods = 'CTRL', action = act.ActivatePaneDirection 'Up' },
	{ key = 'l', mods = 'CTRL', action = act.ActivatePaneDirection 'Right' },
}

return config
