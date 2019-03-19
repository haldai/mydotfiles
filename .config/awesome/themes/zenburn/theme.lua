-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
--     Modified by haldai    --
-------------------------------
local themes_path = require("gears.filesystem").get_themes_dir()
local dpi = require("beautiful.xresources").apply_dpi
local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")

local os = os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

-- {{{ Main
local theme = {}
theme.wallpaper = "~/Pictures/DD.jpg"
-- }}}

-- {{{ Styles
theme.font = "方正宋刻本秀楷 13"

-- {{{ Colors
local black0 = "#2B2B2B"
local black1 = "#3F3F3F"
local black2 = "#494949"
local black3 = "#4F4F4F"

local red1 = "#CC9393"
local red2 = "#9C6363"

local green1 = "#7F9F7F"
local green2 = "#5F7F5F"

local yellow1 = "#F0DFAF"
local yellow2 = "#D0BF8F"

local blue1 = "#8CD0D3"
local blue2 = "#94BFF3"

local magenta1 = "#DC8CC3"
local magenta2 = "#CB7BB2"

local cyan1 = "#93E0E3"
local cyan2 = "#82CFD2"

local white0 = "#BDBDBD"
local white1 = "#DCDCCC"
local white2 = "#FFFFEF"

local orange = "#DFAF8F"

theme.fg_normal  = white1
theme.fg_focus   = orange
theme.fg_urgent  = red1
theme.bg_normal  = black2
theme.bg_focus   = black1
theme.bg_urgent  = black2
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.useless_gap   = dpi(2)
theme.border_width  = dpi(2)
theme.border_normal = black3
theme.border_focus  = white1
theme.border_marked = red1
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = black1
theme.titlebar_bg_normal = black2
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CC9393"
theme.tooltip_border_color = white2
theme.tooltip_bg = black2
theme.tooltip_fg = white1
theme.tooltip_font = theme.font
theme.tooltip_border_width = 2
theme.tooltip_opacity = 90
-- theme.tooltip_shape = nil
theme.tooltip_align = bottom_right
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.fg_widget        = "#AECF96"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
theme.widget_main_color = white1
theme.widget_red = red1
theme.widget_yellow = yellow1
theme.widget_green = green1
theme.widget_black = black2
theme.widget_transparent = "#00000000"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = red1
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = dpi(28)
theme.menu_width  = dpi(180)
theme.menu_border_color = white1
-- }}}

-- {{{ Icons
theme.lain_icons = os.getenv("HOME") .. "/.config/awesome/lain/icons/layout/zenburn/"
-- {{{ Taglist
theme.taglist_squares_sel   = themes_path .. "zenburn/taglist/squarefz.png"
theme.taglist_squares_unsel = themes_path .. "zenburn/taglist/squarez.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon           = themes_path .. "zenburn/awesome-icon.png"
theme.menu_submenu_icon      = themes_path .. "default/submenu.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = themes_path .. "zenburn/layouts/tile.png"
theme.layout_tileleft   = themes_path .. "zenburn/layouts/tileleft.png"
theme.layout_tilebottom = themes_path .. "zenburn/layouts/tilebottom.png"
theme.layout_tiletop    = themes_path .. "zenburn/layouts/tiletop.png"
theme.layout_fairv      = themes_path .. "zenburn/layouts/fairv.png"
theme.layout_fairh      = themes_path .. "zenburn/layouts/fairh.png"
theme.layout_spiral     = themes_path .. "zenburn/layouts/spiral.png"
theme.layout_dwindle    = themes_path .. "zenburn/layouts/dwindle.png"
theme.layout_max        = themes_path .. "zenburn/layouts/max.png"
theme.layout_fullscreen = themes_path .. "zenburn/layouts/fullscreen.png"
theme.layout_magnifier  = themes_path .. "zenburn/layouts/magnifier.png"
theme.layout_floating   = themes_path .. "zenburn/layouts/floating.png"
theme.layout_cornernw   = themes_path .. "zenburn/layouts/cornernw.png"
theme.layout_cornerne   = themes_path .. "zenburn/layouts/cornerne.png"
theme.layout_cornersw   = themes_path .. "zenburn/layouts/cornersw.png"
theme.layout_cornerse   = themes_path .. "zenburn/layouts/cornerse.png"
theme.layout_termfair    = theme.lain_icons .. "termfair.png"
theme.layout_centerfair  = theme.lain_icons .. "centerfair.png"  -- termfair.center
theme.layout_cascade     = theme.lain_icons .. "cascade.png"
theme.layout_cascadetile = theme.lain_icons .. "cascadetile.png" -- cascade.tile
theme.layout_centerwork  = theme.lain_icons .. "centerwork.png"
theme.layout_centerworkh = theme.lain_icons .. "centerworkh.png" -- centerwork.horizontal
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = themes_path .. "zenburn/titlebar/close_focus.png"
theme.titlebar_close_button_normal = themes_path .. "zenburn/titlebar/close_normal.png"

theme.titlebar_minimize_button_normal = themes_path .. "default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path .. "default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_focus_active  = themes_path .. "zenburn/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = themes_path .. "zenburn/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path .. "zenburn/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = themes_path .. "zenburn/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = themes_path .. "zenburn/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = themes_path .. "zenburn/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path .. "zenburn/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = themes_path .. "zenburn/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = themes_path .. "zenburn/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = themes_path .. "zenburn/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = themes_path .. "zenburn/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = themes_path .. "zenburn/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = themes_path .. "zenburn/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = themes_path .. "zenburn/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path .. "zenburn/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = themes_path .. "zenburn/titlebar/maximized_normal_inactive.png"

theme.widget_ac = themes_path .. "zenburn/myicon/ac.png"
theme.widget_battery = themes_path .. "zenburn/myicon/battery.png"
theme.widget_battery_mid = themes_path .. "zenburn/myicon/battery_mid.png"
theme.widget_battery_low = themes_path .. "zenburn/myicon/battery_low.png"
theme.widget_battery_empty = themes_path .. "zenburn/myicon/battery_empty.png"
-- }}}
-- }}}

awful.util.tagnames = { "", "", "", "", "", "", "", "", "" }

local markup     = lain.util.markup
local separators = lain.util.separators

--- {{{ Widgets
local mytextclock = wibox.widget.textclock(" %H:%M ")
mytextclock.font = theme.font

-- {{{ CPU load
theme.cpugraph = wibox.widget {
  forced_width = 40,
  paddings = 1,
  border_width = 1,
  border_color = white2,
  color = white1,
  background_color = black2,
  widget = wibox.widget.graph
}
vicious.register(theme.cpugraph, vicious.widgets.cpu, "$1")
local cpubg = wibox.container.background(theme.cpugraph, black2, gears.shape.rectangle)
local cpuwidget = wibox.container.margin(cpubg, 7, 7, 5, 5)
--- }}}

--- {{{ Volume bar
theme.volume = lain.widget.alsabar {
  ticks = true, width = 67,
  colors = {
    background = black0,
    mute = red1,
    unmute = white1
  },
  notification_preset = { font = "方正宋刻本秀楷 10" }
}
theme.volume.bar:buttons(
  my_table.join (
    awful.button({}, 1, function()
        awful.spawn(string.format("%s -e alsamixer", terminal))
    end),
    awful.button({}, 2, function()
        os.execute(string.format("%s set %s 100%%", theme.volume.cmd, theme.volume.channel))
        theme.volume.update()
    end),
    awful.button({}, 3, function()
        os.execute(string.format("%s set %s toggle", theme.volume.cmd, theme.volume.togglechannel or theme.volume.channel))
        theme.volume.update()
    end),
    awful.button({}, 4, function()
        os.execute(string.format("%s set %s 2%%+", theme.volume.cmd, theme.volume.channel))
        theme.volume.update()
    end),
    awful.button({}, 5, function()
        os.execute(string.format("%s set %s 2%%-", theme.volume.cmd, theme.volume.channel))
        theme.volume.update()
    end)
))
local volumebg = wibox.container.background(theme.volume.bar, black2, gears.shape.rectangle)
local volumewidget = wibox.container.margin(volumebg, 7, 7, 9, 9)
--- }}}
--- {{{ Battery
local baticon = wibox.widget.imagebox(theme.widget_battery)
local bat = lain.widget.bat({
    settings = function()
      if bat_now.status and bat_now.status ~= "N/A" then
        if bat_now.ac_status == 1 then
          baticon:set_image(theme.widget_ac)
        elseif not bat_now.perc and tonumber(bat_now.perc) <= 15 then
          baticon:set_image(theme.widget_battery_empty)
        elseif not bat_now.perc and tonumber(bat_now.perc) <= 50 then
          baticon:set_image(theme.widget_battery_low)
        elseif not bat_now.perc and tonumber(bat_now.perc) <= 85 then
          baticon:set_image(theme.widget_battery_mid)
        else
          baticon:set_image(theme.widget_battery)
        end
        widget:set_markup(markup.font(theme.font, " " .. bat_now.perc .. "% "))
      else
        baticon:set_image(theme.widget_ac)
        widget:set_markup(markup.font(theme.font, " AC "))
      end
    end
})
--- }}}
theme.membar = wibox.widget {
  {
    max_value = 100,
    forced_height = 20,
    forced_width = 40,
    paddings = 1,
    border_width = 1,
    border_color = white2,
    color = white1,
    background_color = black2,
    widget = wibox.widget.progressbar,
  },
  layout = wibox.layout.rotate
}

-- RAM usage tooltip
memwidget_t = awful.tooltip({ objects = { theme.membar },})

vicious.cache(vicious.widgets.mem)
vicious.register(theme.membar, vicious.widgets.mem,
                 function (widget, args)
                   local p = io.popen("ps -eo user,pid,rss,args --sort=-rss | head -11 | awk '{hr[1024*1024]=\"GB\"; hr[1024]=\"MB\"; h=0; for (x=1024*1024*1024; x>=1024; x/=1024) { if ($3>=x) { h=x; break } } { proc=\"\"; for(i = 4; i <= NF; i++) proc = proc $i \" \" } { if(NR != 1) { printf (\"\\n%-10s %-6s %-6.2f%s %-30s\", $1, $2, $3/h, hr[h], substr(proc, 0, 30)) } else { printf (\"\\n%-10s %-6s %s %-30s\", $1, $2, \"SIZE    \", substr(proc, 0, 30)) }}}'")
                   local o = p:read("*all")
                   p:close()
                   memwidget_t:set_text(string.format("RAM: %sMB / %sMB%s", args[2], args[3], o))
                   widget.widget:set_value(args[1])
                   return args[1]
                 end, 6)
local membg = wibox.container.background(theme.membar, black2, gears.shape.rectangle)
local memwidget = wibox.container.margin(membg, 7, 7, 5, 5)

--- }}}

-- {{{ Wibar
-- Create a textclock widget
local layoutlist = { lain.layout.centerwork, awful.layout.suit.tile, awful.layout.suit.tile,
                     awful.layout.suit.tile, awful.layout.suit.tile, awful.layout.suit.floating,
                     awful.layout.suit.tile, awful.layout.suit.tile, awful.layout.suit.tile }

function theme.at_screen_connect(s)
  -- Quake application
  s.quake = lain.util.quake({ app = awful.util.terminal })

  -- Wallpaper
  local wallpaper = theme.wallpaper
  if type(wallpaper) == "function" then
    wallpaper = wallpaper(s)
  end
  gears.wallpaper.maximized(wallpaper, s, true)

  -- Each screen has its own tag table.
  awful.tag(awful.util.tagnames, s, layoutlist)

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()
  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
                          awful.button({ }, 1, function () awful.layout.inc( 1) end),
                          awful.button({ }, 3, function () awful.layout.inc(-1) end),
                          awful.button({ }, 4, function () awful.layout.inc( 1) end),
                          awful.button({ }, 5, function () awful.layout.inc(-1) end)))
  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", height = 28, screen = s, bg = theme.bg_normal, fg = theme.fg_normal })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      -- mylauncher,
      s.mytaglist,
      -- s.mypromptbox,
    },
    s.mytasklist, -- Middle widget
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      -- mykeyboardlayout,
      baticon,
      bat,
      volumewidget,
      memwidget,
      cpuwidget,
      wibox.widget.systray(),
      mytextclock,
      s.mylayoutbox,
    },
  }
end
-- }}}

return theme
