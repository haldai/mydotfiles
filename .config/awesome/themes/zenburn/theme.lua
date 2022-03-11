-------------------------------------------
--        "Zenburn" awesome theme        --
--        By Wang-Zhou Dai (haldai)      --
-- Based on Adrian C. (anrxc)'s settings --
-------------------------------------------
local themes_path = require("gears.filesystem").get_themes_dir()
local home_path = os.getenv("HOME")
local script_path = home_path .. "/.scripts/"
local dpi = require("beautiful.xresources").apply_dpi
local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")

local os = os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

-- {{{ Main
local theme = {
   wallpapers = { os.getenv("HOME") .. "/.config/awesome/themes/zenburn/pw.jpg",
                  os.getenv("HOME") .. "/.config/awesome/themes/zenburn/DD.jpg"},
   wallpaper_scales = {0.25, 1.5}
}
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
theme.fg_urgent  = yellow1
theme.bg_normal  = black2
theme.bg_focus   = black1
theme.bg_urgent  = red1
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.useless_gap   = dpi(16)
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
theme.tooltip_shape = nil
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
theme.menu_submenu_icon = themes_path .. "default/submenu.png"
theme.menu_height = dpi(28)
theme.menu_width  = dpi(98)
theme.menu_border_color = white1
theme.menu_border_width = 2
theme.menu_font = "方正宋刻本秀楷 Bold 13"
-- }}}

-- {{{ Icons
theme.lain_icons = os.getenv("HOME") .. "/.config/awesome/lain/icons/layout/zenburn/"
-- {{{ Taglist
theme.taglist_font = "方正宋刻本秀楷 Bold 16"
theme.taglist_bg_focus = black1
theme.taglist_fg_focus = orange
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon = themes_path .. "zenburn/awesome-icon.png"
theme.tasklist_font = "方正宋刻本秀楷 10"
theme.tasklist_font_focus = "方正宋刻本秀楷 Bold 10"
theme.tasklist_bg_focus = orange
theme.tasklist_fg_focus = white1
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
-- }}}
-- }}}

awful.util.tagnames = { "壹", "貳", "叄", "肆", "伍", "陸", "柒", "捌", "玖" }
-- awful.util.tagnames = { "☰", "☱", "☲", "☳", "☴", "☵", "☶", "☷", "☯" }
-- awful.util.tagnames = { "樞", "璇", "璣", "權", "衡", "陽", "光", "輔", "弻" }

local markup     = lain.util.markup
local separators = lain.util.separators

--- {{{ Widgets
local mytextclock = wibox.widget.textclock(" %a %m/%d %H:%M ")
mytextclock.font = "方正宋刻本秀楷 Bold 12"

-- {{{ CPU load
theme.cpugraph = wibox.widget {
   forced_width = 32,
   paddings = 1,
   border_width = 1,
   border_color = white2,
   color = white1,
   background_color = black1,
   widget = wibox.widget.graph
}
cpuwidget_t = awful.tooltip({ objects = { theme.cpugraph },})
awful.widget.watch('bash -c "mpstat -P ALL 2 1 | awk \'$12 ~ /[0-9.]+/ { print 100 - $12 }\'"', 5,
                   function(widget, stdout)
                      if stdout ~= nil and stdout ~= "" then
                         args = {}
                         for val in stdout:gmatch("[^\r\n]+") do
                            table.insert(args, tonumber(val))
                         end
                         theme.cpugraph:add_value(args[1] / 100, 1)
                         cpuwidget_t:set_text(string.format("CPU使用率: \n%.2f%%\n核心使用率: \n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%,\n%.2f%%, %.2f%%, %.2f%%, %.2f%%.",
                                                            args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9],
                                                            args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17],
                                                            args[18], args[19], args[20], args[21], args[22], args[23], args[24], args[25],
                                                            args[26], args[27], args[28], args[29], args[30], args[31], args[32], args[33]))
                      else
                         theme.cpugraph:add_value(0, 1)
                         cpuwidget_t:set_text(string.format("CPU使用率: 不明"))
                      end
end)

local cpubg = wibox.container.background(theme.cpugraph, black2, gears.shape.rectangle)
local cpuwidget = wibox.container.margin(cpubg, 5, 8, 5, 5)
--- }}}

--- {{{ Volume bar, add borders to lain.widget.alsabar
local myalsabar = require("myalsabar")
theme.volume = myalsabar {
   ticks = false,
   width = 64,
   height = 16,
   paddings = 1,
   border_width = 1,
   border_color = white2,
   timeout = 11, -- time interval
   colors = {
      background = black1,
      mute = red1,
      unmute = white1
   },
   notification_preset = { font = "方正宋刻本秀楷 10" }
}
theme.volume.bar:buttons(
   my_table.join (
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
local volumewidget = wibox.container.margin(volumebg, 5, 8, 7, 7)
--- }}}

--- {{{ GPU bar
local mygpubar = require("mygpubar")
theme.gputhermal = mygpubar {
   ticks = false,
   width = 10,
   height = 5,
   paddings = 1,
   border_width = 1,
   border_color = white1,
   timeout = 7, -- time interval
   colors = {
      background = black1,
      low = blue1,
      mid = white1,
      high = red1
   }
}

local gputhermalbg = wibox.container.background(theme.gputhermal.bar, black2, gears.shape.rectangle)
local gputhermalwidget = wibox.container.margin(gputhermalbg, -2, 8, 5, 5)
--- }}}

--- {{{ RAM
theme.membar = wibox.widget {
   {
      max_value = 100,
      color = white1,
      background_color = black1,
      paddings = 1,
      border_width = 1,
      border_color = white1,
      ticks = false,
      widget = wibox.widget.progressbar,
   },
   forced_height = 5,
   forced_width = 10,
   direction = 'east',
   layout = wibox.container.rotate,
}
memwidget_t = awful.tooltip({ objects = { theme.membar },})

awful.widget.watch('bash -c "free -h --si | sed -n 2p"', 13,
                   function (widget, stdout)
                      if stdout ~= nil and stdout ~= "" then
                         mtab = {}
                         for val, unit in string.gmatch(stdout, "([%d.]+)(%a)") do
                            table.insert(mtab, {tonumber(val), unit})
                         end
                         if mtab[2][2] == "M" and mtab[1][2] == "G" then
                            mem_usage = mtab[2][1] / 1024 / mtab[1][1] * 100
                         elseif mtab[2][2] == "G" and mtab[1][2] == "G" then
                            mem_usage = mtab[2][1] / mtab[1][1] * 100
                         end
                         memwidget_t:set_text(string.format("內存消耗:\n總計: %.1f%s\n已用: %.1f%s\n空閒: %.1f%s\n共享: %.1f%s\n緩衝 / 緩存: %.1f%s\n可用: %.1f%s", mtab[1][1], mtab[1][2], mtab[2][1], mtab[2][2], mtab[3][1], mtab[3][2], mtab[4][1], mtab[4][2], mtab[5][1], mtab[5][2], mtab[6][1], mtab[6][2]))
                         if mem_usage >= 80 then
                            theme.membar.widget:set_color(red1)
                         elseif mem_usage < 80 and mem_usage >= 70 then
                            theme.membar.widget:set_color(orange)
                         elseif mem_usage < 70 and mem_usage >= 45 then
                            theme.membar.widget:set_color(white1)
                         else
                            theme.membar.widget:set_color(blue1)
                         end
                         theme.membar.widget:set_value(mem_usage)
                      else
                         theme.membar.widget:set_color(white1)
                         theme.membar.widget:set_value(0)
                         memwidget_t:set_text(string.format("內存消耗: 不明"))
                      end
end)

local membg = wibox.container.background(theme.membar, black2, gears.shape.rectangle)
local memwidget = wibox.container.margin(membg, 5, 8, 5, 5)
--- }}}

-- {{{ Thermal
theme.thermalbar = wibox.widget {
   {
      max_value = 100,
      -- color = white1,
      background_color = black1,
      paddings = 1,
      border_width = 1,
      border_color = white1,
      ticks = false,
      widget = wibox.widget.progressbar,
   },
   forced_height = 5,
   forced_width = 10,
   direction = 'east',
   layout = wibox.container.rotate,
}
thermalwidget_t = awful.tooltip({ objects = { theme.thermalbar },})
awful.widget.watch('bash -c "sensors | grep Tctl"', 7,
                   function (widget, stdout)
                      if stdout ~= nil and stdout ~= "" then
                         temp = tonumber(string.match(stdout, '[%d.]+'))
                         thermalwidget_t:set_text(string.format("核心溫度: %.1f℃", temp))
                         if temp >= 90 then
                            theme.thermalbar.widget:set_color(red1)
                         elseif temp < 90 and temp >= 70 then
                            theme.thermalbar.widget:set_color(orange)
                         elseif temp < 70 and temp >= 45 then
                            theme.thermalbar.widget:set_color(white1)
                         else
                            theme.thermalbar.widget:set_color(blue1)
                         end
                         theme.thermalbar.widget:set_value(temp)
                      else
                         theme.thermalbar.widget:set_color(white1)
                         theme.thermalbar.widget:set_value(0)
                         thermalwidget_t:set_text(string.format("核心溫度: 不明"))
                      end
end)

local thermalbg = wibox.container.background(theme.thermalbar, black2, gears.shape.rectangle)
local thermalwidget = wibox.container.margin(thermalbg, 5, 8, 5, 5)
--- }}}

--- {{{ Pump
theme.pumpbar = wibox.widget {
   {
      -- color = white1,
      background_color = black1,
      paddings = 1,
      border_width = 1,
      border_color = white1,
      ticks = false,
      widget = wibox.widget.progressbar,
   },
   forced_height = 5,
   forced_width = 10,
   direction = 'east',
   layout = wibox.container.rotate,
}
pumpwidget_t = awful.tooltip({ objects = { theme.pumpbar },})
awful.widget.watch('bash -c "liquidctl status | tail -n 4 | grep -o \'[0-9.]\\+\'"', 61,
                   function (widget, stdout)
                      if stdout ~= nil and stdout ~= "" then
                         pumpargs = {}
                         for val in stdout:gmatch("[^\r\n]+") do
                            table.insert(pumpargs, tonumber(val))
                         end
                         pumpwidget_t:set_text(string.format("水溫: %.1f℃\n泵速: %d rpm / %d%%", pumpargs[1], pumpargs[2], pumpargs[3]))
                         if pumpargs[3] >= 90 then
                            theme.pumpbar.widget:set_color(orange)
                         elseif pumpargs[3] < 90 and pumpargs[3] >= 70 then
                            theme.pumpbar.widget:set_color(white1)
                         elseif pumpargs[3] < 70 and pumpargs[3] >= 10 then
                            theme.pumpbar.widget:set_color(blue1)
                         else
                            theme.pumpbar.widget:set_background_color(red1)
                            theme.pumpbar.widget:set_color(black1)
                            pumpwidget_t:set_text(string.format("泵速過低！！！"))
                            naughty.notify({ title = "警告",
                                             text = "泵速過低！！！",
                                             timeout = 0,
                                             position = "top_middle",
                                             fg = theme.fg_urgent,
                                             bg = theme.bg_urgent,
                                             border_width = theme.border_width,
                                             border_color = white1 })
                         end
                         theme.pumpbar.widget:set_value(pumpargs[3] / 100)
                      else
                         theme.pumpbar.widget:set_background_color(red1)
                         theme.pumpbar.widget:set_value(0)
                         pumpwidget_t:set_text(string.format("泵速: 不明"))
                      end
end)

local pumpbg = wibox.container.background(theme.pumpbar, black2, gears.shape.rectangle)
local pumpwidget = wibox.container.margin(pumpbg, 5, 8, 5, 5)
--- }}}

--- {{{ Calendar
local calendar = awful.widget.calendar_popup.year({
      font          = 'SauceCodePro Nerd Font Mono',
      spacing       = 2,
      week_numbers  = false,
      start_sunday  = false,
      long_weekdays = true,
      opacity       = 0.9,
      bg            = black3,
      margin        = 10,
      style_focus   = {
         fg_color   = yellow1,
         bg_color   = black0
      }
})
calendar:attach( mytextclock, "tr" )
--- }}}
--- {{{ Mail
theme.emailnum = wibox.widget {
   markup = "<b>未知</b>",
   align  = 'center',
   valign = 'center',
   widget = wibox.widget.textbox
}
emailnum_t = awful.tooltip({ objects = { theme.emailnum },})
local emailbg = wibox.widget {
   theme.emailnum,
   bg = black1,
   shape = gears.shape.rectangle,
   shape_border_width = 1,
   shape_border_color = white1,
   forced_width = 20,
   widget = wibox.container.background
}
awful.widget.watch(
   script_path .. "offlineimap-count.sh", 600,
   function(widget, stdout, stderr, exitreason, exitcode)
      local unread_emails_num = tonumber(stdout) or 0
      if (unread_emails_num > 0) then
         theme.emailnum:set_markup_silently("<span color=\"" .. white2 .. "\" font_desc=\"SauceCodePro Nerd Font Mono\"><b>" .. tostring(unread_emails_num) .. "</b></span>")
         emailbg:set_bg(red1)
         emailnum_t:set_text("郵件！右鍵！")
      elseif (unread_emails_num == 0) then
         theme.emailnum:set_markup_silently("<span color=\"" .. white1 .. "\" font_desc=\"SauceCodePro Nerd Font Mono 9\">無</span>")
         emailbg:set_bg(black1)
         emailnum_t:set_text("暫無新郵件")
      end
   end
)
theme.emailnum:buttons(
   my_table.join(
      awful.button({}, 3, function()
            awful.spawn("emacsclient -c -a emacs --eval \"(mu4e)\"")
            theme.emailnum:set_markup_silently("<span color=\"" .. white1 .. "\" font_desc=\"SauceCodePro Nerd Font Mono 9\">無</span>")
            emailbg:set_bg(black1)
            emailnum_t:set_text("暫無新郵件")
      end)
))
local emailwidget = wibox.container.margin(emailbg, 5, 8, 5, 5)
--- }}}

--- {{{ Net
theme.netgraph = wibox.widget {
   forced_width = 32,
   paddings = 1,
   border_width = 1,
   border_color = white2,
   stack_colors = { blue1, red1 },
   step_width = 2,
   background_color = black1,
   scale = true,
   stack = true,
   widget = wibox.widget.graph
}

netwidget_t = awful.tooltip({ objects = { theme.netgraph },})

awful.widget.watch('bash -c "~/.scripts/mynetstat"', 3,
                   function(widget, stdout)
                      net_args = {}
                      for line in stdout:gmatch("[^\r\n]+") do
                         table.insert(net_args, line)
                      end
                      if_name = net_args[1]
                      if_rx = tonumber(net_args[2])
                      if_tx = tonumber(net_args[3])
                      if if_name == "none" then
                         netwidget_t:set_text(string.format("網絡斷開！"))
                         return 0
                      else
                         if string.sub(if_name, 1, 1) == 'e' then
                            netwidget_t:set_text(string.format("有線網絡[%s]: \n%.2fkB/s ▲\n%.2fkB/s ▼", if_name, if_tx, if_rx))
                         else
                            netwidget_t:set_text(string.format("無線網絡[%s]: \n%.2fkB/s ▲\n%.2fkB/s ▼", if_name, if_tx, if_rx))
                         end
                         theme.netgraph:add_value(if_rx / 100, 1)
                         theme.netgraph:add_value(if_tx / 100, 2)
                      end
end)

local netbg = wibox.container.background(theme.netgraph, black2, gears.shape.rectangle)
local netwidget = wibox.container.margin(netbg, 5, 8, 5, 5)
--- }}}

--- }}}

-- {{{ Wibar
local mylayouts = {
   { awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal,
     awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal,
     awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal, awful.layout.suit.fair.horizontal},
   { lain.layout.centerwork, awful.layout.suit.tile, awful.layout.suit.tile,
     awful.layout.suit.tile, awful.layout.suit.tile, awful.layout.suit.floating,
     awful.layout.suit.tile, awful.layout.suit.tile, awful.layout.suit.tile }
}

function theme.at_screen_connect(s)
   -- Quake application
   s.quake = lain.util.quake({ app = awful.util.terminal })
   screen_index = s.index

   -- Wallpaper
   local wallpaper = theme.wallpapers[screen_index]
   if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
   end
   gears.wallpaper.centered(wallpaper, s, theme.bg_normal, theme.wallpaper_scales[screen_index])

   -- Each screen has its own tag table.
   awful.tag(awful.util.tagnames, s, mylayouts[screen_index])

   -- Create a promptbox for each screen
   s.mypromptbox = awful.widget.prompt()
   -- Create an imagebox widget which will contain an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   s.mylayoutbox = awful.widget.layoutbox(s)
   s.mylayoutbox:buttons(my_table.join(
                            awful.button({ }, 1, function () awful.layout.inc( 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(-1) end),
                            awful.button({ }, 4, function () awful.layout.inc( 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   -- Create a taglist widget
   -- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)
   s.mytaglist = awful.widget.taglist {
      screen = s,
      filter = awful.widget.taglist.filter.all,
      buttons = awful.util.taglist_buttons,
      style   = {
         shape = gears.shape.square,
         shape_border_width_focus = 0.5,
         shape_border_color_focus = orange,
         fg_occupied = orange,
         bg_urgent = black1,
         fg_urgent = red1,
         bg_focus = black1,
         fg_focus = orange,
      },
      layout   = {
         spacing = 7,
         spacing_widget = wibox.widget.textbox("<b>ꞏ</b>"),
         layout  = wibox.layout.fixed.horizontal
      },
   }

   -- Create a tasklist widget
   s.mytasklist = awful.widget.tasklist {
      screen = s,
      filter = awful.widget.tasklist.filter.currenttags,
      buttons = awful.util.tasklist_buttons,
      style = {
         spacing_widget = {
            {
               thickness = 1,
               color = white1,
            },
            valign = 'center',
            halign = 'center',
            widget = wibox.container.place,
         },
         spacing = nil,
         layout = wibox.layout.align.horizontal,
      },
      widget_template = {
         {
            wibox.widget.base.make_widget(),
            forced_height = 3,
            id = 'background_role',
            widget = wibox.container.background,
         },
         {
            {
               {
                  id = 'icon_role',
                  widget = wibox.widget.imagebox,
               },
               margins = 2,
               widget = wibox.container.margin
            },
            {
               {
                  id = 'text_role',
                  widget = wibox.widget.textbox,
               },
               margins = 2,
               widget = wibox.container.margin
            },
            layout = wibox.layout.align.horizontal,
         },
         layout = wibox.layout.align.vertical,
      },
   }
   -- Create the wibox
   s.mywibox = awful.wibar({ position = "top", height = 36, screen = s,
                             bg = "#00000000",
                             border_width = 1,
                             border_color = "#00000000",
                             fg = theme.fg_normal })

   -- Add widgets to the wibox
   s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      expand = outside,
      { -- Left widgets
         wibox.widget.textbox("<b>『</b>"),
         layout = wibox.layout.align.horizontal,
         s.mytaglist,
         wibox.widget.textbox("<b>』</b>"),
         -- s.mypromptbox,
      },
      s.mytasklist, -- Middle widget
      { -- Right widgets
         -- mykeyboardlayout,
         wibox.widget.textbox("<b>網</b>"),
         netwidget,
         wibox.widget.textbox("<b>郵</b>"),
         emailwidget,
         -- wibox.widget.textbox("<b>亮</b>"),
         -- brightnesswidget,
         wibox.widget.textbox("<b>聲</b>"),
         volumewidget,
         -- wibox.widget.textbox("<b>泵</b>"),
         -- pumpwidget,
         wibox.widget.textbox("<b>溫</b>"),
         thermalwidget,
         gputhermalwidget,
         -- wibox.widget.textbox("<b>電</b>"),
         -- batwidget,
         wibox.widget.textbox("<b>存</b>"),
         memwidget,
         wibox.widget.textbox("<b>核</b>"),
         cpuwidget,
         -- wibox.widget.systray(),
         mytextclock,
         s.mylayoutbox,
         layout = wibox.layout.fixed.horizontal,
      },
   }
end
-- }}}

return theme
