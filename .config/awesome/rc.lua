-------------------------------------------
--        "Zenburn" awesome theme        --
--        By Wang-Zhou Dai (haldai)      --
-------------------------------------------
-- {{{ Required libraries
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local lain = require("lain")
local menubar = require("menubar")
local scratch = require("scratch")
local hotkeys_popup = require("awful.hotkeys_popup").widget
require("awful.hotkeys_popup.keys")
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
                   title = "Oops, there were errors during startup!",
                   text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal(
    "debug::error", function (err)
      -- Make sure we don't go into an endless error loop
      if in_error then return end
      in_error = true
      naughty.notify({ preset = naughty.config.presets.critical,
                       title = "Oops, an error happened!",
                       text = tostring(err) })
      in_error = false
  end)
end
-- }}}

-- {{{ Function definitions
-- Return the tag index of focust client
local function tag_num(c)
  local tag_idx = {}
  for k, v in pairs(c.screen.tags) do
    tag_idx[v] = k
  end
  local t = c and c.first_tag or nil
  return tag_idx[t]
end
-- Notify volume control
local function notify_vol(n)
  local status = io.popen("amixer get Master"):read("*all")
  local vol = string.match(status, "(%d?%d?%d)%%")
  local stat = string.match(status, "%[(o[nf]*)%]")
  if n == 1 then -- up
    naughty.notify({ text = "音量增加：" .. vol .. "%", timeout = 0.5 })
  elseif n == -1 then -- down
    naughty.notify({ text = "音量減少：" .. vol .. "%", timeout = 0.5 })
  elseif n == 0 then -- toggle
    if stat == "on" then
      naughty.notify({ text = "音量：開", timeout = 0.5 })
    else
      naughty.notify({ text = "音量：關", timeout = 0.5 })
    end
  end
end

local function notify_bright(n)
  local status = io.popen("xbacklight -get"):read("*all")
  local bri = tonumber(status)
  if n == 1 then -- up
    naughty.notify({ text = "亮度增加：" .. bri .. "%", timeout = 0.5 })
  elseif n == -1 then -- down
    naughty.notify({ text = "亮度增加：" .. bri .. "%", timeout = 0.5 })
  end
end
-- }}}

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = "Mod4"
local altkey = "Mod1"
local terminal = "st"
local editor = "emacsclient -c -a emacs"
local editor_cmd = terminal .. " -e " .. editor
local browser = "chromium"
local filemanager = "pcmanfm"

-- {{{ taglist and tasklist buttons
awful.util.taglist_buttons = my_table.join(
  awful.button({ }, 1, function(t) t:view_only() end),
  awful.button({ modkey }, 1, function(t)
      if client.focus then
        client.focus:move_to_tag(t)
      end
  end),
  awful.button({ }, 3, awful.tag.viewtoggle),
  awful.button({ modkey }, 3, function(t)
      if client.focus then
        client.focus:toggle_tag(t)
      end
  end),
  awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
  awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end)
)

awful.util.tasklist_buttons = my_table.join(
  awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        c:emit_signal(
          "request::activate",
          "tasklist",
          {raise = true}
        )
      end
  end),
  awful.button({ }, 3, function()
      awful.menu.client_list({ theme = { width = 250 } })
  end),
  awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
  end),
  awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
end))
--- }}}

-- {{{ lain layout setting
lain.layout.termfair.nmaster           = 3
lain.layout.termfair.ncol              = 1
lain.layout.termfair.center.nmaster    = 3
lain.layout.termfair.center.ncol       = 1
lain.layout.cascade.tile.offset_x      = 2
lain.layout.cascade.tile.offset_y      = 32
lain.layout.cascade.tile.extra_padding = 5
lain.layout.cascade.tile.nmaster       = 5
lain.layout.cascade.tile.ncol          = 2
--- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(awful.util.getdir("config") .. "/themes/zenburn/theme.lua")

-- Table of layouts to cover with awful.layout.inc, order matters.
-- awful.util.tagnames = { "", "", "", "", "", "", "", "", "" }
awful.util.tagnames = { "壹", "貳", "叄", "肆", "伍", "陸", "柒", "捌", "玖" }
-- awful.util.tagnames = {"1", "2", "3", "4", "5", "6", "7", "8", "9"}
awful.layout.layouts = {
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    lain.layout.centerwork,
    -- lain.layout.termfair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
    awful.layout.suit.floating,
}
-- }}}

-- {{{ Menu
-- Helper functions
local function client_menu_toggle_fn()
  local instance = nil
  return function ()
    if instance and instance.wibox.visible then
      instance:hide()
      instance = nil
    else
      instance = awful.menu.clients({ theme = { width = 250 } })
    end
  end
end

-- Icon path
local icon_path = "/usr/share/icons/Paper/24x24@2x/"

-- {{{ Menu
local myawesomemenu = {
  { " 熱鍵", function() return false, hotkeys_popup.show_help end, icon_path .. "devices/keyboard.png" },
  { " 設定", string.format("%s -e %s %s", terminal, editor, awesome.conffile), icon_path .. "categories/system-settings.png" },
  { " 重載", awesome.restart, icon_path .. "actions/system-reboot.png" },
  { " 退出", function() awesome.quit() end, icon_path .. "actions/system-log-out.png" }
}

local myexitmenu = {
  { " 登出", function() awesome.quit() end, icon_path .. "actions/system-log-out.png" },
  { " 暫停", "systemctl suspend", icon_path .. "actions/system-suspend.png" },
  { " 休眠", "systemctl hibernate", icon_path .. "actions/system-hibernate.png" },
  { " 重啓", "systemctl reboot", icon_path .. "actions/system-reboot.png" },
  { " 關機", "poweroff", icon_path .. "actions/system-shutdown.png" }
}

local myxrandrmenu = {
  { " 複製", function () awful.util.spawn_with_shell("~/.scripts/presentation") end, icon_path .. "devices/system.png" },
  { " 左擴展", function () awful.util.spawn_with_shell("~/.scripts/presentation-x -l") end, icon_path .. "actions/previous.png" },
  { " 右擴展", function () awful.util.spawn_with_shell("~/.scripts/presentation-x -r") end, icon_path .. "actions/next.png" },
}

awful.util.mymainmenu = awful.menu({
    items = {
      { " 終端", function () awful.spawn(terminal) end, icon_path .. "apps/utilities-terminal.png" },
      { " 編輯", function () awful.spawn("emacsclient -c -a emacs") end, icon_path .. "apps/emacs.png" },
      { " 衝浪", function () awful.spawn("chromium") end, icon_path .. "apps/chromium.png" },
      { " 文件", function () awful.spawn("pcmanfm") end, icon_path .. "apps/file-manager.png" },
      { " 監控", function () awful.spawn("st -e htop") end, icon_path .. "apps/utilities-system-monitor.png" },
      { " 窗口", myawesomemenu, beautiful.awesome_icon },
      { " 演示", myxrandrmenu, icon_path .. "devices/system.png" },
      { " 退出", myexitmenu, icon_path .. "actions/system-shutdown.png" } }
})

-- hide menu when mouse leaves it
--awful.util.mymainmenu.wibox:connect_signal("mouse::leave", function() awful.util.mymainmenu:hide() end)

--menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal(
  "property::geometry", function(s)
    -- Wallpaper
    if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
    end
end)
-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s) beautiful.at_screen_connect(s) end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
               awful.button({ }, 3, function () awful.util.mymainmenu:toggle() end),
               awful.button({ }, 4, awful.tag.viewprev),
               awful.button({ }, 5, awful.tag.viewnext)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
  --- Workspace Navigation
  awful.key({ modkey }, "s", hotkeys_popup.show_help,
    {description="show help", group="awesome"}),
  awful.key({ modkey }, "Left", awful.tag.viewprev,
    {description = "view previous", group = "tag"}),
  awful.key({ modkey }, "Right", awful.tag.viewnext,
    {description = "view next", group = "tag"}),
  awful.key({ modkey }, "k", awful.tag.history.restore,
    {description = "go back", group = "tag"}),
  awful.key({ modkey }, "p", function () awful.client.focus.bydirection("up") end,
    {description = "focus up", group = "client"}),
  awful.key({ modkey }, "n", function () awful.client.focus.bydirection("down") end,
    {description = "focus down", group = "client"}),
  awful.key({ modkey }, "b", function () awful.client.focus.bydirection("left") end,
    {description = "focus left", group = "client"}),
  awful.key({ modkey }, "f", function () awful.client.focus.bydirection("right") end,
    {description = "focus right", group = "client"}),
  awful.key({ modkey, "Ctrl" }, "p", function ()
      awful.screen.focus_bydirection("up", awful.screen.focused()) end,
    {description = "focus screen up", group = "client"}),
  awful.key({ modkey, "Ctrl" }, "n", function ()
      awful.screen.focus_bydirection("down", awful.screen.focused()) end,
    {description = "focus screen down", group = "client"}),
  awful.key({ modkey, "Ctrl" }, "b", function ()
      awful.screen.focus_bydirection("left", awful.screen.focused()) end,
    {description = "focus screen left", group = "client"}),
  awful.key({ modkey, "Ctrl" }, "f", function ()
      awful.screen.focus_bydirection("right", awful.screen.focused()) end,
    {description = "focus screen right", group = "client"}),

  -- Layout manipulation
  awful.key({ modkey, "Shift" }, "p", function () awful.client.swap.bydirection("up") end,
    {description = "swap with upward client", group = "client"}),
  awful.key({ modkey, "Shift" }, "n", function () awful.client.swap.bydirection("down") end,
    {description = "swap with downward client", group = "client"}),
  awful.key({ modkey, "Shift" }, "b", function () awful.client.swap.bydirection("left") end,
    {description = "swap with leftward client", group = "client"}),
  awful.key({ modkey, "Shift" }, "f", function () awful.client.swap.bydirection("right") end,
    {description = "swap with rightward client", group = "client"}),
  awful.key({ modkey }, "u", awful.client.urgent.jumpto,
    {description = "jump to urgent client", group = "client"}),
  awful.key({ modkey }, "Tab",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "go back", group = "client"}),
  awful.key({ modkey }, "l", function () awful.layout.inc(1) end,
    {description = "select next", group = "layout"}),
  awful.key({ modkey, "Control" }, "l", function () awful.layout.inc(-1) end,
    {description = "select previous", group = "layout"}),

   -- ALSA volume control
  awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer sset Master 1%+", false) notify_vol(1) end),
  awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer sset Master 1%-", false) notify_vol(-1) end),
  awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer sset Master toggle", false) notify_vol(0) end),

  -- Brightness
  awful.key({ }, "XF86MonBrightnessUp", function () os.execute("xbacklight -inc 10") end),
  awful.key({ }, "XF86MonBrightnessDown", function () os.execute("xbacklight -dec 10") end),
  awful.key({ modkey }, "F12", function () os.execute("xbacklight -inc 10") notify_bright(1) end),
  awful.key({ modkey }, "F11", function () os.execute("xbacklight -dec 10") notify_bright(-1) end),

  -- On the fly useless gaps change
  awful.key({ altkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end,
    {description = "increment useless gaps", group = "tag"}),
  awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end,
    {description = "decrement useless gaps", group = "tag"}),

  -- Standard programs
  awful.key({ modkey }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey, "Shift" }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),
  awful.key({ modkey, "Control" }, "Esc", awesome.quit,
    {description = "quit awesome", group = "awesome"}),

  -- Rofi
  awful.key({ modkey }, "d",  function () awful.spawn("/usr/bin/rofi -show") end,
    {description = "launch rofi", group = "launcher"}),
  awful.key({ modkey }, "0",  function () awful.util.spawn_with_shell("~/.scripts/myexit") end,
    {description = "lauch exit menu", group = "launcher"}),

  -- Scratchpad
  awful.key({ modkey }, "-", function ()
      scratch.drop(terminal, "center", "center", 720, 400) end,
    {description = "display a scratchpad of terminal", group = "launcher"}),

  -- Applications
  awful.key({ modkey }, "F2", function () awful.spawn("emacsclient -c -a emacs") end,
    {description = "launch Emacs", group = "launcher"}),
  awful.key({ modkey }, "F3", function () awful.spawn("chromium") end,
    {description = "launch chrome", group = "launcher"}),
  awful.key({ modkey, "Shift" }, "F3", function () awful.spawn("chromium --disable-web-security --user-data-dir") end,
    {description = "launch Chrome with user data dir", group = "launcher"}),
  awful.key({ modkey }, "F4", function () awful.spawn("thunderbird") end,
    {description = "launch thunderbird", group = "launcher"}),
  awful.key({ modkey }, "F5", function () awful.spawn("st -e ranger") end,
    {description = "launch ranger", group = "launcher"}),
  awful.key({ modkey }, "F6", function () awful.spawn("electronic-wechat") end,
    {description = "launch wechat", group = "launcher"}),
  awful.key({ modkey, "Ctrl" }, "t", function () awful.spawn("pkill compton") end,
    {description = "kill compton", group = "launcher"}),
  awful.key({ modkey }, "t",  function () awful.spawn("compton --config /home/daiwz/.config/compton.conf") end,
    {description = "launch compton", group = "launcher"}),
  awful.key({ }, "Print", function () awful.spawn("deepin-screenshot -f") end,
    {description = "print full screen", group = "launcher"}),
  awful.key({ modkey }, "Print", function () awful.spawn("deepin-screenshot") end,
    {description = "launch deepin-screenshot", group = "launcher"}),
  awful.key({ modkey }, "c", function () awful.util.spawn_with_shell("~/.scripts/myclip.sh") end,
    {description = "show clipboard", group = "launcher"}),
  awful.key({ modkey, "Ctrl" }, "c", function () awful.spawn('greenclip clear') end,
    {description = "clear clipboard", group = "launcher"}),

  -- Custom scripts
  awful.key({ modkey }, "Escape", function ()
      awful.util.spawn_with_shell("~/.scripts/flash-win.sh") end,
    {description = "flash current screen", group = "launcher"}),
  awful.key({ modkey }, "F8", function ()
      awful.util.spawn_with_shell("~/.scripts/presentation") end,
    {description = "External screen copy", group = "launcher"}),
  awful.key({ modkey, "Ctrl" }, "F8", function ()
      awful.util.spawn_with_shell("~/.scripts/presentation-x -r") end,
    {description = "External screen on the right", group = "launcher"}),
  awful.key({ modkey, "Shift" }, "F8", function ()
      awful.util.spawn_with_shell("~/.scripts/presentation-x -l") end,
    {description = "External screen on the left", group = "launcher"})
)

clientkeys = gears.table.join(
  awful.key({ modkey, "Shift" }, "m", lain.util.magnify_client,
    {description = "magnify client", group = "client"}),
  awful.key({ modkey }, "j",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift" }, "q", function (c) c:kill() end,
      {description = "close", group = "client"}),
    awful.key({ modkey, "Shift" }, "space",  awful.client.floating.toggle,
      {description = "toggle floating", group = "client"}),
    awful.key({ modkey }, "o", function (c) c:move_to_screen() end,
      {description = "move to screen", group = "client"}),
    awful.key({ modkey }, "F1", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
    awful.key({ modkey }, "z",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey }, "m",
      function (c)
        c.maximized = not c.maximized
        c:raise()
      end,
      {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = gears.table.join(
    globalkeys,
    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
      {description = "view tag #"..i, group = "tag"}),
    -- Move client to tag.
    awful.key({ modkey, "Control" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      {description = "move focused client to tag #"..i, group = "tag"}),
    -- Move client to screen on the same tag
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function ()
        if client.focus
          and 0 < i
          and i <= screen.count()
        then
          local cnt = 0
          for s in screen do
            cnt = cnt + 1
            if cnt == i and s ~= client.focus.screen then
              client.focus:move_to_tag(s.tags[tag_num(client.focus)])
              break
            end
          end
        end
      end,
      {description = "move focused client to screen #" .. i .. " in the same tag", group = "tag"})
  )
end

clientbuttons = gears.table.join(
  awful.button({ }, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
  end),
  awful.button({ modkey }, 1, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
      awful.mouse.client.move(c)
  end),
  awful.button({ modkey }, 3, function (c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
      awful.mouse.client.resize(c)
  end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  { rule = { },
    properties = { border_width = beautiful.border_width,
                   border_color = beautiful.border_normal,
                   focus = awful.client.focus.filter,
                   raise = true,
                   keys = clientkeys,
                   buttons = clientbuttons,
                   screen = awful.screen.preferred,
                   size_hints_honor = false, -- gaps
                   placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  },

  -- Floating clients.
  { rule_any = {
      instance = {
        "DTA",  -- Firefox addon DownThemAll.
        "copyq",  -- Includes session name in class.
        "pinentry",
      },
      class = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "MessageWin",  -- kalarm.
        "Sxiv",
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
        "Wpa_gui",
        "veromix",
        "xtightvncviewer"},

      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name = {
        "Event Tester",  -- xev.
      },
      role = {
        "AlarmWindow",  -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
        "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
      }
  }, properties = { floating = true }},

  -- Add titlebars to normal clients and dialogs
  { rule_any = {type = { "normal", "dialog" }
               }, properties = { titlebars_enabled = false }
  },

  -- Set Firefox to always map on the tag named "2" on screen 1.
  -- { rule = { class = "Firefox" },
  --   properties = { screen = 1, tag = "2" } },
  { rule = {class = "Emacs"},
    properties = { tag = awful.util.tagnames[2] } },
  { rule = {class = "Chromium"},
    properties = { tag = awful.util.tagnames[3], maximized = true } },
  { rule = {class = "Thunderbird"},
    properties = { tag = awful.util.tagnames[4] } },
  { rule = {class = "shadowsocks-qt5"},
    properties = { tag = awful.util.tagnames[4], minimized = true } },
  { rule = {class = "Pcmanfm"},
    properties = { tag = awful.util.tagnames[5] } },
  { rule_any = {class = { "electronic-wechat", "qq.exe" } },
    properties = { tag = awful.util.tagnames[6] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
  "manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
  "request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
      awful.button({ }, 1, function()
          c:emit_signal("request::activate", "titlebar", {raise = true})
          awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
          c:emit_signal("request::activate", "titlebar", {raise = true})
          awful.mouse.client.resize(c)
      end)
    )

    awful.titlebar(c) : setup {
      { -- Left
        awful.titlebar.widget.iconwidget(c),
        buttons = buttons,
        layout  = wibox.layout.fixed.horizontal
      },
      { -- Middle
        { -- Title
          align  = "center",
          widget = awful.titlebar.widget.titlewidget(c)
        },
        buttons = buttons,
        layout  = wibox.layout.flex.horizontal
      },
      { -- Right
        awful.titlebar.widget.floatingbutton (c),
        awful.titlebar.widget.maximizedbutton(c),
        awful.titlebar.widget.stickybutton   (c),
        awful.titlebar.widget.ontopbutton    (c),
        awful.titlebar.widget.closebutton    (c),
        layout = wibox.layout.fixed.horizontal()
      },
      layout = wibox.layout.align.horizontal
                              }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
  "mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

-- No border for maximized clients
function border_adjust(c)
  if c.maximized then -- no borders if only 1 client visible
    c.border_width = 0
  elseif #awful.screen.focused().clients > 1 then
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_focus
  end
end

client.connect_signal("property::maximized", border_adjust)
client.connect_signal("focus", border_adjust)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

awful.util.spawn_with_shell("~/.config/awesome/autostart.sh")
