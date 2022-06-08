-------------------------------
--     brightness widget     --
--   Wang-Zhou Dai(haldai)   --
-------------------------------

local helpers  = require("lain.helpers")
local awful    = require("awful")
local naughty  = require("naughty")
local wibox    = require("wibox")
local math     = math
local string   = string
local type     = type
local tonumber = tonumber

local function factory(args)
  local brightnessbar = {
    _current_level = 0,
  }

  local args = args or {}
  local timeout = args.timeout or 5
  local settings = args.settings or function() end
  local width = args.width or 64
  local height = args.height or 16
  local margins = args.margins or 1
  local paddings = args.paddings or 1
  local ticks = args.ticks or false
  local ticks_size = args.ticks_size or 7
  local border_width = args.border_width or 1
  local border_color = args.border_color or "#DCDCCC"
  local color = args.color or "#DCDCCC"
  local background_color = args.background_color or "#3F3F3F"

  brightnessbar.cmd = args.cmd or "xbacklight"
  brightnessbar.followtag = args.followtag or false
  brightnessbar.notification_preset = args.notification_preset

  if not brightnessbar.notification_preset then
    brightnessbar.notification_preset = {}
    brightnessbar.notification_preset.font = "monospace 10"
  end

  local format_cmd = string.format("%s -get", brightnessbar.cmd)

  brightnessbar.bar = wibox.widget {
    color = color,
    background_color = background_color,
    max_value = 1,
    forced_width = width,
    forced_height = height,
    border_width  = border_width,
    border_color = border_color,
    paddings = paddings,
    margins = margins,
    ticks = ticks,
    ticks_size = ticks_size,
    widget = wibox.widget.progressbar,
  }

  brightnessbar.tooltip = awful.tooltip({ objects = { brightnessbar.bar } })

  function brightnessbar.update(callback)
    helpers.async_with_shell(
      format_cmd, function(backlight)
        local brt = string.match(backlight, "(%d+)")

        if not brt then return end

        if brt ~= brightnessbar._current_level then
          brightnessbar._current_level = tonumber(brt)
          brightnessbar.bar:set_value(brightnessbar._current_level / 100)
          brightnessbar.tooltip:set_text("亮度：".. brt .."%")

          brightness_now = {
            level  = brightnessbar._current_level,
          }

          settings()

          if type(callback) == "function" then callback() end
        end
    end)
  end

  function brightnessbar.notify()
    brightnessbar.update(function()
        local preset = brightnessbar.notification_preset

        preset.title = string.format("亮度：%s%%", brightnessbar._current_level)

        -- tot is the maximum number of ticks to display in the notification
        -- fallback: default horizontal wibox height
        local wib, tot = awful.screen.focused().mywibox, 20

        -- if we can grab mywibox, tot is defined as its height if
        -- horizontal, or width otherwise
        if wib then
          if wib.position == "left" or wib.position == "right" then
            tot = wib.width
          else
            tot = wib.height
          end
        end

        int = math.modf((brightnessbar._current_level / 100) * tot)
        preset.text = string.format("[%s%s]", string.rep("|", int),
                                    string.rep(" ", tot - int))

        if brightnessbar.followtag then preset.screen = awful.screen.focused() end

        if not brightnessbar.notification then
          brightnessbar.notification = naughty.notify {
            preset  = preset,
            destroy = function() brightnessbar.notification = nil end
          }
        else
          naughty.replace_text(brightnessbar.notification, preset.title, preset.text)
        end
    end)
  end

  helpers.newtimer(string.format("brighnessbar-%s", brightnessbar.cmd), timeout, brightnessbar.update)

  return brightnessbar

end

return factory
