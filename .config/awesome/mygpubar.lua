--[[

   Licensed under GNU General Public License v2
   * (c) 2013, Luca CPZ
   * (c) 2013, Rman

--]]

local helpers  = require("lain.helpers")
local awful    = require("awful")
local naughty  = require("naughty")
local wibox    = require("wibox")
local math     = math
local string   = string
local type     = type
local tonumber = tonumber

-- Nvidia GPU Thermal

local function factory(args)
   local gpubar = {
      colors = {
         background = "#000000",
         low = "#000000",
         mid = "#000000",
         high = "#000000"
      },
      _current_level = 0
   }

   local args       = args or {}
   local timeout    = args.timeout or 5
   local settings   = args.settings or function() end
   local width      = args.width or 63
   local height     = args.height or 1
   local margins    = args.margins or 1
   local paddings   = args.paddings or 1
   local ticks      = args.ticks or false
   local ticks_size = args.ticks_size or 7
   local border_width = args.border_width or 1
   local border_color = args.border_color or "#DCDCCC"

   gpubar.cmd                 = "nvidia-smi | grep %"
   gpubar.colors              = args.colors or gpubar.colors
   gpubar.followtag           = args.followtag or false

   local format_cmd = { awful.util.shell, "-c", gpubar.cmd }

   gpubar.bar = wibox.widget {
      wibox.widget {
         color = gpubar.colors.mid,
         background_color = gpubar.colors.background,
         paddings = paddings,
         border_width = border_width,
         border_color = border_color,
         ticks = ticks,
         ticks_size = ticks_size,
         widget = wibox.widget.progressbar
      },
      forced_height = height,
      forced_width = width,
      direction = 'east',
      layout = wibox.container.rotate,
   }

   gpubar.tooltip = awful.tooltip({ objects = { gpubar.bar } })

   function gpubar.update(callback)
      helpers.async(
         format_cmd, function(nvsmi)
            result = {}
            for x in string.gmatch(nvsmi, "(%d+)") do
               table.insert(result, x)
            end

            local fan = result[1]
            local temp = result[2]
            local pow = result[4]
            local pow_total = result[5]
            local mem = result[6]
            local mem_total = result[7]

            if not fan or not temp then return end

            if temp ~= gpubar._current_level then
               gpubar._current_level = tonumber(temp)
               gpubar.bar.widget:set_value(gpubar._current_level / 100)
               if gpubar._current_level <= 45 then
                  gpubar.bar.widget.color = gpubar.colors.low
               elseif gpubar._current_level > 45 and gpubar._current_level <= 70 then
                  gpubar.bar.widget.color = gpubar.colors.mid
               else
                  gpubar.bar.widget.color = gpubar.colors.high
               end

               gpubar.tooltip:set_text(string.format("顯卡: \n風扇: \t%s%%,\n溫度: \t%s℃,\n功率: \t%sW / %sW,\n顯存: \t%sMiB / %sMiB.", fan, temp, pow, pow_total, mem, mem_total))

               settings()

               if type(callback) == "function" then callback() end
            end
      end)
   end

   helpers.newtimer(string.format("gpubar-%s", gpubar.cmd), timeout, gpubar.update)

   return gpubar
end

return factory
