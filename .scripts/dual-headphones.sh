#!/bin/bash
# dual-headphones.sh - 在两个耳机之间切换输出
# INZONE Buds + Core Ultra HD Audio Headphones 同时输出

INZONE="alsa_output.usb-Sony_INZONE_Buds-00.analog-stereo"
BUILTIN="alsa_output.pci-0000_00_1f.3-platform-sof_sdw.HiFi__Headphones__sink"
COMBINED="combined_headphones"

usage() {
    echo "用法: $0 <dual|inzone|builtin|status>"
    echo "  dual    - 两个耳机同时输出"
    echo "  inzone  - 仅 INZONE Buds 输出"
    echo "  builtin  - 仅内置耳机输出"
    echo "  status  - 查看当前状态"
    exit 1
}

case "${1:-$2}" in
    dual)
        # 加载混音模块
        EXISTING=$(pactl list modules short | grep -c "module-combine-sink.*$COMBINED")
        if [ "$EXISTING" -eq 0 ]; then
            pactl load-module module-combine-sink \
                sink_name="$COMBINED" \
                slaves=$INZONE,$BUILTIN \
                sink_properties="device.description='Dual Headphones (INZONE + Built-in)'"
            echo "✅ 已创建混音节点"
        fi
        pactl set-default-sink "$COMBINED"
        echo "✅ 默认输出已切换到: Dual Headphones (两个耳机同时输出)"
        ;;
    inzone)
        # 卸载混音模块
        pactl unload-module "$COMBINED" 2>/dev/null
        pactl set-default-sink "$INZONE"
        echo "✅ 默认输出已切换到: INZONE Buds"
        ;;
    builtin)
        # 卸载混音模块
        pactl unload-module "$COMBINED" 2>/dev/null
        pactl set-default-sink "$BUILTIN"
        echo "✅ 默认输出已切换到: 内置耳机 (Core Ultra HD Audio)"
        ;;
    status)
        DEFAULT=$(pactl get-default-sink)
        echo "当前默认输出设备: $DEFAULT"
        echo ""
        echo "可用设备:"
        pactl list sinks short | while read id name desc fmt state; do
            echo "  [$id] $name - $state"
        done
        ;;
    *)
        usage
        ;;
esac
