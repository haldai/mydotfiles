#!/usr/bin/env bash
set -euo pipefail

CCTK="/usr/bin/cctk"
BAT="BAT0"

if [[ $EUID -ne 0 ]]; then
  echo "请用 sudo 运行"
  exit 1
fi

if [[ ! -x "$CCTK" ]]; then
    echo "找不到 cctk: $CCTK"
  exit 1
fi

CAP_FILE="/sys/class/power_supply/$BAT/capacity"
if [[ ! -r "$CAP_FILE" ]]; then
  echo "找不到电池容量文件: $CAP_FILE，请安装。"
  exit 1
fi

capacity=$(<"$CAP_FILE")

# 读取当前配置，例如：
# PrimaryBattChargeCfg=Custom:50-92
cfg=$("$CCTK" --PrimaryBattChargeCfg | tail -n1)

if [[ ! "$cfg" =~ Custom:([0-9]+)-([0-9]+) ]]; then
  echo "当前不是 Custom 模式，无法设置 chargeonce"
  echo "当前配置: $cfg"
  exit 1
fi

orig_start="${BASH_REMATCH[1]}"
orig_stop="${BASH_REMATCH[2]}"

echo "当前电量: ${capacity}%"
echo "原始阈值: ${orig_start}-${orig_stop}"

if (( capacity >= orig_stop )); then
  echo "当前电量已达到或超过 stop 阈值，无需操作"
  exit 0
fi

# 为了触发充电，需要 start > 当前电量
temp_start=$((capacity + 1))

# Dell 要求 stop - start >= 5
max_allowed_start=$((orig_stop - 5))

if (( temp_start > max_allowed_start )); then
  echo "无法设置 chargeonce："
  echo "当前电量 ${capacity}% 距离 stop=${orig_stop}% 太近。"
  echo "Dell 要求 stop-start 至少相差 5%。"
  exit 2
fi

echo "临时设置阈值: ${temp_start}-${orig_stop}"
"$CCTK" --PrimaryBattChargeCfg="Custom:${temp_start}-${orig_stop}"

restore() {
  echo
  echo "恢复原始阈值: ${orig_start}-${orig_stop}"
  "$CCTK" --PrimaryBattChargeCfg="Custom:${orig_start}-${orig_stop}" || true
}
trap restore EXIT INT TERM

echo "等待充电至 ${orig_stop}% ..."
while true; do
  capacity=$(<"$CAP_FILE")
  printf "\r当前电量: %s%%" "$capacity"

  if (( capacity >= orig_stop )); then
    echo
    echo "已达到目标电量"
    break
  fi

  sleep 60
done
