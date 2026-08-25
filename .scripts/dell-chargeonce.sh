#!/usr/bin/env bash
set -euo pipefail

# Dell XPS 14 DA14260 上的“一次充到停止阈值”。
#
# 不再使用 Dell Command Configure (cctk)：它会进入独立的固件调用路径，
# 已在本机造成过整机无响应。这里仅使用内核 dell_laptop 驱动导出的
# power_supply 阈值接口，并尽量减少固件访问次数。

readonly BATTERY="BAT0"
readonly POWER_DIR="/sys/class/power_supply/${BATTERY}"
readonly CAPACITY_FILE="${POWER_DIR}/capacity"
readonly STATUS_FILE="${POWER_DIR}/status"
readonly START_FILE="${POWER_DIR}/charge_control_start_threshold"
readonly STOP_FILE="${POWER_DIR}/charge_control_end_threshold"
readonly TYPES_FILE="${POWER_DIR}/charge_types"

readonly STATE_DIR="/var/lib/dell-chargeonce"
readonly STATE_FILE="${STATE_DIR}/state"
readonly LOCK_FILE="/run/lock/dell-chargeonce.lock"

readonly EXPECTED_VENDOR="Dell Inc."
readonly EXPECTED_PRODUCT="XPS 14 DA14260"
readonly POLL_SECONDS=60
readonly MAX_SECONDS=$((12 * 60 * 60))

state_version=""
state_phase=""
state_battery=""
state_original_start=""
state_stop=""
state_temporary_start=""
state_created_at=""
state_bios_version=""

original_start=""
stop_threshold=""
temporary_start=""
capacity=""
battery_status=""
bios_version=""

usage() {
  cat <<'EOF'
用法：
  sudo dell-chargeonce.sh                  只读预览（默认，不修改阈值）
  sudo dell-chargeonce.sh --apply          前台循环执行一次充电
  sudo dell-chargeonce.sh --status         查看未完成操作的持久化状态
  sudo dell-chargeonce.sh --restore        只读预览待恢复的阈值
  sudo dell-chargeonce.sh --restore --apply
                                             显式恢复原始起充阈值

执行期间请保持这个进程运行。达到停止阈值、拔掉交流电、超时，或收到
INT/TERM/HUP 信号时，脚本会尝试恢复原始起充阈值。

如果机器在固件访问期间失去响应，信号处理函数也无法运行；重启后请先用
--status 查看，再用 --restore --apply 人工恢复。脚本不会在开机时自动写固件。
EOF
}

die() {
  printf '错误：%s\n' "$*" >&2
  exit 1
}

is_uint() {
  [[ ${1-} =~ ^[0-9]+$ ]]
}

read_one_line() {
  local path=$1
  local value

  IFS= read -r value <"$path" || [[ -n $value ]] || return 1
  printf '%s' "$value"
}

require_root() {
  (( EUID == 0 )) || die "请用 sudo 运行"
}

check_host() {
  local vendor product

  [[ -r /sys/class/dmi/id/sys_vendor ]] || die "无法读取 DMI 厂商信息"
  [[ -r /sys/class/dmi/id/product_name ]] || die "无法读取 DMI 产品信息"
  vendor=$(read_one_line /sys/class/dmi/id/sys_vendor)
  product=$(read_one_line /sys/class/dmi/id/product_name)

  [[ $vendor == "$EXPECTED_VENDOR" && $product == "$EXPECTED_PRODUCT" ]] ||
    die "拒绝在未验证的机型上运行：${vendor} ${product}"

  if [[ -r /sys/class/dmi/id/bios_version ]]; then
    bios_version=$(read_one_line /sys/class/dmi/id/bios_version)
  else
    bios_version="unknown"
  fi
}

check_interfaces() {
  local path

  for path in "$CAPACITY_FILE" "$STATUS_FILE" "$START_FILE" "$STOP_FILE" "$TYPES_FILE"; do
    [[ -r $path ]] || die "缺少或无法读取内核接口：$path"
  done
  [[ -w $START_FILE ]] || die "起充阈值接口不可写：$START_FILE"
  command -v flock >/dev/null 2>&1 || die "缺少 flock 命令"
}

ac_online() {
  local supply type online

  for supply in /sys/class/power_supply/*; do
    [[ -r $supply/type && -r $supply/online ]] || continue
    type=$(read_one_line "$supply/type") || continue
    [[ $type == Mains ]] || continue
    online=$(read_one_line "$supply/online") || continue
    [[ $online == 1 ]] && return 0
  done
  return 1
}

load_state() {
  local key value

  [[ -f $STATE_FILE ]] || return 1

  state_version=""
  state_phase=""
  state_battery=""
  state_original_start=""
  state_stop=""
  state_temporary_start=""
  state_created_at=""
  state_bios_version=""

  while IFS='=' read -r key value || [[ -n ${key}${value} ]]; do
    case $key in
      version) state_version=$value ;;
      phase) state_phase=$value ;;
      battery) state_battery=$value ;;
      original_start) state_original_start=$value ;;
      stop) state_stop=$value ;;
      temporary_start) state_temporary_start=$value ;;
      created_at) state_created_at=$value ;;
      bios_version) state_bios_version=$value ;;
    esac
  done <"$STATE_FILE"

  [[ $state_version == 1 ]] || die "状态文件版本无效：$STATE_FILE"
  [[ $state_phase == prepared || $state_phase == active ]] ||
    die "状态文件阶段无效：$STATE_FILE"
  [[ $state_battery == "$BATTERY" ]] || die "状态文件中的电池名称无效"
  is_uint "$state_original_start" || die "状态文件中的原始阈值无效"
  is_uint "$state_stop" || die "状态文件中的停止阈值无效"
  is_uint "$state_temporary_start" || die "状态文件中的临时阈值无效"
  (( state_original_start >= 50 && state_original_start <= 95 )) ||
    die "状态文件中的原始阈值超出支持范围"
  (( state_stop >= 55 && state_stop <= 100 )) ||
    die "状态文件中的停止阈值超出支持范围"
  (( state_stop - state_original_start >= 5 )) ||
    die "状态文件中的原始阈值间隔无效"
  (( state_temporary_start > state_original_start && state_temporary_start <= 95 )) ||
    die "状态文件中的临时起充阈值无效"
  (( state_stop - state_temporary_start >= 5 )) ||
    die "状态文件中的临时阈值间隔无效"
}

save_state() {
  local phase=$1
  local tmp

  install -d -m 0700 "$STATE_DIR"
  tmp=$(mktemp "$STATE_DIR/.state.XXXXXX")
  chmod 0600 "$tmp"

  if ! {
    printf 'version=1\n'
    printf 'phase=%s\n' "$phase"
    printf 'battery=%s\n' "$BATTERY"
    printf 'original_start=%s\n' "$original_start"
    printf 'stop=%s\n' "$stop_threshold"
    printf 'temporary_start=%s\n' "$temporary_start"
    printf 'created_at=%s\n' "$(date --iso-8601=seconds)"
    printf 'bios_version=%s\n' "$bios_version"
  } >"$tmp"; then
    rm -f -- "$tmp"
    die "无法写入恢复状态"
  fi

  sync "$tmp"
  mv -f -- "$tmp" "$STATE_FILE"
  sync "$STATE_DIR" 2>/dev/null || true
}

remove_state() {
  rm -f -- "$STATE_FILE"
  sync "$STATE_DIR" 2>/dev/null || true
}

show_saved_state() {
  if ! load_state; then
    echo "没有待恢复的 charge-once 状态。"
    return 0
  fi

  echo "发现未完成的 charge-once 状态："
  echo "  阶段：${state_phase}"
  echo "  电池：${state_battery}"
  echo "  原始起充阈值：${state_original_start}%"
  echo "  停止阈值：${state_stop}%"
  echo "  临时起充阈值：${state_temporary_start}%"
  echo "  创建时间：${state_created_at:-unknown}"
  echo "  当时 BIOS：${state_bios_version:-unknown}"
  echo "脚本不会自动恢复；确认后运行：sudo $0 --restore --apply"
}

collect_plan() {
  local charge_types max_start

  check_host
  check_interfaces
  [[ ! -e $STATE_FILE ]] ||
    die "存在未完成状态；请先运行 --status，必要时执行 --restore --apply"
  ac_online || die "未检测到已连接的交流电源"

  charge_types=$(read_one_line "$TYPES_FILE")
  [[ " $charge_types " == *" [Custom] "* ]] ||
    die "当前充电模式不是 Custom：$charge_types"

  capacity=$(read_one_line "$CAPACITY_FILE")
  battery_status=$(read_one_line "$STATUS_FILE")
  original_start=$(read_one_line "$START_FILE")
  stop_threshold=$(read_one_line "$STOP_FILE")

  is_uint "$capacity" || die "电池容量不是有效整数：$capacity"
  is_uint "$original_start" || die "起充阈值不是有效整数：$original_start"
  is_uint "$stop_threshold" || die "停止阈值不是有效整数：$stop_threshold"
  (( original_start >= 50 && original_start <= 95 )) ||
    die "起充阈值超出 Dell 支持范围：$original_start"
  (( stop_threshold >= 55 && stop_threshold <= 100 )) ||
    die "停止阈值超出 Dell 支持范围：$stop_threshold"
  (( stop_threshold - original_start >= 5 )) ||
    die "当前阈值不满足 Dell 的最小 5% 间隔"

  if (( capacity >= stop_threshold )); then
    echo "当前电量 ${capacity}% 已达到停止阈值 ${stop_threshold}%，无需操作。"
    return 2
  fi

  if [[ $battery_status == Charging ]]; then
    echo "电池已在充电；保持现有阈值即可充到 ${stop_threshold}%，无需修改。"
    return 2
  fi

  if (( capacity < original_start )); then
    die "当前电量低于起充阈值却未充电；为避免掩盖电源或固件异常，拒绝修改"
  fi

  temporary_start=$((capacity + 1))
  (( temporary_start >= 50 )) || temporary_start=50
  max_start=$((stop_threshold - 5))
  if (( temporary_start > max_start )); then
    die "当前电量 ${capacity}% 距停止阈值 ${stop_threshold}% 太近；Dell 要求至少相差 5%"
  fi
  (( temporary_start > original_start )) ||
    die "计算出的临时起充阈值没有高于原阈值，拒绝修改"
}

print_plan() {
  echo "机型：${EXPECTED_PRODUCT}"
  echo "BIOS：${bios_version}"
  echo "当前电量：${capacity}%（${battery_status}）"
  echo "原始阈值：${original_start}-${stop_threshold}"
  echo "临时阈值：${temporary_start}-${stop_threshold}"
  echo "轮询间隔：${POLL_SECONDS} 秒；最长运行：$((MAX_SECONDS / 3600)) 小时"
}

write_start_and_verify() {
  local wanted=$1
  local actual

  printf '%s\n' "$wanted" >"$START_FILE"
  actual=$(read_one_line "$START_FILE")
  [[ $actual == "$wanted" ]] || {
    printf '阈值写后校验失败：期望 %s，实际 %s\n' "$wanted" "$actual" >&2
    return 1
  }
}

restore_saved_state() {
  local current_stop

  load_state || {
    echo "没有待恢复状态。"
    return 0
  }

  check_host
  check_interfaces
  current_stop=$(read_one_line "$STOP_FILE")
  [[ $current_stop == "$state_stop" ]] || {
    printf '停止阈值已从 %s 变成 %s；为避免产生无效阈值，拒绝自动恢复。\n' \
      "$state_stop" "$current_stop" >&2
    return 1
  }

  echo "恢复原始起充阈值：${state_original_start}%（停止阈值保持 ${state_stop}%）"
  write_start_and_verify "$state_original_start" || return 1
  remove_state
  echo "原始阈值已恢复并校验。"
}

run_charge_once() {
  local started_at now elapsed current_capacity current_status
  local restore_needed=0
  local rc=0

  exec 9>"$LOCK_FILE"
  flock -n 9 || die "另一个 dell-chargeonce 进程正在运行"

  if collect_plan; then
    :
  else
    rc=$?
    (( rc == 2 )) && return 0
    return "$rc"
  fi
  print_plan

  echo "先持久化恢复状态，再写入临时起充阈值……"
  save_state prepared

  cleanup() {
    local cleanup_rc=$?
    trap - EXIT INT TERM HUP
    if (( restore_needed )); then
      echo
      if ! restore_saved_state; then
        echo "自动恢复失败；状态文件已保留。请重启后运行 --status。" >&2
        cleanup_rc=1
      fi
    else
      echo "临时阈值未确认生效；状态文件已保留，请运行 --status。" >&2
      cleanup_rc=1
    fi
    exit "$cleanup_rc"
  }
  trap cleanup EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
  trap 'exit 129' HUP

  if ! write_start_and_verify "$temporary_start"; then
    die "临时阈值写入或校验失败；未继续尝试固件写入"
  fi
  restore_needed=1
  save_state active
  started_at=$(date +%s)

  echo "临时阈值已生效。前台等待充到 ${stop_threshold}%（Ctrl-C 可安全停止）……"
  while true; do
    current_capacity=$(read_one_line "$CAPACITY_FILE")
    current_status=$(read_one_line "$STATUS_FILE")
    is_uint "$current_capacity" || die "电池容量不是有效整数：$current_capacity"
    printf '\r当前电量：%s%%（%s）' "$current_capacity" "$current_status"

    if (( current_capacity >= stop_threshold )); then
      echo
      echo "已达到停止阈值。"
      break
    fi
    if ! ac_online; then
      echo
      echo "交流电源已断开，停止等待。"
      break
    fi

    now=$(date +%s)
    elapsed=$((now - started_at))
    if (( elapsed >= MAX_SECONDS )); then
      echo
      echo "已达到最长运行时间，停止等待。" >&2
      rc=1
      break
    fi
    sleep "$POLL_SECONDS"
  done

  return "$rc"
}

main() {
  local mode="preview"
  local apply=0
  local plan_rc

  if (( $# == 0 )); then
    :
  elif (( $# == 1 )); then
    case $1 in
      --dry-run) ;;
      --apply) apply=1 ;;
      --status) mode="status" ;;
      --restore) mode="restore" ;;
      -h | --help)
        usage
        return 0
        ;;
      *)
        usage >&2
        die "未知参数：$1"
        ;;
    esac
  elif (( $# == 2 )) &&
    { [[ $1 == --restore && $2 == --apply ]] ||
      [[ $1 == --apply && $2 == --restore ]]; }; then
    mode="restore"
    apply=1
  else
    usage >&2
    die "参数组合无效"
  fi

  require_root

  case $mode in
    status)
      (( apply == 0 )) || die "--status 不能与 --apply 同时使用"
      show_saved_state
      ;;
    restore)
      exec 9>"$LOCK_FILE"
      flock -n 9 || die "另一个 dell-chargeonce 进程正在运行"
      if (( apply == 0 )); then
        show_saved_state
        echo "以上仅为预览；恢复需显式添加 --apply。"
      else
        restore_saved_state
      fi
      ;;
    preview)
      if (( apply == 0 )); then
        if collect_plan; then
          print_plan
          echo "以上仅为预览；未修改任何阈值。执行需显式添加 --apply。"
        else
          plan_rc=$?
          (( plan_rc == 2 )) || return "$plan_rc"
        fi
      else
        run_charge_once
      fi
      ;;
  esac
}

main "$@"
