#!/bin/env bash

usage() { echo "Usage: $0 [-h <usage>] [-p <path>] [-i <ignored paths, separated with comma>] [-d <path traverse depth>]"; exit 1; }

while getopts ":p:d:i:" opt; do
    case ${opt} in
        p) dir=${OPTARG}
           ;;
        d) depth=${OPTARG}
           ;;
        i) IFS=','
           ign=($OPTARG)
           ;;
        h) usage
           ;;
        *) usage
           ;;
    esac
done
# shift $((OPTIND-1))

function expand_path() {
    local path
    local -a pathElements resultPathElements
    IFS=':' read -r -a pathElements <<<"$1"
    : "${pathElements[@]}"
    for path in "${pathElements[@]}"; do
        : "$path"
        case $path in
            "~+"/*)
                path=$PWD/${path#"~+/"}
                ;;
            "~-"/*)
                path=$OLDPWD/${path#"~-/"}
                ;;
            "~"/*)
                path=$HOME/${path#"~/"}
                ;;
            "~"*)
                username=${path%%/*}
                username=${username#"~"}
                IFS=: read -r _ _ _ _ _ homedir _ < <(getent passwd "$username")
                if [[ $path = */* ]]; then
                    path=${homedir}/${path#*/}
                else
                    path=$homedir
                fi
                ;;
        esac
        resultPathElements+=( "$path" )
    done
    local result
    printf -v result '%s:' "${resultPathElements[@]}"
    printf '%s\n' "${result%:}"
}

# ignore some paths
function ignore_paths() {
    if [ -z "${ign}" ]; then
        return
    else
        re=
        for i in ${ign[@]};
        do
            escape_path=$(realpath $(expand_path ${i}))
            # escape_path=${escape_path// /\\ }
            re+=" -not -path ${escape_path@Q}"
        done
        echo "${re}"
    fi
}

ign_pth_str=$(ignore_paths)

if [ -z "${dir}" ]; then
    dir=$(pwd)/
else
    dir=$(realpath "${dir}")
fi

if [ -z "${depth}" ]; then
    depth=1
fi

echo "Starting to extract coverarts in ${dir} with maximum search depth ${depth}."

unset IFS
find "${dir}/" -maxdepth $depth -type d -print0 |
    while IFS= read -rd '' dir; do
        if [ -f "${dir@Q}/Cover.jpg" ] || [ -f "${dir@Q}/cover.jpg" ]; then
            echo "coverart exists."
            continue
        else
            cmd="find ${dir@Q}/ ${ign_pth_str} -maxdepth 1 -type f -print0 -iname '*.mp3' -or -iname '*.flac' -or -iname '*.wav' -or -iname '*.dsd' -or -iname '*.dsf' -or -iname '*.dff' -or -iname '*.aac' -or -iname '*.ogg'"
            eval $cmd | while IFS= read -rd '' music; do
                extract_cmd="ffmpeg -i ${music@Q} ${dir@Q}/Cover.jpg"
                echo $extract_cmd
                eval $extract_cmd
                break
            done
        fi
    done
