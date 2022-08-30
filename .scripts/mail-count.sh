#!/usr/bin/bash

#run OfflineIMAP once, with quiet interface
muw sync

#count new mail for every maildir
maildirnew="$HOME/.local/share/mail/*/*/new/"
new="$(find $maildirnew -type f | wc -l)"

if [ $new -gt 0 ]
then
    echo $new
fi
