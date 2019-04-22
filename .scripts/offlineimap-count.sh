#!/usr/bin/bash

#run OfflineIMAP once, with quiet interface
offlineimap -o -q -u quiet

#count new mail for every maildir
maildirnew="$HOME/Mail/*/*/new/"
new="$(find $maildirnew -type f | wc -l)"

if [ $new -gt 0 ]
then
    echo $new
fi
