#!/bin/bash

# This script unzips a file and converts the filenames from GBK to UTF-8.
# It requires 'unzip' and 'convmv' to be installed.

# Check if a file path is provided as an argument.
if [ -z "$1" ]; then
  echo "Usage: $0 <path_to_zip_file>"
  exit 1
fi

ZIP_FILE="$1"

# Unzip the file. The -O option is used to specify the charset of the filenames in the zip file.
# If your version of unzip doesn't support this, you can remove it.
unzip -O gbk "$ZIP_FILE"

# Recursively convert filenames from GBK to UTF-8 in the current directory.
# The --notest flag applies the changes.
convmv -f gbk -t utf8 -r --notest .

echo "Unzipping and filename conversion complete."
