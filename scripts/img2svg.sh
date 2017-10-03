#! /bin/sh
#
# img2svg.sh
# Copyright (C) 2017 devil <adamfaryna@appdy.net>
#
# Distributed under terms of the Proprietary license.
#

input=$1

if [ -e input ]; then
  echo "Usage '$0 png|jpg|jpeg [output file name]'"
  exit 1
fi

command -v convert > /dev/null 2>&1 || { echo >&2 "Imagemagick not installed!"; exit 1; }
command -v potrace > /dev/null 2>&1 || { echo >&2 "potrace not installed!"; exit 1; }

output=$2
: ${output:=${input/\.*/'.svg'}}

convert -channel RGB -compress None $input bmp:- | potrace -s - -o $output

echo "Processing complete!"

