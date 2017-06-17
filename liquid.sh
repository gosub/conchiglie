#!/bin/sh
FILENAME=${1}
NEWFILENAME=${FILENAME%.*}-seam.${FILENAME#*.}
SCALE=${2:-75}
REVSCALE=$((10000/${SCALE}))
convert ${FILENAME} -liquid-rescale ${SCALE}% -resize ${REVSCALE}% ${NEWFILENAME}
