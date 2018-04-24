#!/bin/csh

cd Workfiles

foreach i (*.eps)
  echo converting $i
  convert -alpha Remove -density 300x300 $i ${i}conv.gif
end

#
# make a gif-animation
#
echo 'making animation.gif'
convert -delay 10 *conv.gif animation.gif

