#!/bin/csh

cd Workfiles

foreach i (*.eps)
  echo converting $i
  convert -density 144x144 $i ${i}conv.gif
end

#
# make a gif-animation
#
convert -delay 10 *.gif animation.gif

