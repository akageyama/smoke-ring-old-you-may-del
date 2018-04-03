#!/bin/csh

foreach i (`cat nloop_list.txt | awk '{print $2}'`)
  ./make_plotscript.csh $i 
  gnuplot 'Workfiles/plot.gp'
end
