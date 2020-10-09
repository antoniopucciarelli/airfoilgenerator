unset   key
set     xlabel 'x'
set     ylabel 'y'
set     title  'mean elements'
set     size    ratio -1
unset   xtics
unset   ytics
plot    'mean.dat' using 1:2 with lines