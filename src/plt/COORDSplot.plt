unset   key
set     xlabel 'x'
set     ylabel 'y'
set     title  'UP & DOWN coords elements'
set     size    ratio -1
unset   xtics
unset   ytics
plot    'UP.dat'   using 1:2 with lines
plot    'DOWN.dat' using 1:2 with lines