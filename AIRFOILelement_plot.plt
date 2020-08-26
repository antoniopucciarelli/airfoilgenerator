unset   key
set     xlabel 'x'
set     ylabel 'y'
set     title  'airfoil elements'
set     size    ratio -1
unset   xtics
unset   ytics
plot    'elem.txt' using 1:2 with lines, 'elem.txt' using 1:2:3:4 with vectors, 'elem.txt' using 1:2:5:6 with vectors