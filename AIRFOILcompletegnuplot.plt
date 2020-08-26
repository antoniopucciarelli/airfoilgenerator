set     multiplot layout 1,3 
unset   key
set     xlabel 'x'
set     ylabel 'y'
unset   xtics
unset   ytics
set     size    ratio -1
plot    'data.dat' with lines, 'meanline.dat' with lines
set     size    ratio -1
plot    'normal_tangent_vec.dat' using 1:2:5:6 with vectors head, 'data.dat' with lines
set     size    ratio -1
plot    'normal_tangent_vec.dat' using 1:2:3:4  with vectors head, 'data.dat' with lines  
unset   multiplot