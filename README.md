# airfoilgenerator_FORTRAN

This program computes the geometry of a **NACA 4 digits** airfoil following Abbot steps.

## Code compilation steps

In order to compile the code: **CMake** and **GNUplot** are *required*.

```bash
cmake . 

make 

cd bin/ 

./agen
```

## **NACA0012** data generation and plot

These are the steps for the generation of a **NACA0012** airfoil.

Once in ```bin/```:

```bash
./agen 
# typing airfoil name 
# -> naca**** or NACA*** syntax is required
naca0012
# setting the number of coords for the discretization of airfoil chord
# typing the scaling factor for the airfoil
1 
# setting the airfoil AOA
0
# setting if the airfoil leading edge is translated into the domain
0
0
# storing data option 
Y
# printing the airfoil option
Y
# exiting the program 
n
```

All the airfoil's data are in the ```*.dat``` files.
