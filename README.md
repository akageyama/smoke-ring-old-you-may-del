# smoke-ring
A simple CFD code to solve a smoke ring formation and propagation
in a gas contained in a rectangular box. This is for education.

## Usage (on iMac)

    cd src
    make
    make data
    cd ../slice_grapher
    make figs
    open Workfiles/*.eps

## Parameters

- Change (NX,NY,NZ) in src/constants.f90 and slice_grapher/constants.f90
- Change values in src/sample.namelist
    

## Basic equation

Compressible Navier-Stokes equations for an ideal gas.

## Numerical method

Second-order central difference with 4-th order Runge-Kutta integration.

## Boundary condition

Periodic boundary condition in all (three) dimensions.

## Programing language

Fortran95.

## Author

Akira Kageyama, Kobe Univ., Japan
 email: kage@port.kobe-u.ac.jp
