# smoke-ring
A simple CFD code to solve a smoke ring formation and propagation.
This is for education.

## Physical Model
A gas contained in a rectangular box is driven by a localized force.


## Prerequisite

- [KVS-osmesa](https://github.com/vizlab-kobe/KVS.osmesa), for simulation run and (3-D) in-situ visualization.
- ImageMagic, for post-process (2-D) visualization.

## Usage

    cd src
    kvsmake -g smoke-ring
    kvsmake
    ./smoke-ring < sample.namelist
    cd ../slice_grapher
    make
    make gif  # ImageMagic (convert command) is used.
    make view  # Here we use Safari browser for gif animation.

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
