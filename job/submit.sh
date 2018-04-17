#!/bin/bash
#PBS -N serial
#PBS -q uv-test
#PBS -o stdout.log
#PBS -e stderr.log
#PBS -l select=1:ncpus=1
####PBS -l cputim_job=00:02:00
####PBS -l memsz_job=2gb
####PBS -l cpunum_job=1

cd ${PBS_O_WORKDIR}
dplace  ../src/smoke-ring < test00.namelist

