#PBS -l cputim_job=00:02:00
#PBS -l memsz_job=2gb
#PBS -l cpunum_job=1
#PBS -q SXS

cd ~/lecture-fluid/test/src/BoxFluid/job_vector

../code/fluidbox < test00.namelist

