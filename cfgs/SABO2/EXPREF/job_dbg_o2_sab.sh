#!/usr/bin/env bash

#SBATCH --account=egi6035
#SBATCH --job-name=cpl_OCE-SAB
#SBATCH --constraint=GENOA
#SBATCH --nodes=1                    # ATTENTION ! 192 CPU reserved per node if "EXCLUSIVE" option
#SBATCH --ntasks-per-node=35        # number of MPI processes by node
#SBATCH --cpus-per-task=1            # number of CPUs by MPI process
#SBATCH --threads-per-core=1         # number of threads by CPU (usefull for OpenMP, or shared memory in general) 
#SBATCH --time=0:03:00              # physical time after which your job is killed

set -x
ulimit -s unlimited

NEMO_DIR=/path/to/executable/of/your/NEMO/component
SAB_DIR=/path/to/executable/of/your/SAB/component

rm -f nemo.exe || exit 5
rm -f sab.exe || exit 5

echo 'Getting executables : '
echo '     nemo.exe from : $NEMO_DIR'
echo '     sab.exe from : $SAB_DIR'

ln -s $NEMO_DIR/BLD/bin/nemo.exe ./nemo.exe  || exit 5
ln -s $SAB_DIR/BLD/bin/nemo.exe  ./sab.exe  || exit 5

echo "DIR BEFORE EXEC"
ls -l
echo ' Prepare launch of the run'
echo '----------------'

rm -f run_file 

touch ./run_file
echo 0-31 ./nemo.exe >>./run_file
echo 32-33 ./sab.exe >>./run_file
echo 34-34 ./xios_server.exe >>./run_file

echo -------- LOADING MODULES FOR EXECTION ON ADASTRA -----------
. /load/your/modules/for/your/architecture
echo 'Run the code'
echo '----------------'
time srun --multi-prog ./run_file
exit 0
