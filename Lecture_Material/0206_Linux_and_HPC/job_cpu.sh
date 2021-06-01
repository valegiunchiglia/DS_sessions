#PBS -lwalltime=24:00:00
#PBS -lselect=1:ncpus=32:mem=124gb

module load anaconda3/personal
source activate test1

python3 $HOME/test_script.py

mkdir $WORK/$PBS_JOBID
cp * $WORK/$PBS_JOBID

