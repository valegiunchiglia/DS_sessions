# LECTURE HPC - DATA SCIENCE HELPER TEAM

module load anaconda3/personal

# To run the first time you access the HPC
anaconda – setup
 
# Create a python conda environemnt
conda create -n test1 python=2.7 ipykernel # You can choose which python version to have
# Create a R conda enviornment
conda create -n test2 r=3.4.3 r-irkernel

#Activate the environment:
source activate test1

#Install desired packages:
conda install package_name[=version]

#Install python kernel for Jupyter:
python -m ipykernel install --user --name python2_test1 --display-name "Python2.7 (test1)"

# If you want an environment based on python 3
python3 -m ipykernel install --user --name python3_test1 --display-name "Python2.7 (test1)"

# If yoou want to install the R in your jupyter 
R

IRkernel::installspec(user = TRUE)
IRkernel::installspec(name = 'ir343', displayname = 'R 3.4.3 (test2)')
