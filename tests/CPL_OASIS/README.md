
# Coupling with OASIS test case

The CPL_OASIS test case allows to set up and check a basic coupling of NEMO to a simple TOYATM fake atmposhere through the OASIS coupler.  
A very limited number of fields are exchanged between NEMO and the TOYATM. The test checks that the fields are indeed exchanged through 
OASIS and that the `ATSSTSST` field of sea surface temparture received by the TOYATM is correct. If the test is sucessful, it states that 
the set up of NEMO-OASIS interface in the NEMO SBC module works fine.  
We provide here a description of details of this experiment so as as how to run it and check test is sucessful.


## Objectives

This test case enables the OASIS interface in NEMO (in the surc/OCE/SBC directory). A few fields are sent and received by NEMO and by 
TOYATM (the simplified "atmosphere"). The success of this test (see below **Verification**) indicates that :
* the OASIS interface in NEMO is functionnal (some fields are sent and received)
* The sea surface temperature received by the TOYATM is correct

This test case can also be used as a template to set up a coupling between NEMO and an atmospheric model through OASIS.


## Detailed description

This test case is a set up of NEMO (dynamics, sea-ice and biogeochemistry) on a global 2° grid (as in ithe ORCA2_ICE_PISCES reference 
configuration), except NEMO is here coupled to a "toyatm" through OASIS.

NEMO will run 160 time-steps (10 days). The coupling is done at each timestep (nn_fsbc=1 in namelist_cfg). The fields exchanged with 
the toyatm are defined in the `&namsbc_cpl` block in namelist_cfg file.

This test case requires:
* OASIS3-MCT to be downloaded from [OASIS repository](https://gitlab.com/cerfacs/oasis3-mct) and compiled following OASIS documentation
* XIOS2-trunk compiled with OASIS3-MCT using the `--use_oasis 'oasis3_mct` option and modified arch files to include OASIS library
* NEMO compiled with OASIS3-MCT using an arch file correctly modified to include OASIS library
* TOYATM located in NEMO/tools directory and compiled using `tools/maketools` command and the same arch file including OASIS as NEMO

The tests/CPL_OASIS directory contains :
* cpp_CPL_OASIS.fcm defining the active cpp keys for NEMO
* EXPREF directory containing the NEMO namelists, XIOS xml files, a template job tu run the test case and a script to check the results 
and to produce the report.

The tools/TOYATM/EXP directory contains the OASIS namcouple file and 2 netcdf files defining grids and masks of the TOYATM model.

For NEMO > 5.0 versions, the namcouple file has been moved to tests/CPL_OASIS/EXPREF directory and netcdf files are available directly 
from [Jasmin server](https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/r5.0.0/CPL_OASIS_v5.0.0.tar.gz).


## Building the CPL_OASIS test case

* Build the NEMO executable for this CPL_OASIS test case:  
First you need to add the correct OASIS library path in your arch file in the %OASIS_HOME variable or to declare the `$OASIS_prefix` 
variable to use the `build_arch_auto.sh` script to generate your arch file.  
Then, in your local NEMO root directory:
``` 
./makenemo -a CPL_OASIS -n MYCPL_OASIS -m "your arch file"
```
This makenemo command will create the test case in cfgs/MYCPL_OASIS.

* Build the TOYATM executable:
```
cd tools 
./maketools -n TOYATM -m "your arch file"
```

## Running the test case

```
cd tests/MYCPL_OASIS/EXP00
cp ../../CPL_OASIS/job_run_CPL_TESTCASE .
```
Adapt the job_run_CPL_TESTCASE to your target computer and run it.  
In this directory, the job_run_CPL_TESTCASE script contains all the steps to run the testcase. These steps are commented in the file.  
After adapting the headers for your batch system and the paths for the files, run this script through the batch system of your target computer.

## Verification and validation of the test case

The script `gen_report.sh` located in the CPL_OASIS directory allows to check if the run came to a sucessful ending:
```
cd MYCPL_OASIS/EXP00
cp ../../CPL_OASIS/gen_report.sh .
./gen_report.sh
```
If the report is successful, a final check should be done by visualising the ATSSTSST_toyatm_01.nc (using ncview or any other visualiser
for NETCDF files) and comparing it to the reference ref_ATSSTSST_last_time_step.jpg image in the CPL_OASIS directory: the two visualisations must look alike.

![](./ref_ATSSTSST_last_time_step.jpg)