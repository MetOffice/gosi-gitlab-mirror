#### very little doc of 4 python scripts useful for ORCA2-SAB coupled simulations

1) write_OASIS_OCE-zeros-rst.py:

	run 'python write_OASIS_OCE-zeros-rst.py', the script will tell you what arguments it needs to run
        aim : generate sea-surface state and 3D uu, vv ts, oasis restart/input files filled with zeros (named rst_ss_2D.nc and rst_UuVvTs.nc)
        
        ++ : it can be used for any ORCA configuration 
        Warning : the fields are already regrouped in bundles, to match the structure of the namcouple file located in cfgs/SABO2/EXPREF

#=======================================

2) write_OASIS_ICB-zeros-rst.py:
 
        run 'python write_OASIS_ICB-zeros-rst.py', the script will tell you what arguments it needs to run
        aim : generate a single iceberg fluxes (water + heat) oasis restart/input file, filled with zeros.

        ++ : it can be used for any ORCA configuration
        Warning : the fields are grouped in a bundle, to match the structure of the namcouple file located
in cfgs/SABO2/EXPREF

#=======================================

3) remapping_SABtoNEMO.py and remapping_NEMOtoSAB.py

        run 'python remapping_SABtoNEMO.py', the script will tell you what arguments it needs to run
        aim : create specific .nc files for oasis remapping option. Absolutely mandatory if you have SAB running on a "cropped" grid, with only polar (north and south) latitudes.
  
        ++ : it can be used for any ORCA configuration
        -- : the computation of the addresses and sources of the points linked by the remapping (see the scripts) is assuming that a line of zeroes separates the concatenated North and South polar zones. BE CAREFUL WHAT domcfg file you use !!
