#! /usr/bin/env python3

import xarray as xr
import sys

'''
A simple script to check for incorrect variable masks in NEMO diagnostics produced by XIOS.

Usage: 
    `check_netcdf_masking.py masked_file unmasked_file`

Arguments:
    * `masked_file`- A NEMO NetCDF diagnostic file generated with `ln_mskland=.true.` 
    * `unmasked_file`- A NEMO NetCDF diagnostic file generated with `ln_mskland=.false.` 
    
Method:
    The data from `unmasked_file` corresponding in location to the missing data from `masked_file` is checked for
    valid data. Variables in `masked_file` with masked valid data are reported.
    
Assumptions:
  * Valid data is nonzero (not true for data that is not multiplied by the land-sea mask in NEMO)
  * Data does not contain a different land-sea mask to that applied by XIOS
  * Both files contain the same variables
'''

ds_land_masked = xr.open_dataset(sys.argv[1])       # Argument 1: file with land-sea mask
ds_not_masked = xr.open_dataset(sys.argv[2])        # Argument 2: file with no land-sea mask

vars_some_masked = {}
vars_all_masked = []

# Loop over non-coordinate variables
for name in sorted(ds_land_masked.data_vars):
    da_land_masked = ds_land_masked[name]

    is_masked = da_land_masked.isnull().any()
    all_masked = da_land_masked.isnull().all()

    # Only check masked variables
    if is_masked and (not all_masked):
        da_not_masked = ds_not_masked[name]

        # Masked points
        mask = da_land_masked.to_masked_array().mask
        n_masked = mask.sum()

        # Masked points that were valid data (assume "valid" = nonzero)
        da_ocean_masked = da_not_masked.where(mask, 0.)
        da_ocean_masked_has_valid = da_ocean_masked != 0.
        n_masked_valid = int(da_ocean_masked_has_valid.sum())

        # Store info on variables that have masked valid data
        if n_masked_valid > 0:
            if n_masked_valid == n_masked:
                vars_all_masked += [name]                               # All masked data is valid
            else:
                vars_some_masked[name] = [n_masked_valid, n_masked]     # Some masked data is valid

# Report variables that have masked valid data
if vars_some_masked or vars_all_masked:
    print(f'{len(vars_some_masked) + len(vars_all_masked)} / {len(ds_land_masked.data_vars)} '
          f'variables have masks that remove valid data:')

    if vars_some_masked:
        print(f'\tFor {len(vars_some_masked)} variables the masking removes valid data:')
        for var, (n_masked_valid, n_masked) in vars_some_masked.items():
            print(f'\t\t{var}- {n_masked_valid} / {n_masked} masked points were valid data')

    # If the data from NEMO wasn't multiplied by the land-sea mask, it's likely all the data will be "valid" (nonzero).
    # Report these variables separately- they will require a more in-depth investigation.
    if vars_all_masked:
        print(f'\tFor {len(vars_all_masked)} variables the masking removes ONLY valid data:')
        print('\t(It\'s likely the data was not multiplied by a land-sea mask in NEMO, '
              'so we can\'t automatically determine if the wrong mask was applied to the NetCDF files)')
        for var in vars_all_masked:
            print(f'\t\t{var}')
