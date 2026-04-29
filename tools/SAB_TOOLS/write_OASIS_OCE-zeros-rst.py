import numpy as np
import xarray as xr
import argparse

def main(nn_lvl3D, nx_src, ny_src):

    # =============================
    # 1) rst_ss_2D.nc
    # =============================

    # 2D bundles for sea-surface variables
    vars_ss = {
        "O_ss_T": 7,  # 7 levels for SST (Sea Surface Temperature)
        "O_ss_U": 3,  # 3 levels for U velocity
        "O_ss_V": 3   # 3 levels for V velocity
    }

    data_ss = {}

    # Creating 2D bundles for surface variables
    for var, nlev in vars_ss.items():
        for k in range(1, nlev + 1):
            varname = f"{var}.{k:03d}"
            data_ss[varname] = (("y", "x"),
                                np.zeros((ny_src, nx_src), dtype="float64"))

    ds_ss = xr.Dataset(
        data_ss,
        coords={
            "x": np.arange(nx_src),
            "y": np.arange(ny_src),
        }
    )

    ds_ss.to_netcdf("rst_ss_2D.nc",encoding={var: {"_FillValue": None} for var in ds_ss.variables})
    print("rst_ss_2D.nc created.")


    # =============================
    # 2) rst_UuVvTs.nc
    # =============================

    # The 3D variables are actually 2D bundles!
    vars_3d = ["O_Uu_3D", "O_Vv_3D", "O_Tt_3D"]

    data_3d = {}

    for var in vars_3d:
        for k in range(1, nn_lvl3D + 1):
            varname = f"{var}.{k:03d}"
            data_3d[varname] = (("y", "x"),
                                np.zeros((ny_src, nx_src), dtype="float64"))

    ds_3d = xr.Dataset(
        data_3d,
        coords={
            "x": np.arange(nx_src),
            "y": np.arange(ny_src),
        }
    )

    ds_3d.to_netcdf("rst_UuVvTs.nc",encoding={var: {"_FillValue": None} for var in ds_3d.variables})
    print("rst_UuVvTs.nc created.")


# =============================
#  Running the script
# =============================
if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description="Generates OASIS restart files with zeroed fields."
    )
    parser.add_argument("--nx_src", type=int, required=True,
                        help="Number of grid points along the x-axis")
    parser.add_argument("--ny_src", type=int, required=True,
                        help="Number of grid points along the y-axis")
    parser.add_argument("--nn_lvl3D", type=int, required=True,
                        help="Number of levels for O_Uu_3D, O_Vv_3D, and O_Tt_3D")
    args = parser.parse_args()

    main(args.nn_lvl3D, args.nx_src, args.ny_src)
