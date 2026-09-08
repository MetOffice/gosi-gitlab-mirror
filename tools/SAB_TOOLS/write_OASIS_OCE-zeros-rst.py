import numpy as np
import sys
from netCDF4 import Dataset

def create_netcdf(lon, lat, lev):
    # Créer un fichier NetCDF 2D
    dataset = Dataset('rst_ss_2D_t.nc', 'w', format='NETCDF4')
    
    # Définir les dimensions
    dataset.createDimension('lon', lon)
    dataset.createDimension('lat', lat)
    
    vars_2d = ["icb_ssh","icb_sst","icb_sss","icb_fr_i","icb_at_i","icb_vt_i","icb_r3t","icb_ssu","icb_utau","icb_u_ice","icb_ssv","icb_vtau","icb_v_ice"]

    for varname in vars_2d:
        variable_net = dataset.createVariable(varname, np.float64, ('lat', 'lon'))
        variable_net[:] = np.zeros((lat, lon), dtype=np.float64)
    
    # Fermer le fichier NetCDF
    dataset.close()
    print("Le fichier rst_ss_2D_t.nc a été créé avec succès !")

    # Créer un fichier NetCDF 3D
    dataset3d = Dataset('rst_3d.nc', 'w', format='NETCDF4')
    
    # Définir les dimensions
    dataset3d.createDimension('lon', lon)
    dataset3d.createDimension('lat', lat)
    
    vars_3d = ["icb_uu_3D", "icb_vv_3D", "icb_tt_3D"]

    for var in vars_3d:
        for k in range(1, lev + 1):
            varname = f"{var}.{k:03d}"
            variable_net = dataset3d.createVariable(varname, np.float64, ('lat', 'lon'))
            variable_net[:] = np.zeros((lat, lon), dtype=np.float64)
    
    # Fermer le fichier NetCDF
    dataset3d.close()
    print("Le fichier rst_3d.nc a été créé avec succès !")

if __name__ == '__main__':
    # Vérifier si les bons arguments sont passés
    if len(sys.argv) != 4:
        print("Usage: python script.py <lat> <lon> <lev>")
        sys.exit(1)
    
    # Récupérer les arguments depuis la ligne de commande
    try:
        lon = int(sys.argv[1])
        lat = int(sys.argv[2])
        lev = int(sys.argv[3])
    except ValueError:
        print("Erreur : Les arguments lon, lat et lev doivent être des entiers.")
        sys.exit(1)
    
    # Appeler la fonction avec les arguments passés
    create_netcdf(lon, lat, lev)

