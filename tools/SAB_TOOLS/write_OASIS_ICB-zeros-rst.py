import numpy as np
import sys
from netCDF4 import Dataset

def create_netcdf(lon, lat, fnc_name):
    # Créer un fichier NetCDF
    dataset = Dataset(fnc_name, 'w', format='NETCDF4')
    
    # Définir les dimensions
    dataset.createDimension('lon', lon)
    dataset.createDimension('lat', lat)
    
    # Créer les variable 2D sab_berg_wfx:sab_berg_hcfx (lon, lat) avec des valeurs initiales de 0
    Bberg_wx = dataset.createVariable('sab_berg_wfx', np.float64, ('lat', 'lon'))
    Bberg_wx[:] = np.zeros((lat, lon), dtype=np.float64)
    Bberg_hx = dataset.createVariable('sab_berg_hcfx', np.float64, ('lat', 'lon'))
    Bberg_hx[:] = np.zeros((lat, lon), dtype=np.float64)
    
    # Fermer le fichier NetCDF
    dataset.close()
    print("Le fichier  ",fnc_name," a été créé avec succès !")

if __name__ == '__main__':
    # Vérifier si les bons arguments sont passés
    if len(sys.argv) != 4:
        print("Usage: python script.py <lat> <lon> <file_name.nc>")
        sys.exit(1)
    
    # Récupérer les arguments depuis la ligne de commande
    try:
        lon = int(sys.argv[1])
        lat = int(sys.argv[2])
        fname = sys.argv[3]  
    except ValueError:
        print("Erreur : Les arguments lon et lat doivent être des entiers.")
        sys.exit(1)
    
    # Appeler la fonction avec les arguments passés
    create_netcdf(lon, lat, fname)

