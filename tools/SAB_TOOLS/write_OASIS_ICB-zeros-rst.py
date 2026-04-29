import numpy as np
import sys
from netCDF4 import Dataset

def create_netcdf(lat, lon, z, fnc_name):
    # Créer un fichier NetCDF
    dataset = Dataset(fnc_name, 'w', format='NETCDF4')
    
    # Définir les dimensions
    dataset.createDimension('lon', lon)
    dataset.createDimension('lat', lat)
    
    # Créer la ou les variable 2D Bberg_fx, Bberg_fx.001, Bberg_fx.00z  (lon, lat) avec des valeurs initiales de 0
    if (z == 1): 
        Bberg_fx = dataset.createVariable('Bberg_fx', np.float64, ('lat', 'lon'))
        Bberg_fx[:] = np.zeros((lat, lon), dtype=np.float64)
    
    elif (z == 2):     
       Bberg_fx_001 = dataset.createVariable('Bberg_fx.001', np.float64, ('lat', 'lon'))
       Bberg_fx_001[:] = np.zeros((lat, lon), dtype=np.float64)
       Bberg_fx_002 = dataset.createVariable('Bberg_fx.002', np.float64, ('lat', 'lon'))
       Bberg_fx_002[:] = np.zeros((lat, lon), dtype=np.float64)

            
    else : 
        print("z must = either 1 or 2 (if purely 2D or no bundle file, set z=1 ")
        sys.exit(1)
    
    # Fermer le fichier NetCDF
    dataset.close()
    print("Le fichier  ",fnc_name," a été créé avec succès !")

if __name__ == '__main__':
    # Vérifier si les bons arguments sont passés
    if len(sys.argv) != 5:
        print("Usage: python script.py <lat> <lon> <z> <file_name.nc>")
        sys.exit(1)
    
    # Récupérer les arguments depuis la ligne de commande
    try:
        lat = int(sys.argv[1])
        lon = int(sys.argv[2])
        z = int(sys.argv[3])
        fname = sys.argv[4]  
    except ValueError:
        print("Erreur : Les arguments lon, lat, et z doivent être des entiers.")
        sys.exit(1)
    
    # Appeler la fonction avec les arguments passés
    create_netcdf(lat, lon, z, fname)

