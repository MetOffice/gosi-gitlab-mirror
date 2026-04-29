from netCDF4 import Dataset
import numpy as np
import sys

if (len(sys.argv) < 5): 
   print('ERROR : usage = python remapping_sab.py nx ny ny_sabNorth ny_sabSouth') 
   sys.exit()
   #ny_sabN and ny_sabS are respectively nb of y points on "noth pole and south pole sides"


nx = int(sys.argv[1])  
ny = int(sys.argv[2])

ny_sabN = int(sys.argv[3])
ny_sabS = int(sys.argv[4])

ny_sab = ny_sabN + ny_sabS + 1 ## +1 is for line of zeros in between

#total number of grid points NEMO
dst_grid_size = nx * ny

#total number of grid points NEMO
src_grid_size = nx * ny_sab

#total number of pairs of grid points (NEMO,SAB) linked by the remapping.
# = dst_grid_size - nx, for the zero line
num_links = src_grid_size - nx

#only one set of wheight per grid point, (each of them is equal to 1)
num_wgts = 1

#  Création du fichier NetCDF
ncfile = Dataset('rMp_SAB_to_NEMO.nc', mode='w', format='NETCDF4')

#  Définition des dimensions
ncfile.createDimension('src_grid_size', src_grid_size)
ncfile.createDimension('dst_grid_size', dst_grid_size)
ncfile.createDimension('num_links', num_links)
ncfile.createDimension('num_wgts', num_wgts)

#  Définition des variables
src_address_var = ncfile.createVariable('src_address', 'i4', ('num_links',))
dst_address_var = ncfile.createVariable('dst_address', 'i4', ('num_links',))
remap_matrix_var = ncfile.createVariable('remap_matrix', 'f8', ('num_links', 'num_wgts'))

# dst offsets : de x = 0 à ny_sabN -1 (bloc pole Nord), concaténé avec bloc Sud (de  x = ny - ny_sabS à ny -1  

for i in range(num_links):
    if (i < nx * ny_sabN ): 
        dst_address_var[i] = i+1 
    else:
        dst_address_var[i] = nx * (ny - ny_sabS - ny_sabN ) + i+1 
        if (i == nx*ny_sabN): print(" dst_address[nx * ny_sabN] = ",dst_address_var[i])

# src offsets : de x = 0 à ny_SabN-1 (bloc Nord), attention à ligne de 0 de séparation
#           ; puis x = ny_SabN+1 à dst_grid_size (bloc sud)


for i in range(num_links):
    if (i < nx * ny_sabN ):
        src_address_var[i] = i+1
    else:
        src_address_var[i] = nx * (ny_sab - ny_sabS - ny_sabN) + i+1 
        if (i == nx*ny_sabN): print("src_address[nx * ny_sabN] = ",src_address_var[i])


#check:
print('src_address =',src_address_var[:],'size =',np.size(src_address_var))
print('dst_address =',dst_address_var[:],'size =',np.size(dst_address_var))


remap_matrix_var[:,:] = 1.000 #column vector with only "1"
print('remap_matrix_var',remap_matrix_var[:,:],'size =',np.size(remap_matrix_var))

# Sauvegarde et fermeture du fichier
ncfile.close()

print(" Fichier NetCDF 'rMp_SAB_to_NEMO.nc' créé avec succès.")
