!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module creates grid description files for input to the SCRIP code
!
!-----------------------------------------------------------------------

MODULE scripgrid_mod

  USE kinds_mod
  USE constants
  USE iounits
  USE netcdf
  USE netcdf_mod
  
  IMPLICIT NONE

  !-----------------------------------------------------------------------
  !     module variables that describe the grid

  INTEGER (kind=int_kind), parameter :: &
       grid_rank = 2,         &
       grid_corners = 4
  INTEGER (kind=int_kind) :: nx, ny, grid_size
  INTEGER (kind=int_kind), dimension(2) :: &
       grid_dims,    &  ! size of x, y dimensions
       grid_dim_ids     ! ids of the x, y dimensions
  INTEGER (kind=int_kind), ALLOCATABLE, DIMENSION(:) :: &
       grid_imask          ! land-sea mask
  REAL (kind=int_kind), ALLOCATABLE, DIMENSION(:) :: &
       grid_center_lat, &  ! lat/lon coordinates for
       grid_center_lon     ! each grid center in degrees
  REAL (kind=dbl_kind), PARAMETER :: circle = 360.0

  !-----------------------------------------------------------------------
  !     module variables that describe the netcdf file

  INTEGER (kind=int_kind) :: &
       ncstat,           &   ! general netCDF status variable
       ncid_in

CONTAINS

  ! ==============================================================================
  
  SUBROUTINE convert(nm_in)
  
    ! -----------------------------------------------------------------------------
    ! - input variables
  
    CHARACTER(char_len), INTENT(in) ::  &
      nm_in
  
    ! -----------------------------------------------------------------------------
    ! - local variables
  
    CHARACTER(char_len) ::  &
      grid1_grid_in, grid2_grid_in, grid1_file, grid2_file,  &
      grid1_lon, grid1_lat, grid2_lon, grid2_lat, grid1_mask, grid2_mask
    INTEGER (kind=int_kind) :: &
      iunit
    REAL (kind=dbl_kind) :: &
      grid1_mask_value, grid2_mask_value
  
    namelist /grid_inputs/ grid1_grid_in, grid2_grid_in, grid1_file, grid2_file,  &
                           grid1_lon, grid1_lat, grid2_lon, grid2_lat,  &
                           grid1_mask, grid1_mask_value, grid2_mask, grid2_mask_value
  
    !-----------------------------------------------------------------------
    ! - namelist describing the processing
    !   note that mask_value is the minimum good value,
    !   so that where the mask is less than the value is masked

    grid1_grid_in = "data_era5.nc"
    grid1_lon = "lon"
    grid1_lat = "lat"
    grid1_mask = "none"
    grid1_mask_value = 0
    grid1_file = 'grid1_scripfmt.nc'

    grid2_grid_in = "domain_cfg.nc"
    grid2_lon = "glamt"
    grid2_lat = "gphit"
    grid2_mask = "none"
    grid2_mask_value = 0
    grid2_file = 'grid2_scripfmt.nc'

    call get_unit(iunit)
    open(iunit, file=nm_in, status='old', form='formatted')
    read(iunit, nml=grid_inputs) 
    call release_unit(iunit)

    WRITE(6,*) 
    WRITE(6,*) "Processing " // TRIM(grid1_grid_in)
    CALL readgrid(grid1_grid_in, grid1_lon, grid1_lat, grid1_mask, grid1_mask_value)
    CALL createSCRIPgrid(grid1_file, 'grid1 in SCRIP format')
    DEALLOCATE( grid_imask, grid_center_lon, grid_center_lat )

    WRITE(6,*) 
    WRITE(6,*) "Processing regular grid"
    CALL readgrid(grid2_grid_in, grid2_lon, grid2_lat, grid2_mask, grid2_mask_value)
    CALL createSCRIPgrid(grid2_file, 'grid2 in SCRIP format')
    DEALLOCATE( grid_imask, grid_center_lon, grid_center_lat )
 
  END SUBROUTINE convert
  
  ! ==============================================================================
  
  SUBROUTINE readgrid(grid_file_in, name_lon, name_lat, name_mask, value_mask)
  
    !-----------------------------------------------------------------------
    !
    !     This routine read the grid file from a NetCDF file
    !
    !-----------------------------------------------------------------------
    
    CHARACTER(*), INTENT(in) ::  &
         grid_file_in, name_lon, name_lat, name_mask
    REAL(kind=dbl_kind), INTENT(in) ::  &
         value_mask
    
    !-----------------------------------------------------------------------
    !     grid coordinates (note that a flux file just has lon and lat)
  
    REAL (kind=dbl_kind), ALLOCATABLE, DIMENSION(:) :: &
         lam, phi
    REAL (kind=dbl_kind), ALLOCATABLE, DIMENSION(:,:) :: &
         glam, &                   ! longitude
         gphi, &                   ! latitude
         glamc, &
         gphic
    REAL (kind=dbl_kind), ALLOCATABLE, DIMENSION(:,:) :: mask
  
    !-----------------------------------------------------------------------
    !     other local variables
  
    INTEGER (kind=int_kind) :: varid_lam, varid_phi, varid_mask
    INTEGER (kind=int_kind) :: nspace
    INTEGER (kind=int_kind), ALLOCATABLE, DIMENSION(:) :: grid_dimids
  
    !-----------------------------------------------------------------------
    !     read in grid info
    !
  
    WRITE(6,*) '   Opening: ', TRIM(grid_file_in)
    ncstat = nf90_open( grid_file_in, NF90_NOWRITE, ncid_in )
    call netcdf_error_handler(ncstat)
  
    WRITE(6,*) '   read longitude: ', TRIM(name_lon)
    WRITE(6,*) '   read latitude : ', TRIM(name_lat)
    ncstat = nf90_inq_varid( ncid_in, name_lat, varid_phi )
    call netcdf_error_handler(ncstat)
    ncstat = nf90_inq_varid( ncid_in, name_lon, varid_lam )
    call netcdf_error_handler(ncstat)
  
    ncstat = nf90_inquire_variable( ncid_in, varid_lam, ndims=nspace )
    call netcdf_error_handler(ncstat)
    ALLOCATE(grid_dimids(nspace))

    if (nspace == 1) then
      ncstat = nf90_inquire_variable( ncid_in, varid_lam, dimids=grid_dimids )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_inquire_dimension( ncid_in, grid_dimids(1), len=grid_dims(1) )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_inquire_variable( ncid_in, varid_phi, dimids=grid_dimids )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_inquire_dimension( ncid_in, grid_dimids(1), len=grid_dims(2) )
      call netcdf_error_handler(ncstat)
      nx = grid_dims(1)
      ny = grid_dims(2)
      grid_size = nx * ny
    
      WRITE(6,*) '   reading 1D lon/lat'
      ALLOCATE( lam(nx), phi(ny) )
      ncstat = nf90_get_var( ncid_in, varid_lam, lam )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_get_var( ncid_in, varid_phi, phi )
      call netcdf_error_handler(ncstat)
    
      ALLOCATE( glam(nx,ny), gphi(nx,ny))
      glam(:,:) = SPREAD(lam,2,ny)
      gphi(:,:) = SPREAD(phi,1,nx)
      DEALLOCATE( lam, phi )
    else

      ncstat = nf90_inquire_variable( ncid_in, varid_lam, dimids=grid_dimids )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_inquire_dimension( ncid_in, grid_dimids(1), len=grid_dims(1) )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_inquire_dimension( ncid_in, grid_dimids(2), len=grid_dims(2) )
      call netcdf_error_handler(ncstat)
      nx = grid_dims(1)
      ny = grid_dims(2)
      grid_size = nx * ny
    
      WRITE(6,*) '   reading 2D lon/lat'
      ALLOCATE( glam(nx,ny), gphi(nx,ny))
      ncstat = nf90_get_var( ncid_in, varid_lam, glam )
      call netcdf_error_handler(ncstat)
      ncstat = nf90_get_var( ncid_in, varid_phi, gphi )
      call netcdf_error_handler(ncstat)

    endif
    DEALLOCATE(grid_dimids)
    WRITE(6,FMT='("    input grid dimensions are:",2i6)') nx, ny

    !-----------------------------------------------------------------------
  
    ALLOCATE( grid_imask(grid_size) )
    grid_imask(:) = 1
    if (trim(name_mask) /= "none") then
      write(6,*) '   define mask'
      write(6,*) '      masking variable: ', TRIM(name_mask)
      write(6,*) '      masking value: ', value_mask
      ncstat = nf90_inq_varid( ncid_in, name_mask, varid_mask )
      call netcdf_error_handler(ncstat)
      ALLOCATE( mask(nx,ny) )
      ncstat = nf90_get_var( ncid_in, varid_mask, mask )
      call netcdf_error_handler(ncstat)
      IF(    value_mask >  1.0E6 ) THEN   ! 1.0E6 arbitrary defined...
         WHERE ( RESHAPE(mask(:,:),(/ grid_size /)) >= value_mask)   grid_imask = 0
      ELSEIF(value_mask < -1.0E6 ) THEN
         WHERE ( RESHAPE(mask(:,:),(/ grid_size /)) <= value_mask)   grid_imask = 0
      ELSE
         WHERE ( RESHAPE(mask(:,:),(/ grid_size /)) == value_mask)   grid_imask = 0
      ENDIF
    ELSE
      write(6,*) '   no mask'
    END IF
  
  ! For [N, E, W]-ward extrapolation near the poles, should we use stereographic (or
  ! similar) projection?  This issue will come for V,F interpolation, and for all
  ! grids with non-cyclic grids.
  
    ! -----------------------------------------------------------------------------
    ! - reshape for SCRIP input format
  
    ALLOCATE( grid_center_lon(grid_size), grid_center_lat(grid_size) )
    
    grid_center_lon(:) = RESHAPE( glam(:,:), (/ grid_size /) )
    grid_center_lat(:) = RESHAPE( gphi(:,:), (/ grid_size /) )
  
    DEALLOCATE( glam, gphi )
  
  END SUBROUTINE readgrid
  
  ! ==============================================================================
  
  SUBROUTINE mouldlon(lon_grid, nx, ny)

    ! -----------------------------------------------------------------------------
    ! - input variables

    INTEGER, INTENT(in) :: nx, ny
    REAL (kind=dbl_kind), INTENT(inout), DIMENSION(nx,ny) ::  &
      lon_grid
  
    ! -----------------------------------------------------------------------------
    ! - local variables

    INTEGER :: ix, iy
    REAL (kind=dbl_kind), DIMENSION(:,:), ALLOCATABLE ::  &
      dlon
    REAL :: step

    ! -----------------------------------------------------------------------------
    ! - try to eliminate any 360 degree steps in a grid of longitudes

    ALLOCATE(dlon(nx,ny))

    step = 0.75*circle
    dlon(:,:) = 0
    dlon(2:,:) = lon_grid(2:,:) - lon_grid(:nx-1,:)
    WHERE (dlon > -step .AND. dlon < step)
      dlon = 0.0
    ELSEWHERE
      dlon = -SIGN(circle,dlon)
    END WHERE

    ! - close your eyes this is nasty
    DO ix = 2,nx
      dlon(ix,:) = dlon(ix,:) + dlon(ix-1,:)
    END DO
    lon_grid = lon_grid + dlon

  END SUBROUTINE mouldlon
  
  ! ==============================================================================
  
  SUBROUTINE createSCRIPgrid(grid_file_out, grid_name)
  
    ! -----------------------------------------------------------------------------
    ! - input variables
  
    CHARACTER(*), INTENT(in) ::  grid_name, grid_file_out
  
    ! -----------------------------------------------------------------------------
    ! - local variables that describe the netcdf file
  
    INTEGER (kind=int_kind) :: &
         nc_grid_id,       &   ! netCDF grid dataset id
         nc_gridsize_id,   &   ! netCDF grid size dim id
         nc_gridcorn_id,   &   ! netCDF grid corner dim id
         nc_gridrank_id,   &   ! netCDF grid rank dim id
         nc_griddims_id,   &   ! netCDF grid dimensions id
         nc_grdcntrlat_id, &   ! netCDF grid center lat id
         nc_grdcntrlon_id, &   ! netCDF grid center lon id
         nc_grdimask_id,   &   ! netCDF grid mask id
         nc_gridarea_id,   &   ! netCDF grid area id
         nc_grdcrnrlat_id, &   ! netCDF grid corner lat id
         nc_grdcrnrlon_id      ! netCDF grid corner lon id

    INTEGER (kind=int_kind) :: ioldMode

    REAL (kind=dbl_kind), ALLOCATABLE, DIMENSION(:,:) :: zcor

    ! -----------------------------------------------------------------------------
    ! - create netCDF dataset for this grid
    ! - rewrite in nf90 
    ! - (bring out functional blocks into ncclear for readability)

    WRITE(6,*) 'Creating: ', TRIM(grid_file_out)

    ncstat = nf90_create (grid_file_out, IOR( NF90_64BIT_OFFSET, NF90_CLOBBER ), nc_grid_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_set_fill (nc_grid_id, NF90_NOFILL, ioldMode )
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att (nc_grid_id, NF90_GLOBAL, 'title', grid_name)
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid size dimension
  
    ncstat = nf90_def_dim (nc_grid_id, 'grid_size', grid_size, nc_gridsize_id)
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid rank dimension
  
    ncstat = nf90_def_dim (nc_grid_id, 'grid_rank', grid_rank, nc_gridrank_id)
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid corner dimension
  
    ncstat = nf90_def_dim (nc_grid_id, 'grid_corners', grid_corners, nc_gridcorn_id)
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid dim size array
  
    ncstat = nf90_def_var(nc_grid_id, 'grid_dims', NF90_INT, nc_gridrank_id, nc_griddims_id)
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid mask
  
    ncstat = nf90_def_var(nc_grid_id, 'grid_imask', NF90_BYTE, &
                          nc_gridsize_id, nc_grdimask_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att(nc_grid_id, nc_grdimask_id, 'units', 'unitless')
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid center latitude array
  
    ncstat = nf90_def_var(nc_grid_id, 'grid_center_lat', NF90_DOUBLE, &
                          nc_gridsize_id, nc_grdcntrlat_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att(nc_grid_id, nc_grdcntrlat_id, 'units', 'degrees')
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid center longitude array
  
    ncstat = nf90_def_var(nc_grid_id, 'grid_center_lon', NF90_DOUBLE, &
                          nc_gridsize_id, nc_grdcntrlon_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att(nc_grid_id, nc_grdcntrlon_id, 'units', 'degrees')
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid corner latitude array
  
    grid_dim_ids = (/ nc_gridcorn_id, nc_gridsize_id /)
    ncstat = nf90_def_var(nc_grid_id, 'grid_corner_lat', NF90_FLOAT, &
                          grid_dim_ids, nc_grdcrnrlat_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att(nc_grid_id, nc_grdcrnrlat_id, 'units', 'degrees')
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! - define grid corner longitude array
  
    ncstat = nf90_def_var(nc_grid_id, 'grid_corner_lon', NF90_FLOAT, &
                          grid_dim_ids, nc_grdcrnrlon_id)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_att(nc_grid_id, nc_grdcrnrlon_id, 'units', 'degrees')
    call netcdf_error_handler(ncstat)
  
    ! -----------------------------------------------------------------------------
    ! end definition stage
  
    ncstat = nf90_enddef(nc_grid_id)
    call netcdf_error_handler(ncstat)
    WRITE(6,*) '   header definition: done'
  
    ! -----------------------------------------------------------------------------
    ! write grid data
  
    ncstat = nf90_put_var(nc_grid_id, nc_griddims_id, grid_dims)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_var(nc_grid_id, nc_grdimask_id, grid_imask)
    call netcdf_error_handler(ncstat)
    WRITE(6,*) '   mask written'
    ncstat = nf90_put_var(nc_grid_id, nc_grdcntrlat_id, grid_center_lat)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_var(nc_grid_id, nc_grdcntrlon_id, grid_center_lon)
    CALL netcdf_error_handler(ncstat)
    WRITE(6,*) '   centers lon/lat written'

    ALLOCATE ( zcor(4, grid_size) )
    zcor(:,:) = -9999.
    ncstat = nf90_put_var(nc_grid_id, nc_grdcrnrlat_id, zcor)
    call netcdf_error_handler(ncstat)
    ncstat = nf90_put_var(nc_grid_id, nc_grdcrnrlon_id, zcor)
    call netcdf_error_handler(ncstat)
    WRITE(6,*) '   corners "fake" lon/lat written'

    ncstat = nf90_close(nc_grid_id)
    call netcdf_error_handler(ncstat)

    DEALLOCATE( zcor )

  END SUBROUTINE createSCRIPgrid
  
END MODULE scripgrid_mod

