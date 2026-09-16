MODULE icbrst
   !!======================================================================
   !!                       ***  MODULE  icbrst  ***
   !! Ocean physics:  read and write iceberg restart files
   !!======================================================================
   !! History : 3.3.1 !  2010-01  (Martin&Adcroft) Original code
   !!            -    !  2011-03  (Madec)          Part conversion to NEMO form
   !!            -    !                            Removal of mapping from another grid
   !!            -    !  2011-04  (Alderson)       Split into separate modules
   !!            -    !  2011-04  (Alderson)       Restore restart routine
   !!            -    !                            Currently needs a fixed processor
   !!            -    !                            layout between restarts
   !!            -    !  2015-11  Dave Storkey     Convert icb_rst_read to use IOM so can
   !!                                              read single restart files
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   icb_rst_read    : read restart file
   !!   icb_rst_write   : write restart file
   !!----------------------------------------------------------------------
   USE par_oce        ! NEMO parameters
   USE dom_oce        ! NEMO domain
   USE in_out_manager ! NEMO IO routines
   USE lib_mpp        ! NEMO MPI library, lk_mpp in particular
   USE netcdf         ! netcdf routines for IO
   USE iom
   USE icb_oce        ! define iceberg arrays
   USE icbutl         ! iceberg utility routines

   IMPLICIT NONE
   PRIVATE

   PUBLIC   icb_rst_read    ! routine called in icbini.F90 module
   PUBLIC   icb_rst_write   ! routine called in icbstp.F90 module
   PUBLIC   icb_rst_open    ! routine called in icbrst.F90 and domain.F90 in SAB case
   PUBLIC   icb_rst_create  ! routine called in icbrst.F90 and daymod.F90 in SAB case
   
   INTEGER, PUBLIC :: nicbktid, nicbndastpid, nicbadatrjid, nicbntimeid ! used in daymod  
   INTEGER ::   nlonid, nlatid, nxid, nyid, nuvelid, nvvelid, nbasinid
   INTEGER ::   nmassid, nthicknessid, nwidthid, nlengthid
   INTEGER ::   nyearid, ndayid
   INTEGER ::   nscaling_id, nmass_of_bits_id, nheat_density_id, numberid
   INTEGER ::   nsiceid, nsheatid, ncalvid, ncalvhid, nkountid
   INTEGER ::   nret, ncid, nc_dim, nrst_test
   
   INTEGER,  DIMENSION(3)                  :: nstrt3, nlngth3
   INTEGER,  DIMENSION(4)                  :: n_rst_dims

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE icb_rst_create(kt)
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE icb_rst_open  ***
      !!
      !! ** Purpose :   open a iceberg restart file
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(IN) :: kt
      INTEGER ::   idg  ! number of digits
      INTEGER ::   ix_dim, iy_dim, ik_dim, in_dim
      CHARACTER(len=256)     :: cl_path
      CHARACTER(len=256)     :: cl_filename
      CHARACTER(len=8  )     :: cl_kt
      CHARACTER(LEN=12 )     :: clfmt            ! writing format
      !!----------------------------------------------------------------------

      ! Only operate on the restart timestep itself.
      ! Assume we write iceberg restarts to same directory as ocean restarts.
      !
      ! directory name
      cl_path = TRIM(cn_icbrst_outdir)
      IF( cl_path(LEN_TRIM(cl_path):) /= '/' ) cl_path = TRIM(cl_path) // '/'
      !
      ! file name
      WRITE(cl_kt, '(i8.8)') kt
      cl_filename = TRIM(cexper)//"_"//cl_kt//"_"//TRIM(cn_icbrst_out)
      IF( lk_mpp ) THEN
         idg = MAX( INT(LOG10(REAL(MAX(1,jpnij-1),wp))) + 1, 4 )          ! how many digits to we need to write? min=4, max=9
         WRITE(clfmt, "('(a,a,i', i1, '.', i1, ',a)')") idg, idg          ! '(a,a,ix.x,a)'
         WRITE(cl_filename,  clfmt) TRIM(cl_filename), '_', narea-1, '.nc'
      ELSE
         WRITE(cl_filename,'(a,a)') TRIM(cl_filename),               '.nc'
      ENDIF
      
      cfile_icb_rst = TRIM(cl_path)//TRIM(cl_filename)

      IF ( lwp .AND. nn_verbose_level >= 0) WRITE(numout,'(2a)') 'icebergs, write_restart: creating ',  &
        &                                                         TRIM(cl_path)//TRIM(cl_filename)

      nret = NF90_CREATE(TRIM(cl_path)//TRIM(cl_filename), NF90_CLOBBER, numrbw)
      IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_create failed')
   
       ! Dimensions
      nret = NF90_DEF_DIM(numrbw, 'x', Ni_0, ix_dim)
      IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_def_dim x failed')

      nret = NF90_DEF_DIM(numrbw, 'y', Nj_0, iy_dim)
      IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_def_dim y failed')

      nret = NF90_DEF_DIM(numrbw, 'c', nclasses, nc_dim)
      IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_def_dim c failed')

      nret = NF90_DEF_DIM(numrbw, 'k', nkounts, ik_dim)
      IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_def_dim k failed')

      ! global attributes
      IF( lk_mpp ) THEN
         ! Set domain parameters (assume jpdom_local_full)
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_number_total'   , jpnij                          )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_number'         , narea-1                        )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_dimensions_ids' , (/ 1          , 2           /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_size_global'    , (/ Ni0glo     , Nj0glo      /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_size_local'     , (/ Ni_0       , Nj_0        /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_position_first' , (/ mig(Nis0,0), mjg(Njs0,0) /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_position_last'  , (/ mig(Nie0,0), mjg(Nje0,0) /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_halo_size_start', (/ 0          , 0           /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_halo_size_end'  , (/ 0          , 0           /) )
         nret = NF90_PUT_ATT( numrbw, NF90_GLOBAL, 'DOMAIN_type'           , 'BOX'                          )
      ENDIF
      
      IF (associated(first_berg)) then
         nret = NF90_DEF_DIM(numrbw, 'n', NF90_UNLIMITED, in_dim)
         IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_def_dim n failed')
      ENDIF

      ! Time variables
      nret = NF90_DEF_VAR(numrbw, 'kt'    , NF90_DOUBLE, nicbktid)     ! time-step
      nret = NF90_DEF_VAR(numrbw, 'ndastp', NF90_DOUBLE, nicbndastpid) ! date
      nret = NF90_DEF_VAR(numrbw, 'adatrj', NF90_DOUBLE, nicbadatrjid) ! number of elapsed days since the begining of the run [s]
      nret = NF90_DEF_VAR(numrbw, 'ntime' , NF90_DOUBLE, nicbntimeid)  ! time

      ! Variables
      nret = NF90_DEF_VAR(numrbw, 'kount'       , NF90_INT   , (/ ik_dim /), nkountid)
      nret = NF90_DEF_VAR(numrbw, 'calving'     , NF90_DOUBLE, (/ ix_dim, iy_dim /), ncalvid)
      nret = NF90_DEF_VAR(numrbw, 'calving_hflx', NF90_DOUBLE, (/ ix_dim, iy_dim /), ncalvhid)
      nret = NF90_DEF_VAR(numrbw, 'stored_ice'  , NF90_DOUBLE, (/ ix_dim, iy_dim, nc_dim /), nsiceid)
      nret = NF90_DEF_VAR(numrbw, 'stored_heat' , NF90_DOUBLE, (/ ix_dim, iy_dim /), nsheatid)

      ! Attributes
      nret = NF90_PUT_ATT(numrbw, ncalvid , 'long_name', 'iceberg calving')
      nret = NF90_PUT_ATT(numrbw, ncalvid , 'units', 'some')
      nret = NF90_PUT_ATT(numrbw, ncalvhid, 'long_name', 'heat flux associated with iceberg calving')
      nret = NF90_PUT_ATT(numrbw, ncalvhid, 'units', 'some')
      nret = NF90_PUT_ATT(numrbw, nsiceid , 'long_name', 'stored ice used to calve icebergs')
      nret = NF90_PUT_ATT(numrbw, nsiceid , 'units', 'kg/s')
      nret = NF90_PUT_ATT(numrbw, nsheatid, 'long_name', 'heat in stored ice used to calve icebergs')
      nret = NF90_PUT_ATT(numrbw, nsheatid, 'units', 'J/kg/s')

      IF ( ASSOCIATED(first_berg) ) THEN

         ! Only add berg variables for this PE if we have anything to say

         ! Variables
         nret = NF90_DEF_VAR(numrbw, 'lon', NF90_DOUBLE, in_dim, nlonid)
         nret = NF90_DEF_VAR(numrbw, 'lat', NF90_DOUBLE, in_dim, nlatid)
         nret = NF90_DEF_VAR(numrbw, 'xi', NF90_DOUBLE, in_dim, nxid)
         nret = NF90_DEF_VAR(numrbw, 'yj', NF90_DOUBLE, in_dim, nyid)
         nret = NF90_DEF_VAR(numrbw, 'basin', NF90_INT, in_dim, nbasinid)
         nret = NF90_DEF_VAR(numrbw, 'uvel', NF90_DOUBLE, in_dim, nuvelid)
         nret = NF90_DEF_VAR(numrbw, 'vvel', NF90_DOUBLE, in_dim, nvvelid)
         nret = NF90_DEF_VAR(numrbw, 'mass', NF90_DOUBLE, in_dim, nmassid)
         nret = NF90_DEF_VAR(numrbw, 'thickness', NF90_DOUBLE, in_dim, nthicknessid)
         nret = NF90_DEF_VAR(numrbw, 'width', NF90_DOUBLE, in_dim, nwidthid)
         nret = NF90_DEF_VAR(numrbw, 'length', NF90_DOUBLE, in_dim, nlengthid)
         nret = NF90_DEF_VAR(numrbw, 'number', NF90_INT, (/ik_dim,in_dim/), numberid)
         nret = NF90_DEF_VAR(numrbw, 'year', NF90_INT, in_dim, nyearid)
         nret = NF90_DEF_VAR(numrbw, 'day', NF90_DOUBLE, in_dim, ndayid)
         nret = NF90_DEF_VAR(numrbw, 'mass_scaling', NF90_DOUBLE, in_dim, nscaling_id)
         nret = NF90_DEF_VAR(numrbw, 'mass_of_bits', NF90_DOUBLE, in_dim, nmass_of_bits_id)
         nret = NF90_DEF_VAR(numrbw, 'heat_density', NF90_DOUBLE, in_dim, nheat_density_id)

         ! Attributes
         nret = NF90_PUT_ATT(numrbw, nlonid, 'long_name', 'longitude')
         nret = NF90_PUT_ATT(numrbw, nlonid, 'units', 'degrees_E')
         nret = NF90_PUT_ATT(numrbw, nlatid, 'long_name', 'latitude')
         nret = NF90_PUT_ATT(numrbw, nlatid, 'units', 'degrees_N')
         nret = NF90_PUT_ATT(numrbw, nxid, 'long_name', 'x grid box position')
         nret = NF90_PUT_ATT(numrbw, nxid, 'units', 'fractional')
         nret = NF90_PUT_ATT(numrbw, nyid, 'long_name', 'y grid box position')
         nret = NF90_PUT_ATT(numrbw, nbasinid, 'units', 'fractional')
         nret = NF90_PUT_ATT(numrbw, nbasinid, 'long_name', 'source basin id')
         nret = NF90_PUT_ATT(numrbw, nyid, 'units', '1')
         nret = NF90_PUT_ATT(numrbw, nuvelid, 'long_name', 'zonal velocity')
         nret = NF90_PUT_ATT(numrbw, nuvelid, 'units', 'm/s')
         nret = NF90_PUT_ATT(numrbw, nvvelid, 'long_name', 'meridional velocity')
         nret = NF90_PUT_ATT(numrbw, nvvelid, 'units', 'm/s')
         nret = NF90_PUT_ATT(numrbw, nmassid, 'long_name', 'mass')
         nret = NF90_PUT_ATT(numrbw, nmassid, 'units', 'kg')
         nret = NF90_PUT_ATT(numrbw, nthicknessid, 'long_name', 'thickness')
         nret = NF90_PUT_ATT(numrbw, nthicknessid, 'units', 'm')
         nret = NF90_PUT_ATT(numrbw, nwidthid, 'long_name', 'width')
         nret = NF90_PUT_ATT(numrbw, nwidthid, 'units', 'm')
         nret = NF90_PUT_ATT(numrbw, nlengthid, 'long_name', 'length')
         nret = NF90_PUT_ATT(numrbw, nlengthid, 'units', 'm')
         nret = NF90_PUT_ATT(numrbw, numberid, 'long_name', 'iceberg number on this processor')
         nret = NF90_PUT_ATT(numrbw, numberid, 'units', 'count')
         nret = NF90_PUT_ATT(numrbw, nyearid, 'long_name', 'calendar year of calving event')
         nret = NF90_PUT_ATT(numrbw, nyearid, 'units', 'years')
         nret = NF90_PUT_ATT(numrbw, ndayid, 'long_name', 'year day of calving event')
         nret = NF90_PUT_ATT(numrbw, ndayid, 'units', 'days')
         nret = NF90_PUT_ATT(numrbw, nscaling_id, 'long_name', 'scaling factor for mass of calving berg')
         nret = NF90_PUT_ATT(numrbw, nscaling_id, 'units', 'none')
         nret = NF90_PUT_ATT(numrbw, nmass_of_bits_id, 'long_name', 'mass of bergy bits')
         nret = NF90_PUT_ATT(numrbw, nmass_of_bits_id, 'units', 'kg')
         nret = NF90_PUT_ATT(numrbw, nheat_density_id, 'long_name', 'heat density')
         nret = NF90_PUT_ATT(numrbw, nheat_density_id, 'units', 'J/kg')

      ENDIF ! associated(first_berg)

      ! End define mode
      nret = NF90_ENDDEF(numrbw)
      
   END SUBROUTINE icb_rst_create

   SUBROUTINE icb_rst_open()
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE icb_rst_open  ***
      !!
      !! ** Purpose :   open a iceberg restart file
      !!----------------------------------------------------------------------
      CHARACTER(len=256)           ::   cl_path
      CHARACTER(len=256)           ::   cl_filename
      CHARACTER(len=NF90_MAX_NAME) ::   cl_dname
      !!----------------------------------------------------------------------
      ! Find a restart file. Assume iceberg restarts in same directory as ocean restarts
      ! and are called TRIM(cn_icbrst_in)//
      cl_path = TRIM(cn_icbrst_indir)
      IF( cl_path(LEN_TRIM(cl_path):) /= '/' ) cl_path = TRIM(cl_path) // '/'
      cl_filename = TRIM(cn_icbrst_in)
      CALL iom_open( TRIM(cl_path)//cl_filename, numrbr )

   END SUBROUTINE icb_rst_open

   SUBROUTINE icb_rst_read()
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE icb_rst_read  ***
      !!
      !! ** Purpose :   read a iceberg restart file
      !!      NB: for this version, we just read back in the restart for this processor
      !!      so we cannot change the processor layout currently with iceberg code
      !!----------------------------------------------------------------------
      INTEGER                      ::   idim, ivar, iatt
      INTEGER                      ::   jn, iunlim_dim, ibergs_in_file
      INTEGER                      ::   ii, ij, iclass, ibase_err, imax_icb, imax_bas
      REAL(wp), DIMENSION(nkounts) ::   zdata      
      LOGICAL                      ::   ll_found_restart
      TYPE(iceberg)                ::   localberg ! NOT a pointer but an actual local variable
      TYPE(point)                  ::   localpt   ! NOT a pointer but an actual local variable
      !!----------------------------------------------------------------------
      ! Find a restart file. Assume iceberg restarts in same directory as ocean restarts
      ! and are called TRIM(cn_icbrst_in)//
#if ! defined key_sab
      CALL icb_rst_open
#endif

      imax_icb = 0
      imax_bas = 0
      IF( iom_file(numrbr)%iduld .GE. 0) THEN

         ibergs_in_file = iom_file(numrbr)%lenuld
         DO jn = 1,ibergs_in_file

            ! iom_get treats the unlimited dimension as time. Here the unlimited dimension 
            ! is the iceberg index, but we can still use the ktime keyword to get the iceberg we want. 

            CALL iom_get( numrbr, 'xi'     ,localpt%xi  , ktime=jn )
            CALL iom_get( numrbr, 'yj'     ,localpt%yj  , ktime=jn )

            ii = INT( localpt%xi + 0.5 ) + ( nn_hls-1 )
            ij = INT( localpt%yj + 0.5 ) + ( nn_hls-1 )
            ! Only proceed if this iceberg is on the local processor (excluding halos).
            IF ( ii >= mig(Nis0,nn_hls) .AND. ii <= mig(Nie0,nn_hls) .AND.   &
           &     ij >= mjg(Njs0,nn_hls) .AND. ij <= mjg(Nje0,nn_hls) ) THEN           
               CALL iom_get( numrbr, jpdom_unknown, 'number', zdata(:) , ktime=jn, kstart=(/1/), kcount=(/nkounts/) )
               localberg%number(:) = INT(zdata(:))
               imax_icb = MAX( imax_icb, INT(zdata(1)) )
               CALL iom_get( numrbr, 'mass_scaling' , localberg%mass_scaling, ktime=jn )
               CALL iom_get( numrbr, 'lon'          , localpt%lon           , ktime=jn )
               CALL iom_get( numrbr, 'lat'          , localpt%lat           , ktime=jn )
               CALL iom_get( numrbr, 'basin'        , zdata(1)              , ktime=jn )
               localpt%mbasid = INT(zdata(1))
               CALL iom_get( numrbr, 'uvel'         , localpt%uvel          , ktime=jn )
               CALL iom_get( numrbr, 'vvel'         , localpt%vvel          , ktime=jn )
               CALL iom_get( numrbr, 'mass'         , localpt%mass          , ktime=jn )
               CALL iom_get( numrbr, 'thickness'    , localpt%thickness     , ktime=jn )
               CALL iom_get( numrbr, 'width'        , localpt%width         , ktime=jn )
               CALL iom_get( numrbr, 'length'       , localpt%length        , ktime=jn )
               CALL iom_get( numrbr, 'year'         , zdata(1)              , ktime=jn )
               localpt%year = INT(zdata(1))
               CALL iom_get( numrbr, 'day'          , localpt%day           , ktime=jn )
               CALL iom_get( numrbr, 'mass_of_bits' , localpt%mass_of_bits  , ktime=jn )
               CALL iom_get( numrbr, 'heat_density' , localpt%heat_density  , ktime=jn )
               !
               CALL icb_utl_add( localberg, localpt )
               !
               imax_bas=MAX(imax_bas, localpt%mbasid)
            ENDIF
            !
         END DO
         !
      ELSE
         ibergs_in_file = 0
      ENDIF 

      ! Gridded variables
      CALL iom_get( numrbr, jpdom_auto,    'calving'     , src_calving  )
      CALL iom_get( numrbr, jpdom_auto,    'calving_hflx', src_calving_hflx  )
      CALL iom_get( numrbr, jpdom_auto,    'stored_heat' , berg_grid%stored_heat  )
      ! with jpdom_auto_xy, ue use only the third element of kstart and kcount.
      CALL iom_get( numrbr, jpdom_auto_xy, 'stored_ice'  , berg_grid%stored_ice, kstart=(/-99,-99,1/), kcount=(/-99,-99,nclasses/) )
      
      CALL iom_get( numrbr, jpdom_unknown, 'kount' , zdata(:) )
      num_bergs(:) = INT(zdata(:))
      !
      ! Sanity checks
      jn = icb_utl_count()
      IF ( lwp .AND. nn_verbose_level >= 0 )   &
         WRITE(numout,'(2(a,i5))') 'icebergs, read_restart_bergs: # bergs =',jn,' on PE',narea-1
      IF( lk_mpp ) THEN
      !  (JP) storing dimensions of rstart in n_rst_dims (reading from 'calving' 2D field)
         nrst_test = iom_varid(numrbr,'calving',n_rst_dims)
         !  (JP) if dimensions of rstart file opened by proc 0 are smaller the than whole domain (Ni0glo,Nj0glo), then there must be  multiple rstart files, hence the 'mpp_sum('icbrst', ibergs_in_file)'  
         IF( n_rst_dims(1)*n_rst_dims(2) < Ni0glo * Nj0glo   )  CALL mpp_sum('icbrst', ibergs_in_file) 
         CALL mpp_sum('icbrst', jn) 
      ENDIF
      IF( lwp )   WRITE(numout,'(a,i5,a,i5,a)') 'icebergs, icb_rst_read: there were',ibergs_in_file,   &
         &                                    ' bergs in the restart file and', jn,' bergs have been read'
      !
      IF (jn /= ibergs_in_file) CALL ctl_stop('Some icebergs lost during restart read') 
      !
#if ! defined key_sab
      ! Close file
      CALL iom_close( numrbr )
#endif
      !
      ! Confirm that all areas have a suitable base for assigning new iceberg
      ! numbers. This will not be the case if restarting from a collated dataset
      ! (even if using the same processor decomposition)
      !
      ibase_err = 0
      IF( num_bergs(1) < 0 .AND. num_bergs(1) /= narea - jpnij ) THEN
         ! If this area has never calved a new berg then the base should be
         ! set to narea - jpnij. If it is negative but something else then
         ! a new base will be needed to guarantee unique, future iceberg numbers
         ibase_err = 1
      ELSEIF( MOD( num_bergs(1) - narea , jpnij ) /= 0 ) THEN
         ! If this area has a base which is not in the set {narea + N*jpnij}
         ! for positive integers N then a new base will be needed to guarantee 
         ! unique, future iceberg numbers
         ibase_err = 1
      ENDIF
      IF( lk_mpp ) THEN
         CALL mpp_sum('icbrst', ibase_err)
      ENDIF
      IF( ibase_err > 0 ) THEN
         ! 
         ! A new base is needed. The only secure solution is to set bases such that
         ! all future icebergs numbers will be greater than the current global maximum
         IF( lk_mpp ) THEN
            CALL mpp_max('icbrst', imax_icb)
         ENDIF
         num_bergs(1) = imax_icb - jpnij + narea
      ENDIF
      !

      IF( lk_mpp ) THEN
         CALL mpp_max('icbrst', imax_bas)
      ENDIF
      IF (imax_bas > nicbbas) THEN 
         IF (lwp) WRITE(numout,*) 'basin restart check :', imax_bas, nicbbas
         CALL ctl_stop('some iceberg basin id beyond the number of basin allowed in the simulation')
      END IF
      !
      IF( lwp .AND. nn_verbose_level >= 0 )  WRITE(numout,'(a)') 'icebergs, icb_rst_read: completed'
      !
   END SUBROUTINE icb_rst_read


   SUBROUTINE icb_rst_write( kt )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE icb_rst_write  ***
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt
      !
      INTEGER ::   jn   ! dummy loop index
      CHARACTER(512) :: cl_filename
      TYPE(iceberg), POINTER :: this
      TYPE(point)  , POINTER :: pt
      !!----------------------------------------------------------------------

      IF ( kt == nitrst ) THEN

         ! create restart file
#if ! defined key_sab
         CALL icb_rst_create(kt)
#endif
   
         ! --------------------------------
         ! now write some data
   
         nstrt3(1) = 1
         nstrt3(2) = 1
         nlngth3(1) = Ni_0
         nlngth3(2) = Nj_0
         nlngth3(3) = 1
   
         DO jn=1,nclasses
            nstrt3(3) = jn
            nret = NF90_PUT_VAR( numrbw, nsiceid, berg_grid%stored_ice(Nis0:Nie0,Njs0:Nje0,jn), nstrt3, nlngth3 )
            IF (nret .ne. NF90_NOERR) THEN
               IF( lwp ) WRITE(numout,*) TRIM(NF90_STRERROR( nret ))
               CALL ctl_stop('icebergs, write_restart: nf_put_var stored_ice failed')
            ENDIF
         ENDDO
         IF( lwp ) WRITE(numout,*) 'file: ',TRIM(cfile_icb_rst),' var: stored_ice  written'
   
         nret = NF90_PUT_VAR( numrbw, nkountid, num_bergs(:) )
         IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_put_var kount failed')
   
         nret = NF90_PUT_VAR( numrbw, nsheatid, berg_grid%stored_heat(Nis0:Nie0,Njs0:Nje0) )
         IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_put_var stored_heat failed')
         IF( lwp ) WRITE(numout,*) 'file: ',TRIM(cfile_icb_rst),' var: stored_heat written'
   
         nret = NF90_PUT_VAR( numrbw, ncalvid , src_calving(Nis0:Nie0,Njs0:Nje0) )
         IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_put_var calving failed')
         nret = NF90_PUT_VAR( numrbw, ncalvhid, src_calving_hflx(Nis0:Nie0,Njs0:Nje0) )
         IF (nret .ne. NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_put_var calving_hflx failed')
         IF( lwp ) WRITE(numout,*) 'file: ',TRIM(cfile_icb_rst),' var: calving written'
   
         IF ( ASSOCIATED(first_berg) ) THEN
   
            ! Write variables
            ! just write out the current point of the trajectory
   
            this => first_berg
            jn = 0
            DO WHILE (ASSOCIATED(this))
               pt => this%current_point
               jn=jn+1
   
               nret = NF90_PUT_VAR(numrbw, numberid, this%number, (/1,jn/), (/nkounts,1/) )
               nret = NF90_PUT_VAR(numrbw, nscaling_id, this%mass_scaling, (/ jn /) )
   
               nret = NF90_PUT_VAR(numrbw, nlonid, pt%lon, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nlatid, pt%lat, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nxid, pt%xi, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nyid, pt%yj, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nbasinid, pt%mbasid, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nuvelid, pt%uvel, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nvvelid, pt%vvel, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nmassid, pt%mass, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nthicknessid, pt%thickness, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nwidthid, pt%width, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nlengthid, pt%length, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nyearid, pt%year, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, ndayid, pt%day, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nmass_of_bits_id, pt%mass_of_bits, (/ jn /) )
               nret = NF90_PUT_VAR(numrbw, nheat_density_id, pt%heat_density, (/ jn /) )
   
               this=>this%next
            END DO
            !
         ENDIF ! associated(first_berg)
   
#if ! defined key_sab
         ! Finish up
         nret = NF90_CLOSE(numrbw)
         IF (nret /= NF90_NOERR) CALL ctl_stop('icebergs, write_restart: nf_close failed')
#endif
   
         ! Sanity check
         jn = icb_utl_count()
         IF ( lwp .AND. nn_verbose_level >= 0)   &
            WRITE(numout,'(2(a,i5))') 'icebergs, icb_rst_write: # bergs =',jn,' on PE',narea-1
         IF( lk_mpp ) THEN
            CALL mpp_sum('icbrst', jn)
         ENDIF
         IF(lwp)   WRITE(numout,'(a,i5,a,i5,a)') 'icebergs, icb_rst_write: ', jn,   &
            &                                    ' bergs in total have been written at timestep ', kt
         !
         ! Finish up
         !
      ENDIF
   END SUBROUTINE icb_rst_write
   !
   !!======================================================================
END MODULE icbrst
