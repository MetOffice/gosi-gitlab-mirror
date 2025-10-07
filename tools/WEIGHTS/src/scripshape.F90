PROGRAM scripshape
   !
   ! program to take output from the SCRIP weights generator
   ! and rearrange the data into a series of 2D fields suitable
   ! for reading with iom_get in NEMO configurations using the
   ! interpolation on the fly option
   !
   USE netcdf
   IMPLICIT none
   INTEGER    :: ncId, VarId, status
   INTEGER    :: nd, ns, nl, nw, sx, sy, dx, dy, dw, dn
   INTEGER    :: i, j, k, m, n
   !
   INTEGER(KIND=4), ALLOCATABLE :: src(:), dst(:)
   INTEGER(KIND=4), ALLOCATABLE :: nei(:,:)
   INTEGER(KIND=4), ALLOCATABLE :: src1(:,:,:)
   REAL(KIND=8), ALLOCATABLE :: wgt(:,:)
   REAL(KIND=8), ALLOCATABLE :: wgt1(:,:,:)
   LOGICAL  :: around
   CHARACTER(LEN=1) :: y

   CHARACTER(LEN=256) :: interp_file, output_file, name_file
   INTEGER            :: ew_wrap
   NAMELIST /shape_inputs/ interp_file, output_file, ew_wrap

   ! scripshape requires 1 arguments; the name of the file containing
   ! the input namelist.
   ! This namelist contains:
   !     the name of the input file containing the weights ! produced by SCRIP in its format;
   !     the name of the new output file which ! is to contain the reorganized fields ready for input to NEMO.
   !     ONLY the bucubic interpolation, define the east-west wrapping of the input grid: 
   !        ew_wrap < 0: no east-west cyclic data
   !        ew_wrap = 0: east-west cyclic data with no overlap
   !        ew_wrap > 0: east-west cyclic data with an overlap of no n columns
   !
   ! E.g.
   !     interp_file = 'data_nemo_bilin.nc'
   !     output_file = 'weights_bilin.nc'
   !     ew_wrap     = 0
   !

   IF (COMMAND_ARGUMENT_COUNT() == 1) THEN
      CALL GET_COMMAND_ARGUMENT(1, name_file)
   ELSE
      WRITE(6,*) 'enter name of namelist file'
      READ(5,*) name_file
   ENDIF

   ! namelist default values
   interp_file = 'none'
   output_file = 'none'
   
   ew_wrap = 0
   ! open and read namelist
   OPEN(12, FILE=name_file, STATUS='OLD', FORM='FORMATTED')
   READ(12, NML=shape_inputs)
   CLOSE(12)
   !
   INQUIRE(FILE = TRIM(interp_file), EXIST=around)
   IF (.not.around) THEN
      WRITE(*,*) 'Input file: '//TRIM(interp_file)//' not found'
      STOP
   ENDIF
   ! 
   INQUIRE(FILE = TRIM(output_file), EXIST=around)
   IF (around) THEN
      WRITE(*,*) 'Output file: '//TRIM(output_file)//' exists'
      WRITE(*,*) 'Ok to overwrite (y/n)?'
      READ(5,'(a)') y
      IF ( y .ne. 'y' .AND. y .ne. 'Y' ) STOP
   ENDIF
   !
   ! Obtain grid size information from interp_file
   !
   CALL ncgetsize
   !
   ! Allocate array spaces
   !
   ALLOCATE(src(nl), STAT=status)
   IF(status /= 0 ) CALL alloc_err('src')
   ALLOCATE(dst(nl), STAT=status)
   IF(status /= 0 ) CALL alloc_err('dst')
   ALLOCATE(wgt(nw,nl), STAT=status)
   IF(status /= 0 ) CALL alloc_err('wgt')
   ALLOCATE(nei(dx,dy), STAT=status)
   IF(status /= 0 ) CALL alloc_err('nei')
   !
   ! Read all required data from interp_file
   !
   CALL ncgetfields

   ! find the number of neibourhs used for the interpolation
   nei(:,:) = 0
   DO n = 1,nl
      i = MOD( dst(n)-1 , dx) + 1
      j =     (dst(n)-1)/ dx  + 1      
      nei(i,j) = nei(i,j) + 1
   END DO
   dn = MAXVAL(nei)
   dw = dn * nw
   DEALLOCATE(nei)

   WRITE(*,*) 'Max number of weights: ', dw
   WRITE(*,*) 'Max number of neighbours: ', dn

   ALLOCATE(src1(dx,dy,dn), STAT=status)
   IF(status /= 0 ) CALL alloc_err('src1')
   ALLOCATE(wgt1(dx,dy,dw), STAT=status)
   IF(status /= 0 ) CALL alloc_err('wgt1')
   wgt1(:,:,:) = 1000.   ! flag value
   src1(:,:,:) = -1

   DO n = 1,nl
      i = MOD( dst(n)-1 , dx) + 1
      j =     (dst(n)-1)/ dx  + 1
      k = 1
      DO WHILE( wgt1(i,j,k) /= 1000. )
         k = k+1
         IF(k > dn) STOP 123
      END DO
      src1(i,j,k) = src(n)
      DO m = 1,nw
         wgt1(i,j,k+dn*(m-1)) = wgt(m,n)
      END DO

   END DO

   WHERE( wgt1 == 1000. )   wgt1 = 0.

   CALL ncputfields

   STOP
CONTAINS
   !
   !----------------------------------------------------------------------*
   SUBROUTINE ncgetsize
      !
      ! Access grid size information in interp_file and set the
      ! following integers:
      !
      !    nd = dst_grid_size
      !    ns = src_grid_size
      !    nl = num_links    
      !    nw = num_wgts     
      ! sx,sy = src_grid_dims     
      ! dx,dy = dst_grid_dims     
      !
      INTEGER idims(2)
      !
      status = nf90_open(interp_file, nf90_NoWrite, ncid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_dimid(ncid, 'dst_grid_size', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_inquire_dimension(ncid, VarId, LEN = nd)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_dimid(ncid, 'src_grid_size', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_inquire_dimension(ncid, VarId, LEN = ns)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_dimid(ncid, 'num_links', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_inquire_dimension(ncid, VarId, LEN = nl)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_dimid(ncid, 'num_wgts', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_inquire_dimension(ncid, VarId, LEN = nw)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_varid(ncid, 'src_grid_dims', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_get_var(ncid, VarId, idims, (/1/), (/2/))
      IF(status /= nf90_NoErr) CALL handle_err(status)
      sx = idims(1) ; sy = idims(2)
      !
      status = nf90_inq_varid(ncid, 'dst_grid_dims', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_get_var(ncid, VarId, idims, (/1/), (/2/))
      IF(status /= nf90_NoErr) CALL handle_err(status)
      dx = idims(1) ; dy = idims(2)
      !
      status = nf90_close(ncid)
      IF (status /= nf90_noerr) CALL handle_err(status)
      !
      WRITE(*,*) 'Detected sizes: '
      WRITE(*,*) 'dst_grid_size: ', nd
      WRITE(*,*) 'src_grid_size: ', ns
      WRITE(*,*) 'num_links    : ', nl
      WRITE(*,*) 'num_wgts     : ', nw
      WRITE(*,*) 'src_grid_dims: ', sx, ' x ', sy
      WRITE(*,*) 'dst_grid_dims: ', dx, ' x ', dy
      !
   END SUBROUTINE ncgetsize

   !----------------------------------------------------------------------*
   SUBROUTINE ncgetfields
      !
      ! Read all required data from interp_file. The data read are:
      !
      ! netcdf variable    size   internal array
      !-----------------+-------+--------------
      ! src_address        nl     src
      ! dst_address        nl     dst
      ! remap_matrix     (nw,nl)  wgt
      !
      status = nf90_open(interp_file, nf90_NoWrite, ncid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_varid(ncid, 'src_address', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! Read the values for src
      status = nf90_get_var(ncid, VarId, src)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_varid(ncid, 'dst_address', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! Read the values for dst
      status = nf90_get_var(ncid, VarId, dst)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_inq_varid(ncid, 'remap_matrix', VarId)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! Read the values for wgt
      status = nf90_get_var(ncid, VarId, wgt)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_close(ncid)
      IF (status /= nf90_noerr) CALL handle_err(status)
      !
   END SUBROUTINE ncgetfields
   !                                                                       
   !----------------------------------------------------------------------*
   SUBROUTINE ncputfields
      !
      ! Write out each set of 2D fields to output_file.
      ! Each call will write out a set of srcXX and wgtXX fields.
      !
      INTEGER :: status, ncid, ncin
      INTEGER :: Lontdid, Lattdid
      INTEGER,DIMENSION(dn) :: sid
      INTEGER,DIMENSION(dw) :: wid
      INTEGER :: ioldfill
      CHARACTER(LEN=2) :: cs
      !
      ! Create output_file and set the dimensions
      !
      WRITE(*,*) 'Create file: ', TRIM(output_file)
      status = nf90_create(output_file, IOR( NF90_64BIT_OFFSET, NF90_CLOBBER ), ncid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_set_fill(ncid, nf90_NoFill, ioldfill)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_def_dim(ncid, "lon", dx, Lontdid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      status = nf90_def_dim(ncid, "lat", dy, Lattdid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_put_att(ncid, nf90_global, 'ew_wrap', ew_wrap)
      IF(status /= nf90_NoErr) CALL handle_err(status)

      !
      ! --     Reopen interp_file and transfer some global attributes
      !
      status = nf90_open(interp_file, nf90_NoWrite, ncin)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_copy_att(ncin,NF90_GLOBAL,'title',        ncid,NF90_GLOBAL)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_copy_att(ncin,NF90_GLOBAL,'normalization',ncid,NF90_GLOBAL)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_copy_att(ncin,NF90_GLOBAL,'map_method',   ncid,NF90_GLOBAL)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_copy_att(ncin,NF90_GLOBAL,'conventions',  ncid,NF90_GLOBAL)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      status = nf90_copy_att(ncin,NF90_GLOBAL,'history',      ncid,NF90_GLOBAL)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! --     Close interp_file
      !
      status = nf90_close(ncin)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! Define new variables
      !
      DO k = 1,dn
         WRITE(cs,'(i2.2)') k
         status = nf90_def_var(ncid, "src"//cs, nf90_int, &
            (/ Lontdid, Lattdid /), sid(k))
         IF(status /= nf90_NoErr) CALL handle_err(status)
      END DO
      DO k = 1,dw
         WRITE(cs,'(i2.2)') k
         status = nf90_def_var(ncid, "wgt"//cs, nf90_double, &
            (/ Lontdid, Lattdid /), wid(k))
         IF(status /= nf90_NoErr) CALL handle_err(status)
      END DO
      !
      ! Leave define mode
      !
      status = nf90_enddef(ncid)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !
      ! Write the data
      !
      DO k = 1,dn
         status = nf90_put_var(ncid, sid(k), src1(:,:,k))
         IF(status /= nf90_NoErr) CALL handle_err(status)
      END DO
      DO k = 1,dw
         status = nf90_put_var(ncid, wid(k), wgt1(:,:,k))
         IF(status /= nf90_NoErr) CALL handle_err(status)
      END DO
      !
      ! --     Close output_file
      !
      status = nf90_close(ncid)
      IF(status /= nf90_NoErr) CALL handle_err(status)


   END SUBROUTINE ncputfields

   !----------------------------------------------------------------------*
   SUBROUTINE handle_err(status)
      !
      ! Simple netcdf error checking routine
      !
      INTEGER, intent ( in) :: status
      !
      IF(status /= nf90_noerr) THEN
         IF(trim(nf90_strerror(status)) .eq. 'Attribute not found') THEN
            ! ignore
         ELSE
            WRITE(*,*) trim(nf90_strerror(status))
            STOP "Stopped"
         END IF
      END IF
   END SUBROUTINE handle_err

   !----------------------------------------------------------------------*
   SUBROUTINE alloc_err(arname)
      !
      ! Simple allocation error checking routine
      !
      CHARACTER(LEN=*) :: arname
      !
      WRITE(*,*) 'Allocation error attempting to ALLOCATE '//arname
      STOP "Stopped"
   END SUBROUTINE alloc_err

END PROGRAM scripshape
