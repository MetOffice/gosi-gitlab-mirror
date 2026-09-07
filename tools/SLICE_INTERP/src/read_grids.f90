MODULE read_grid
!
! Purpose
! =======
!
!    Read meshmask to define grid properties
!    and array dimensions
!
  USE netcdf
  use declaration
  use netcdferr

  IMPLICIT NONE
  PUBLIC

  CONTAINS

!
   SUBROUTINE read_grid_info

   integer :: varid, ncid, ndims, perio_id
   integer :: dimids(NF90_MAX_VAR_DIMS)
   integer :: i, isg, i_arg, iperio
   character :: catt 
   REAL(wp) ::  zperio  

   ! loop over source/target grids
   DO isg = 1, 2

!
!     Open mask files
!
      IF ( isg == 1 ) THEN
         smsk_fname = TRIM ( sn_msk_fname_s )
      ELSE
         smsk_fname = TRIM ( sn_msk_fname_t )
      ENDIF

      i_err = nf90_open(TRIM(smsk_fname), NF90_NOWRITE, ncid)
      IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Unable to open file '//TRIM(smsk_fname))

      ! Mask files must include a tmask variable
      i_err = nf90_inq_varid(ncid, 'tmask', varid)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to find mandatory tmask variable in file: '//TRIM(smsk_fname))

      ! tmask variable must be 3 dimensional
      i_err = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to find tmask dimensions ')
      !
      ! Fill appropriate structure with grid dimensions
      !
      DO i=1, 3
         i_err = nf90_inquire_dimension(ncid, dimids(i), len=st_grid(isg)%ndim(i))
         IF ( i_err .ne. NF90_NOERR ) &
            CALL stop_netcdf(i_err,' Unable to get tmask dimension length', &
                                   ' ? Is tmask tri-dimensioned in file: '//TRIM(smsk_fname))
      ENDDO
      !
      !   Read NEMO periodicity (following domain.F90)
      !
      i_err = nf90_inq_varid(ncid, 'jperio', perio_id)
      IF ( i_err .ne. NF90_NOERR ) THEN
         ! not NEMO old format - check whether NEMO new format : test global attribute
         i_err = nf90_get_att(ncid, NF90_GLOBAL, "Iperio", i_arg)
         ! Iperio present - NEMO grid
         IF ( i_err .eq. NF90_NOERR ) THEN
            st_grid(isg)%iperio = ( i_arg == 1 )
            i_err = nf90_get_att(ncid, NF90_GLOBAL, "Jperio", i_arg)
            IF ( i_err .ne. NF90_NOERR ) &
               CALL stop_netcdf(i_err,' Unable to get Jperio id ')
            st_grid(isg)%jperio = ( i_arg == 1 )
            i_err = nf90_get_att(ncid, NF90_GLOBAL, "NFtype", catt)
            IF ( i_err .ne. NF90_NOERR ) &
               CALL stop_netcdf(i_err,' Unable to get NFtyped ')

            IF( LEN_TRIM(catt) == 1 ) THEN   ;   st_grid(isg)%pivot = TRIM(catt)
            ELSE                             ;   st_grid(isg)%pivot = '-'
            ENDIF
         ! neither old or new NEMO format - random grid
         ELSE
            WRITE (kout,*) ' Unable to get a NEMO periodicity in meshmask'
            WRITE (kout,*) ' We guess we are using a random grid in :', TRIM(smsk_fname)
            WRITE (kout,*) ' Please use another NEMO tool for interpolation'
            CALL FLUSH(kout)
            STOP
         ENDIF
      ! NEMO periodicity old format
      ELSE
         i_err = nf90_get_var(ncid, perio_id, zperio)
         IF ( i_err .ne. NF90_NOERR ) THEN
            CALL stop_netcdf(i_err,' Old NEMO format but unable to get jperio variable ')
         ELSE
            iperio = NINT( zperio )
            st_grid(isg)%iperio = ( iperio == 1 .OR. iperio == 4 .OR. iperio == 6 .OR. iperio == 7 ) ! i-periodicity
            st_grid(isg)%jperio = ( iperio == 2 .OR. iperio == 7 )                                   ! j-periodicity
            IF ( iperio == 3 .OR. iperio == 4 ) THEN
               st_grid(isg)%pivot = 'T'             !    folding at T point
            ELSEIF( iperio == 5 .OR. iperio == 6 ) THEN
               st_grid(isg)%pivot = 'F'             !    folding at F point
            ELSE
               st_grid(isg)%pivot = '-'             !    default value
            ENDIF
         ENDIF
      ENDIF


      ! Get grid 3D variables
      i_err = nf90_close(ncid)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Impossible to close mesh file '//TRIM(smsk_fname))

   ENDDO

   ! more readable structure names
   gsource = st_grid(1)
   gtarget = st_grid(2)

   ! Vertical level checking
   IF ( nn_lvlcut >= 1 ) THEN
      ! Coupling upper levels : both meshmask z dim must be bigger
      IF ( gsource%ndim(3) < nn_lvlcut .OR. gtarget%ndim(3) < nn_lvlcut ) THEN
         WRITE (kout,*) ' Cutting level number nn_lvlcut must be smaller than source and target grid vertical levels #'
         CALL FLUSH(kout)
         STOP
      ENDIF
      ! Shorter variable name
      zd = nn_lvlcut
   ELSE
      ! Coupling the whole columns : meshmask vertical dimensions must be identical
      IF ( gsource%ndim(3) /= gtarget%ndim(3) ) THEN
         WRITE (kout,*) ' Source and target grid must have the same number of vertical levels '
         CALL FLUSH(kout)
         STOP
      ENDIF
      ! Shorter variable name
      zd = gsource%ndim(3)
   ENDIF

   ! easier to handle zoom starting position
   illid(1) = nn_lli
   illid(2) = nn_llj

   END SUBROUTINE read_grid_info

END MODULE
