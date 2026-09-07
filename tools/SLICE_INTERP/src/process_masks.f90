MODULE process_masks
!
!
! Purpose
! =======
!
!  Read mask and areas variables
!

  USE netcdf
  use declaration
  use npf
  use netcdferr

  IMPLICIT NONE
  PUBLIC

  CONTAINS

   SUBROUTINE allocate_arrays
!
!   Since 4.2, model global domain and input dimension differ
!   We calculate mdim (model dimension) with idim (input dimensions)
!   
!   Read masks 
!
    ALLOCATE(bmask_t( 1-hls:xt+hls, 1-hls:yt+hls, zd, 1))
    ALLOCATE(bmask_s( 1-hls:xs+hls, 1-hls:ys+hls, zd, 1))
    ALLOCATE(check_s( 1-hls:xs+hls, 1-hls:ys+hls, zd, 1))
    
!    Assuming horizontal scale factor type (double) and dimensions (see mask)
    ALLOCATE(hsf_r  (1-hls:xt+hls, 1-hls:yt+hls, 1))
    ALLOCATE(hsf_t  (1-hls:xt+hls, 1-hls:yt+hls   ))
    ALLOCATE(addr_t (1-hls:xt+hls, 1-hls:yt+hls   ))
!

   END SUBROUTINE allocate_arrays
!
   SUBROUTINE deallocate_arrays
      DEALLOCATE (bmask_t)
      DEALLOCATE (bmask_s)
      DEALLOCATE (check_s)
      DEALLOCATE (hsf_r)
      DEALLOCATE (hsf_t)
      DEALLOCATE (addr_t)
   END SUBROUTINE deallocate_arrays

   SUBROUTINE get_masks(jm)

      INTEGER ,INTENT(in) :: jm   ! mesh vertex
      integer :: ncid_s, ncid_t, maskid_t, sfid_t, maskid_s
      integer :: ji, jj, jh
      REAL(wp) :: onepm

      !   Open mask files
      i_err = nf90_open(TRIM(sn_msk_fname_s), NF90_NOWRITE, ncid_s)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to open source file '//TRIM(sn_msk_fname_s))

      i_err = nf90_open(TRIM(sn_msk_fname_t), NF90_NOWRITE, ncid_t)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to open target file '//TRIM(sn_msk_fname_t))

      i_err = nf90_inq_varid(ncid_t, TRIM(msk_name(jm)), maskid_t)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to find target variable mask '//TRIM(msk_name(jm)))
          
      i_err = nf90_get_var(ncid_t, maskid_t, bmask_t(1:xt,1:yt,1:zd,:))
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get target grid mask variable '//TRIM(msk_name(jm)))
          
      i_err = nf90_inq_varid(ncid_s, TRIM(msk_name(jm)), maskid_s)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to find source variable mask '//TRIM(msk_name(jm)))

      i_err = nf90_get_var(ncid_s, maskid_s, bmask_s(1:xs,1:ys,1:zd,:))
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get source grid mask variable')

      WRITE (kout,*) ' Grid mask variables ready'
      CALL FLUSH(kout)

      i_err = nf90_close(ncid_s)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Impossible to close mask file '//TRIM(sn_msk_fname_s))

!
!     Read scale factors
!
      i_err = nf90_inq_varid(ncid_t, TRIM(e1_name_t(jm)), sfid_t)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get fine grid scale factor id with name '//TRIM(e1_name_t(jm)))

      i_err = nf90_get_var(ncid_t, sfid_t, hsf_r(1:xt,1:yt,1))

      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get fine grid e1 scale factor named '//TRIM(e1_name_t(jm)))

      hsf_t(:,:) = hsf_r(:,:,1)

      i_err = nf90_inq_varid(ncid_t, TRIM(e2_name_t(jm)), sfid_t)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get fine grid scale factor id with name '//TRIM(e2_name_t(jm)))

      i_err = nf90_get_var(ncid_t, sfid_t, hsf_r(1:xt,1:yt,1))
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Unable to get fine grid e2 scale factor named '//TRIM(e2_name_t(jm)))

      i_err = nf90_close(ncid_t)
      IF ( i_err .ne. NF90_NOERR ) &
         CALL stop_netcdf(i_err,' Impossible to close mask file '//TRIM(sn_msk_fname_t))

      hsf_t(:,:) = hsf_r(:,:,1) * hsf_t(:,:)

      WRITE (kout,*) ' Fine grid scale factors read from files for vertex ', jm
      CALL FLUSH(kout)

      DO jj = 1, yt
         DO ji = 1, xt
            ! fill source address array (to define North pole folding
            ! addresses)
            addr_t(ji, jj) = ji + ( jj - 1 ) * xt
         ENDDO
      ENDDO

      !  Periodic conditions and NPF
      !
      !  Since 4.2 version, duplicated columns and rows are not yet included in the file
      !  We then need to mimic lbc_lnk ...

      IF ( gtarget%iperio ) THEN

         ! Duplicate column
         DO jh = 1, hls
            hsf_t(1-jh,:) = hsf_t(xt,:)
            hsf_t(xt+jh,:) = hsf_t(jh,:)

            bmask_t(1-jh,:,:,:) = bmask_t(xt,:,:,:)
            bmask_t(xt+jh,:,:,:) = bmask_t(jh,:,:,:)

            !bmask_s(1-jh,:,:,:) = bmask_t(xs,:,:,:)
            !bmask_s(xs+jh,:,:,:) = bmask_t(jh,:,:,:)

            addr_t(1-jh,:) = addr_t(xt,:)
            addr_t(xt+jh,:) = addr_t(jh,:)
         ENDDO

      ENDIF

      IF ( gtarget%pivot /= '-' ) THEN

         ! North pole folding, here we go
         onepm = 1.
         if ( jm == 2 .OR. jm == 3 ) onepm = -1.

         call lbc_nfd_ext_double( hsf_t,            gtype(jm), onepm, hls-1, xt+2*hls, yt+2*hls,     gtarget%pivot)
         call lbc_nfd_ext_double( addr_t,           gtype(jm), 1.,    hls-1, xt+2*hls, yt+2*hls,     gtarget%pivot)
         call lbc_nfd_ext_byte  ( bmask_t(:,:,:,1), gtype(jm), 1.,    hls-1, xt+2*hls, yt+2*hls, zd, gtarget%pivot)
         !call lbc_nfd_ext_byte  ( bmask_s(:,:,:,1), gtype(jm), 1., hls-1, xs+2*hls, &
         !                         ys+2*hls, zd, gsource%pivot)

      ENDIF

   END SUBROUTINE get_masks

END MODULE

