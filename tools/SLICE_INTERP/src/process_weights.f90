MODULE process_weights
!
!
! Purpose
! =======
!
!  dimension_weights :  - calculate how much source-target association are needed 
!                       - check that the required transform can be done with the number of grid point read
!  compute_weights   :  - set weight for each source-target association
!  save_weights      :  - save the OASIS format weight&adress netCDF files
!
  USE netcdf
  use declaration
  use npf
  use netcdferr

  IMPLICIT NONE
  PUBLIC

    INTEGER :: jimin, jimax, jjmin, jjmax, ishf, jshf
    INTEGER :: nb_link_ptc_2d
    INTEGER :: out_ncid, out_varid(6)

    INTEGER, ALLOCATABLE :: fin_address(:), crs_address(:)
    INTEGER, ALLOCATABLE :: patch_matrix(:)

    REAL(wp), ALLOCATABLE :: remap_matrix(:), remap_matrix_unif(:)
    REAL(wp), ALLOCATABLE :: remap_matrix_area(:)

  CONTAINS

!
   SUBROUTINE dimension_weights(jm)

      INTEGER ,INTENT(in) :: jm   ! mesh vertex
      INTEGER :: demic
      INTEGER :: ji, jj, jk, tmpj
      INTEGER :: jimax_t, jimin_t, jjmax_t, jjmin_t
!
    !  Calculate w-a array dimensions
    !
      nb_link = 0
      nb_link_ptc = 0
      !bmask_s(:,:,:,:) = 0
      demic=0
      check_s(:,:,:,:) = 0

      IF ( crsf == 1 .AND. gtarget%pivot /= '-' ) demic=1

      ! calculate source starting indexes
      ! EM : a revoir lors du ICB Antarctic : illid(2) devra etre egal a 2
      IF ( ln_zoom ) THEN
         jimin = illid(1)
         jimax = illid(1) - 1 + ( xt - 2 ) / crsf
         jjmin = illid(2)
         jjmax = illid(2) - 1 + ( yt - 2 ) / crsf
      ELSE
         ! non periodic case (lateral wall)
         jimin = 2 ; jjmin = 2 ; jimax =  xs - 2 ; jjmax = ys - 2
         IF ( gsource%iperio ) THEN
            jimin = 1 ; jimax = xs
         ENDIF
         IF ( gsource%jperio .AND. gsource%pivot == '-' ) THEN
            jjmin = 1 ; jjmax = ys
         ENDIF
         IF ( gsource%pivot /= '-' ) THEN
            jjmin = 2; jjmax = ys
         ENDIF
      ENDIF

      ! calculate target starting shifts
      IF ( ln_zoom ) THEN
         ishf = 1 ; jshf = 1
      ELSE
         ishf = 1 ; jshf = 1
         IF ( gtarget%iperio ) &
            ishf = 0
         IF ( gtarget%jperio .AND. gtarget%pivot == '-' ) &
            jshf = 0
         IF ( gtarget%pivot /= '-' ) &
            jshf = 1
      ENDIF

      WRITE(kout,*) ' Warning for following grid points :'
      WRITE(kout,*) ' Mesh mask and calculated mask mismatch'

      DO jk = 1, zd-1

         IF ( gtarget%pivot == 'T' .AND. .NOT. ln_zoom ) THEN
            ! Special patch to take into account the 8 isolated grid
            ! points of the first line of the orca025 grid
            ! - inverse coarsening only
            WRITE(kout,*) ' '
            WRITE(kout,*) ' Warning : special treatment for T pivot grid coarsening '
            WRITE(kout,*) ' '
            nb_link_ptc = nb_link_ptc + SUM(bmask_t(:,1,jk,1))
         ENDIF

         DO jj = jjmin, jjmax

            ! EM attention, vrai pour pivot T seulement ?
            tmpj = jimax
            IF ( gtarget%pivot == 'T' .AND. jj == ys ) &
               jimax = xs / 2 + 1
            
            jjmin_t = (jj-jjmin)*crsf+1+jshf
            jjmax_t = jjmin_t + (crsf-1)

            ! Reduce target to the n upper points (for V transport or F)
            IF ( ( ln_transport .AND. jm == 3 ) .OR. jm == 4 ) jjmin_t = jjmax_t

            DO ji = jimin, jimax

               ! redefine source mask
               ! depending on grid type
               jimin_t = (ji-jimin)*crsf+1+ishf+demic
               jimax_t = jimin_t + (crsf-1)

               ! Reduce target to n lateral points (for U transport or F)
               IF ( ( ln_transport .AND. jm == 2 ) .OR. jm == 4 ) jimin_t = jimax_t

               ! At this point, it would be good to compare the meshmask values
               ! and calculated values
               IF ( bmask_s(ji,jj,jk,1) /= &
                     MIN(1,SUM(MIN(bmask_t(jimin_t:jimax_t,jjmin_t:jjmax_t,jk,1),1) )) ) THEN
                  WRITE(kout,"(A,I5,1X,I5,1X,I5,1X,I1)") 'Source i, j,k, mask: ',ji,jj,jk,bmask_s(ji,jj,jk,1)
                  WRITE(kout,*) ' and ', jimin_t,jimax_t,jjmin_t,jjmax_t
                  check_s(ji,jj,jk,1) = 1
               ENDIF

               ! We fill source mask with matching target nb
               ! Warning: fmask can be 1 or 2
               bmask_s(ji,jj,jk,1) = &
                     SUM(MIN(bmask_t(jimin_t:jimax_t,jjmin_t:jjmax_t,jk,1),1) )

               IF ( jm == 5 .AND. jk > 1 ) &
                  bmask_s(ji,jj,jk,1) = bmask_s(ji,jj,jk  ,1) * &
                                        bmask_s(ji,jj,jk-1,1)

               ! unmasked source
               IF ( bmask_s(ji,jj,jk,1) >= 1 ) THEN
                  nb_link = nb_link + bmask_s(ji,jj,jk,1)
                  bmask_s(ji,jj,jk,1) = 1
               ENDIF

            ENDDO
            jimax = tmpj

         ENDDO
         IF ( jk == 1 ) nb_link_2D = nb_link
      ENDDO

      WRITE (kout,*) ' 2D links needed : ', nb_link_2D
      WRITE (kout,*) ' 3D links needed : ', nb_link
      CALL FLUSH(kout)

   END SUBROUTINE dimension_weights

   SUBROUTINE compute_weights(jm)

      INTEGER ,INTENT(in) :: jm   ! mesh vertex

      INTEGER :: ji, jj, jk, jh, tmpj, locw, jsi, jsj, demic
      INTEGER :: jimax_t, jimin_t, jjmax_t, jjmin_t

      REAL(wp), ALLOCATABLE :: hsf_s(:,:)
      REAL(wp) :: area, area_t

      CALL allocate_weights

      ALLOCATE(hsf_s(1-hls:xs+hls,1-hls:ys+hls))
      hsf_s=0.

!
!     Calculate w-a
!
      nb_link = 1
      nb_link_ptc = 0
      nb_link_ptc_2d = 0

      demic=0

      IF ( crsf == 1 .AND. gtarget%pivot /= '-' ) demic=1

      DO jk = 1, zd-1

         ! Special patch to take into account the 8 isolated grid
         ! points of the first line of the orca025 grid
         ! - inverse coarsening only
         IF ( jm == 1 .AND. gtarget%pivot == 'T' .AND. .NOT. ln_zoom ) THEN  ! T target pivot identify ORCA025 so far
            DO ji = jimin, jimax
               IF ( bmask_s(ji,1,jk,1) >= 1 ) THEN
                  nb_link_ptc = nb_link_ptc + 1
                  patch_matrix(nb_link_ptc) = addr_t (ji,1) + &
                                         ( jk - 1 ) * xt * yt
               ENDIF
            ENDDO
         ENDIF

         DO jj = jjmin, jjmax

            tmpj = jimax
            ! Remove half duplicated line 
            ! EM: May be missing: test if this line is involved or not
            IF ( gsource%pivot == 'T' .AND. jj == ys ) &
               jimax = xs / 2 + 1

            jjmin_t = (jj-jjmin)*crsf+1+jshf
            jjmax_t = jjmin_t + (crsf-1)

            ! Reduce target to the n upper points (for V transport or F)
            IF ( ( ln_transport .AND. jm == 3 ) .OR. jm == 4 ) jjmin_t = jjmax_t

            DO ji = jimin, jimax

               ! unmasked target
               IF ( bmask_s(ji,jj,jk,1) == 1 ) THEN
                  jimin_t = (ji-jimin)*crsf+1+ishf+demic
                  jimax_t = jimin_t + (crsf-1)

                  ! Reduce target to n lateral points (for U transport or F)
                  IF ( ( ln_transport .AND. jm == 2 ) .OR. jm == 4 ) jimin_t = jimax_t

                  locw = 0

                  DO jsj = jjmin_t,jjmax_t; DO jsi = jimin_t,jimax_t
                     ! Warning: fmask can be 1 or 2
                     IF ( bmask_t(jsi,jsj,jk,1) >= 1 ) THEN
                        fin_address(nb_link+locw)  = addr_t (jsi,jsj) + &
                                                     ( jk - 1 ) * xt * yt

                        ! Caution: negative weight in NPole folding area if U or V grid
                        remap_matrix(nb_link+locw) = SIGN(1.,hsf_t(jsi,jsj))

                        remap_matrix_area(nb_link+locw) = hsf_t(jsi,jsj)
                        locw = locw + 1
                        IF ( bmask_t(jsi,jsj,jk,1) == 2 ) bmask_s(ji,jj,jk,1) = 2
                     ENDIF
                  ENDDO; ENDDO

                  IF ( locw == 0 ) THEN
                     write (kout,*) ' WARNING :: Unmasked coarse grid point without any source : ', ji, jj, jk
                  ELSE
                      crs_address(nb_link:nb_link+locw-1)      = ji + ( jj - 1 ) * xs + &
                                                                 ( jk - 1 ) * xs * ys

                      IF ( jk == 1 ) hsf_s(ji,jj) = SUM(remap_matrix_area(nb_link:nb_link+locw-1))
                      remap_matrix_area(nb_link:nb_link+locw-1) = remap_matrix_area(nb_link:nb_link+locw-1) / ABS(hsf_s(ji,jj))
                      remap_matrix_unif(nb_link:nb_link+locw-1) = REAL(1. / locw)

                      nb_link = nb_link + locw

                  ENDIF

               ENDIF

            ENDDO
            ! reset upper bound of line
            jimax = tmpj
         ENDDO

         IF ( jk == 1 ) nb_link_ptc_2d = nb_link_ptc
         WRITE (kout,*) ' Complete level : ', jk
         CALL FLUSH(kout)

      ENDDO
      ! Remark: only T grid conservativeness is checked
      IF ( jm == 1 ) THEN
         area = SUM(hsf_t(1:xt,2:yt-1)*bmask_t(1:xt,2:yt-1,1,1))
         SELECT CASE ( gtarget%pivot ) 
            CASE ('T')
               area = area + SUM(hsf_t(1:xt/2,yt)*bmask_t(1:xt/2,yt,1,1))
            CASE ('F')
               WRITE(kout,*) 'check area for F pivot, we stop'
               STOP
              ! need further investigation
            CASE ('-')
               area = area + SUM(hsf_t(1:xt/2, 1)*bmask_t(1:xt/2, 1,1,1)) + &
                             SUM(hsf_t(1:xt/2,yt)*bmask_t(1:xt/2,yt,1,1))
         END SELECT
         area_t = area
         WRITE(kout,*) ' Total target areas ', area_t
         area = SUM(hsf_s(1:xs,2:ys-1))
         SELECT CASE ( gsource%pivot ) 
            CASE ('T')
               area = area + SUM(hsf_s(1:xs/2,ys))
            CASE ('F')
               WRITE(kout,*) 'check area for F pivot, we stop'
               STOP
              ! need further investigation
            CASE ('-')
               area = area + SUM(hsf_s(1:xs/2, 1)) + SUM(hsf_s(1:xs/2,ys))
         END SELECT
         WRITE(kout,*) ' Total source areas ', area
         area = ABS ( area - area_t )
         IF ( area > 1.e-20_wp ) THEN
            WRITE (kout,*) ' Total source and target grid areas differ ', area; CALL FLUSH(kout); STOP
         ENDIF
      ENDIF

      DEALLOCATE(hsf_s)

      ! Check source address validity
      IF ( MAXVAL(fin_address(1:nb_link_2D)) > ( xt * yt ) ) THEN
         WRITE (kout,*) ' Source grid address badly defined'; CALL FLUSH(kout); STOP
      ENDIF

      nb_link = nb_link - 1

      IF ( gsource%pivot /= '-' ) THEN
         ! Duplicate column
         DO jh = 1, hls
            bmask_s(1-jh,:,:,:) = bmask_s(xs,:,:,:)
            bmask_s(xs+jh,:,:,:) = bmask_s(jh,:,:,:)
         ENDDO
         CALL lbc_nfd_ext_byte  ( bmask_s(:,:,:,1), gtype(jm), 1., hls-1, xs+2*hls, ys+2*hls, zd, gsource%pivot)
      ENDIF

   END SUBROUTINE compute_weights


   SUBROUTINE save_weights(jm)

      INTEGER ,INTENT(in) :: jm   ! mesh vertex

      CHARACTER(LEN=nf90_max_name) :: outfilename
      CHARACTER(LEN=100) :: outmess
      LOGICAL :: l_2d
      INTEGER :: jrmp, ji
      INTEGER :: rmp_ncid, rmp_dimid(2), rmp_varid(3), dimtmp
      INTEGER :: out_dimid(4)
      INTEGER :: nb_link_out
      INTEGER :: src_grid_size, dst_grid_size

!
!      Save w-a (OASIS format)
!

!      Volume and surface coarsening operator

       jrmp = 4
       IF ( jm == 1 .OR. ( .NOT. ln_transport .AND. jm <= 3 ) ) jrmp = 7

       DO ji = 1, jrmp

          ! General case : For all transformations, 
          !                link (and weight) exists only if unmasked target
          SELECT CASE ( ji ) 

             !       Coarsening cases
             !          grid T : get crs * crs fine grid points (excluding masked ones)
             !          grid U : get the crs esternmost fine grid points (excluding masked ones)
             !          grid V : get the crs nothernmost fine grid points (excluding masked ones)
             !          grid F : get the crs NE-most fine grid points (excluding masked ones)

             CASE (1)
                ! Case : 3D Global Coarsening or zoom to parent 
                !        - weight = 1 
                !                ( or -1, if NPF, for U and V grids) : NPF treatment
                !        - This is useful to postpone normalisation, in particular for variable volume
                !        - In this latter case, vertical coordinate must also be coupled
                outfilename = TRIM(rmp_fname(jm))//'3D_crs_add.nc'
                l_2d = .FALSE.
                ! Revert order of source and target to define coarsening related transformation
                src_grid_size = xt*yt*zd
                dst_grid_size = xs*ys*zd
             CASE (2)
                ! Case : 3D Global Coarsening or zoom to parent
                !        - weights = 1 / unmasked grid contributions nb
                !        - no NPF treatment
                !        - useful to send vertical volumes
                outfilename = TRIM(rmp_fname(jm))//'3D_crs_uni.nc'
                l_2d = .FALSE.
                ! Revert order of source and target to define coarsening related transformation
                src_grid_size = xt*yt*zd
                dst_grid_size = xs*ys*zd
             CASE (3)
                ! Case : 3D Global Coarsening or zoom to parent
                !        - weight = e1*e2 / ( e1*e2 sum) -  normalised (per cent)
                !        - NPF treatment : can be negative
                outfilename = TRIM(rmp_fname(jm))//'3D_crs_srf.nc'
                l_2d = .FALSE.
                ! Revert order of source and target to define coarsening related transformation
                src_grid_size = xt*yt*zd
                dst_grid_size = xs*ys*zd
             CASE (4)
                ! Case : 2D Global Coarsening or zoom to parent
                !        - weight = e1*e2 / ( e1*e2 sum) -  normalised (per cent)
                !        - NPF treatment : can be negative
                outfilename = TRIM(rmp_fname(jm))//'2D_crs_srf.nc'
                l_2d = .TRUE.
                ! Revert order of source and target to define coarsening related transformation
                src_grid_size = xt*yt
                dst_grid_size = xs*ys
             CASE (5)
                ! Case : 2D Global Coarsening or zoom to parent
                !        - weights = 1 / unmasked grid contributions nb
                !        - Only for T grids - no NPF tratment
                outfilename = TRIM(rmp_fname(jm))//'2D_crs_uni.nc'
                l_2d = .TRUE.
                ! Revert order of source and target to define coarsening related transformation
                src_grid_size = xt*yt
                dst_grid_size = xs*ys

             !       Refinement cases
             !         grid T  : fill crs * crs fine grid points (excluding masked ones)

             CASE (6)
                ! Case : 3D Global Refinement or parent to zoom
                !        - weights = 1 - every fine grid point receive the same coarse value
                !        - Only for T grids - no NPF treatment
                !        - no global refinement to pivot global grid so far
                !        - no zoom on NPF zone so far
                outfilename = TRIM(rmp_fname(jm))//'3D_ref_add.nc'
                l_2d = .FALSE.
                nb_link_out = nb_link_out + nb_link_ptc
                ! Keep order of source and target to define refinement related transformation
                src_grid_size = xs*ys*zd
                dst_grid_size = xt*yt*zd
             CASE (7)
                ! Case : 2D Global Refinement or parent to zoom
                !        - weights = 1 - every fine grid point receive the same coarse value
                outfilename = TRIM(rmp_fname(jm))//'2D_ref_add.nc'
                l_2d = .TRUE.
                nb_link_ptc =  nb_link_ptc_2d
                nb_link_out = nb_link_out + nb_link_ptc
                ! Keep order of source and target to define refinement related transformation
                src_grid_size = xs*ys
                dst_grid_size = xt*yt
          END SELECT

          IF ( l_2d ) THEN
             nb_link_out = nb_link_2D
          ELSE
             nb_link_out = nb_link
          ENDIF

          i_err = nf90_create(path = TRIM(outfilename), cmode = or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid = rmp_ncid)
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Cannot create rmp file ')

          i_err = nf90_def_dim(rmp_ncid, 'num_wgts', 1, rmp_dimid(1))
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined first dim ')

          i_err = nf90_def_dim(rmp_ncid, 'num_links', nb_link_out, rmp_dimid(2))
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined second dim ')

          i_err = nf90_def_dim(rmp_ncid, 'src_grid_size', src_grid_size, dimtmp)
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined source grid size dim ')

          i_err = nf90_def_dim(rmp_ncid, 'dst_grid_size', dst_grid_size, dimtmp)
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined target grid size dim ')

          i_err = nf90_def_var(rmp_ncid, 'src_address', NF90_INT, dimids=rmp_dimid(2), varid=rmp_varid(1))
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined src_address variable ')

          i_err = nf90_def_var(rmp_ncid, 'dst_address', NF90_INT, dimids=rmp_dimid(2), varid=rmp_varid(2))
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined dst_address variable ')

          i_err = nf90_def_var(rmp_ncid, 'remap_matrix', NF90_DOUBLE, dimids=rmp_dimid(1:2), varid=rmp_varid(3))
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Undefined remap_matrix variable ')

          i_err = nf90_enddef(rmp_ncid)
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Error in nf90_enddef ')

          IF ( ji >= 6 ) THEN
             ! Fill source adresses with finer grid
             i_err = nf90_put_var(rmp_ncid, rmp_varid(1), crs_address(1:nb_link_out-nb_link_ptc), start=(/1/), count=(/nb_link_out-nb_link_ptc/))
             ! ugly patch for first orca025 line
             IF ( jm == 1 .AND. gtarget%pivot == 'T' .AND. .NOT. ln_zoom ) &
                i_err = nf90_put_var(rmp_ncid, rmp_varid(1), patch_matrix(1:nb_link_ptc)*0.+1., start=(/nb_link_out-nb_link_ptc+1/), COUNT=(/nb_link_ptc/))
          ELSE
             i_err = nf90_put_var(rmp_ncid, rmp_varid(1), fin_address(1:nb_link_out))
          ENDIF
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to write source address ')

          IF ( ji >= 6 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(2), fin_address(1:nb_link_out-nb_link_ptc), start=(/1/), COUNT=(/nb_link_out-nb_link_ptc/))
             ! ugly patch for first orca025 line
             IF ( jm == 1 .AND. gtarget%pivot == 'T' .AND. .NOT. ln_zoom ) &
                i_err = nf90_put_var(rmp_ncid, rmp_varid(2), patch_matrix(1:nb_link_ptc), start=(/nb_link_out-nb_link_ptc+1/), count=(/nb_link_ptc/))
          ELSE
             i_err = nf90_put_var(rmp_ncid, rmp_varid(2), crs_address(1:nb_link_out))
          ENDIF
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to write destination address ')

          IF ( ji == 1 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix,(/1,nb_link/)))
          ELSEIF ( ji == 2 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix_unif,(/1,nb_link/)))
          ELSEIF ( ji == 3 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix_area,(/1,nb_link/)))
          ELSEIF ( ji == 4 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix_area,(/1,nb_link_2D/)))
          ELSEIF ( ji == 5 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix_area*0.+1.,(/1,nb_link_2D/)))
          ELSEIF ( ji >= 6 ) THEN
             i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix*0+1.,(/1,nb_link_out-nb_link_ptc/)), start=(/1,1/), count=(/1,nb_link_out-nb_link_ptc/))
             IF ( jm == 1 .AND. gtarget%pivot == 'T' .AND. .NOT. ln_zoom ) &
                i_err = nf90_put_var(rmp_ncid, rmp_varid(3), RESHAPE(remap_matrix*0,(/1,nb_link_ptc/)), start=(/1,nb_link_out-nb_link_ptc+1/), count=(/1,nb_link_ptc/))
          ENDIF
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to write remap_matrix weights ')

          i_err = nf90_close(rmp_ncid)
          IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to close rmp file ')

          IF ( l_2d ) THEN; outmess='Surface' ; ELSE; outmess='Volume';ENDIF
          IF ( ji >=6 ) THEN; outmess=TRIM(outmess)//' refinement' ; ELSE; outmess=TRIM(outmess)//' coarsening';ENDIF
          WRITE (kout,*) TRIM(outmess)//' W&A saved '
          CALL FLUSH(kout)

       ENDDO

      CALL deallocate_weights

      WRITE (kout,*) ' Complete vertex : ', gtype(jm)
      CALL FLUSH(kout)

!
!     Save corrected target masks
!
      IF ( jm == 1 ) THEN

         outfilename = 'crs_msk.nc'
         i_err = nf90_create(path = TRIM(outfilename), cmode = or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid = out_ncid)
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Cannot create crs msk file ')

         i_err = nf90_def_dim(out_ncid, 'x', xs, out_dimid(1))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define first dim ')

         i_err = nf90_def_dim(out_ncid, 'y', ys, out_dimid(2))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define second dim ')

         i_err = nf90_def_dim(out_ncid, 'z', zd, out_dimid(3))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define third dim ')

         i_err = nf90_def_dim(out_ncid, 't', NF90_UNLIMITED, out_dimid(4))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define fourth dim ')

         i_err = nf90_def_var(out_ncid, 'tmask', NF90_INT, dimids=out_dimid(:), varid=out_varid(1))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define tmask variable ')

         i_err = nf90_def_var(out_ncid, 'umask', NF90_INT, dimids=out_dimid(:), varid=out_varid(2))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define umask variable ')

         i_err = nf90_def_var(out_ncid, 'vmask', NF90_INT, dimids=out_dimid(:), varid=out_varid(3))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define vmask variable ')

         i_err = nf90_def_var(out_ncid, 'fmask', NF90_INT, dimids=out_dimid(:), varid=out_varid(4))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define fmask variable ')

         i_err = nf90_def_var(out_ncid, 'checu', NF90_INT, dimids=out_dimid(:), varid=out_varid(5))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define check-u variable ')

         i_err = nf90_def_var(out_ncid, 'checv', NF90_INT, dimids=out_dimid(:), varid=out_varid(6))
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Not able to define check-v variable ')

         i_err = nf90_enddef(out_ncid)
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Error in nf90_enddef ')

      ENDIF

      IF ( jm < 5 ) THEN
         i_err = nf90_put_var(out_ncid, out_varid(jm), bmask_s(1:xs,1:ys,:,1) )
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to write source mask ')
      ENDIF

      IF ( jm == 2 .OR. jm == 3 ) THEN
         i_err = nf90_put_var(out_ncid, out_varid(jm+3), check_s(1:xs,1:ys,:,1) )
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to write check source variable ')
      ENDIF

      IF ( jm == 4 ) THEN
         i_err = nf90_close(out_ncid)
         IF ( i_err .ne. NF90_NOERR ) CALL stop_netcdf(i_err,' Impossible to close modified mask file ')

         WRITE (kout,*) ' New coarsened mask saved '
         CALL flush(kout)
      ENDIF


   END SUBROUTINE save_weights

   SUBROUTINE allocate_weights
!
!      Allocate w-a arrays
!
      ALLOCATE(fin_address(nb_link)) ! old src_address
      ALLOCATE(crs_address(nb_link)) ! old dst_address
      ALLOCATE(remap_matrix(nb_link))
      ALLOCATE(remap_matrix_unif(nb_link))
      ALLOCATE(remap_matrix_area(nb_link))
      ALLOCATE(patch_matrix(nb_link_ptc))
   END SUBROUTINE allocate_weights

   SUBROUTINE deallocate_weights
      DEALLOCATE(fin_address)
      DEALLOCATE(crs_address)
      DEALLOCATE(remap_matrix)
      DEALLOCATE(remap_matrix_unif)
      DEALLOCATE(remap_matrix_area)
      DEALLOCATE(patch_matrix)
   END SUBROUTINE deallocate_weights

END MODULE
