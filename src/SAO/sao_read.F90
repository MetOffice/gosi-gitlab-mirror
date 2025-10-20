MODULE sao_read
   !!======================================================================
   !!                      ***  MODULE sao_read  ***
   !! Read routines : I/O for Stand Alone Observation operator
   !!======================================================================
   USE mppini
   USE lib_mpp
   USE in_out_manager
   USE par_kind, ONLY: lc
   USE netcdf
   USE oce,     ONLY: ts, ssh, uu, vv, rhd, rhop   ! Ocean arrays
#if defined key_si3
   USE ice,     ONLY: at_i, vt_i, vt_s             ! Sea-ice arrays
#endif
   USE eosbn2,  ONLY: eos                          ! Equation of state
   USE dom_oce, ONLY: nimpp, njmpp, tmask
   USE dom_oce, ONLY: mig, mjg                     ! Grid-index conversion
   USE par_oce, ONLY: jpi, jpj, jpk
   !
   USE obs_fbm, ONLY: fbimdi, fbrmdi, fbsp, fbdp
   USE sao_data
   USE iom_nf90
   USE lbclnk,  ONLY: lbc_lnk                      ! Lateral-boundary-condition update

   IMPLICIT NONE
   PRIVATE

   PUBLIC sao_rea_dri

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sao_rea_dri( Kmm, kfile )
      !!------------------------------------------------------------------------
      !!             *** sao_rea_dri ***
      !!
      !! Purpose : To choose appropriate read method
      !! Method  :
      !!
      !! Author  : A. Ryan Oct 2013
      !!
      !!------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kmm     ! Time-level index
      INTEGER, INTENT(in) ::   kfile   ! File number
      !
      CHARACTER(len=lc)   ::   cdfilename    ! File name
      INTEGER ::   kindex        ! File index to read
      !!------------------------------------------------------------------------
      !
      cdfilename = TRIM( sao_files(kfile) )
      kindex = nn_sao_idx(kfile)
      CALL sao_read_file( Kmm, TRIM( cdfilename ), kindex )
      !
   END SUBROUTINE sao_rea_dri


   SUBROUTINE sao_read_file( Kmm, filename, ifcst )
      !!------------------------------------------------------------------------
      !!                         ***  sao_read_file  ***
      !!
      !! Purpose : To fill tn and sn with dailymean field from netcdf files
      !! Method  : Use subdomain indices to create start and count matrices
      !!           for netcdf read.
      !!
      !! Author  : A. Ryan Oct 2010
      !!------------------------------------------------------------------------
      INTEGER,          INTENT(in) ::   Kmm        ! Time-level index
      INTEGER,          INTENT(in) ::   ifcst
      CHARACTER(len=*), INTENT(in) ::   filename
      INTEGER                      ::   ncid, varid, istat, ntimes
      INTEGER                      ::   tdim, xdim, ydim, zdim
      INTEGER                      ::   ii, ij, ik
      INTEGER, DIMENSION(4)        ::   start_n, count_n
      INTEGER, DIMENSION(3)        ::   start_s, count_s
      REAL(fbdp)                   ::   fill_val
      REAL(fbdp), DIMENSION(:,:,:), ALLOCATABLE ::   temp_tn, temp_sn
      REAL(fbdp), DIMENSION(:,:)  , ALLOCATABLE ::   temp_sshn
      REAL(fbdp), DIMENSION(:,:,:), ALLOCATABLE ::   ztmp_uu, ztmp_vv            ! Temporary velocity arrays
#if defined key_si3
      REAL(fbdp), DIMENSION(:,:),   ALLOCATABLE ::   ztmp_ai, ztmp_vi, ztmp_vs   ! Temporary sea-ice arrays
#endif

      ! DEBUG
      INTEGER ::   istage
      !!------------------------------------------------------------------------

      IF (TRIM(filename) == 'nofile') THEN
         ts(:,:,:,:,Kmm) = fbrmdi
         ssh(:,:,Kmm)    = fbrmdi
         rhop(:,:,:)     = fbrmdi
         uu(:,:,:,Kmm)   = fbrmdi
         vv(:,:,:,Kmm)   = fbrmdi
      ELSE
         IF(lwp) WRITE(numout,*) "Opening :", TRIM(filename)
         ! Open Netcdf file to find dimension id
         CALL iom_nf90_check( nf90_open(path=TRIM(filename), mode=nf90_nowrite, ncid=ncid),   &
            &                 TRIM( '   Could not open '//TRIM(filename) ) )
         CALL iom_nf90_check( nf90_inq_dimid(ncid,'x',xdim), '' )
         CALL iom_nf90_check( nf90_inq_dimid(ncid,'y',ydim), '' )
         CALL iom_nf90_check( nf90_inq_dimid(ncid,'deptht',zdim), '' )
         CALL iom_nf90_check( nf90_inq_dimid(ncid,'time_counter',tdim), '' )
         CALL iom_nf90_check( nf90_inquire_dimension(ncid, tdim, len=ntimes), '' )
         IF (ifcst .LE. ntimes) THEN
            ! Allocate temporary temperature array
            ALLOCATE(temp_tn(A2D(0),jpk))
            ALLOCATE(temp_sn(A2D(0),jpk))
            ALLOCATE(temp_sshn(A2D(0)))
            ALLOCATE( ztmp_uu(A2D(0),jpk), ztmp_vv(A2D(0),jpk), STAT=istat )
            IF( istat /= 0 ) CALL ctl_stop( '   SAO - sao_read_file:', '      memory allocation failed' )

            ! Set temp_tn, temp_sn to 0.
            temp_tn(:,:,:) = fbrmdi
            temp_sn(:,:,:) = fbrmdi
            temp_sshn(:,:) = fbrmdi
            ztmp_uu(:,:,:) = fbrmdi
            ztmp_vv(:,:,:) = fbrmdi

            ! Create start and count arrays
            start_n = (/ mig(Nis0,0), mjg(Njs0,0), 1,   ifcst /)
            count_n = (/ Ni_0,        Nj_0,        jpk, 1     /)
            start_s = (/ mig(Nis0,0), mjg(Njs0,0),      ifcst /)
            count_s = (/ Ni_0,        Nj_0,             1     /)

            ! Read information into temporary arrays
            ! retrieve varid and read in temperature
            istat = nf90_inq_varid(ncid,'thetao',varid)
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att(ncid, varid, '_FillValue', fill_val), '' )
               CALL iom_nf90_check( nf90_get_var(ncid, varid, temp_tn, start_n, count_n), '' )
               WHERE(temp_tn(:,:,:) == fill_val) temp_tn(:,:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'thetao' not found in file "//TRIM( filename ) )
            END IF

            ! retrieve varid and read in salinity
            istat = nf90_inq_varid(ncid,'so',varid)
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att(ncid, varid, '_FillValue', fill_val), '' )
               CALL iom_nf90_check( nf90_get_var(ncid, varid, temp_sn, start_n, count_n), '' )
               WHERE(temp_sn(:,:,:) == fill_val) temp_sn(:,:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'so' not found in file "//TRIM( filename ) )
            END IF

            ! retrieve varid and read in SSH or altimeter bias
            istat = nf90_inq_varid(ncid,'zos',varid)
            IF (istat /= nf90_noerr) istat = nf90_inq_varid(ncid,'altbias',varid)
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att(ncid, varid, '_FillValue', fill_val), '' )
               CALL iom_nf90_check( nf90_get_var(ncid, varid, temp_sshn, start_s, count_s), '' )
               WHERE(temp_sshn(:,:) == fill_val) temp_sshn(:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      neither variable 'zos' nor variable 'altbias' found in",   &
                  &           '      file '//TRIM( filename ) )
            END IF

            ! Read in horizontal velocity fields
            istat = nf90_inq_varid( ncid, 'uo', varid )
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att( ncid, varid, '_FillValue', fill_val ), '' )
               CALL iom_nf90_check( nf90_get_var( ncid, varid, ztmp_uu, start_n, count_n ), '' )
               WHERE( ztmp_uu(:,:,:) == fill_val ) ztmp_uu(:,:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'uo' not found in file "//TRIM( filename ) )
            END IF
            istat = nf90_inq_varid( ncid, 'vo', varid )
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att( ncid, varid, '_FillValue', fill_val ), '' )
               CALL iom_nf90_check( nf90_get_var( ncid, varid, ztmp_vv, start_n, count_n ), '' )
               WHERE( ztmp_vv(:,:,:) == fill_val ) ztmp_vv(:,:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'vo' not found in file "//TRIM( filename ) )
            END IF

            ! Initialise model fields to fbrmdi
            ts(:,:,:,:,Kmm) = fbrmdi
            ssh(:,:,Kmm)    = fbrmdi
            uu(:,:,:,Kmm)   = fbrmdi
            vv(:,:,:,Kmm)   = fbrmdi

            ! Mask out missing data index
            ts(A2D(0),:,1,Kmm) = temp_tn(:,:,:) * tmask(A2D(0),:)
            ts(A2D(0),:,2,Kmm) = temp_sn(:,:,:) * tmask(A2D(0),:)
            ssh(A2D(0),Kmm)    = temp_sshn(:,:) * tmask(A2D(0),1)
            uu(A2D(0),:,Kmm)   = ztmp_uu(:,:,:) * tmask(A2D(0),:)
            vv(A2D(0),:,Kmm)   = ztmp_vv(:,:,:) * tmask(A2D(0),:)

            ! Update lateral boundary
            CALL lbc_lnk( 'sao_read', ts(:,:,:,jp_tem,Kmm), 'T', 1.0_wp,    &
               &                      ts(:,:,:,jp_sal,Kmm), 'T', 1.0_wp,    &
               &                      uu(:,:,:,Kmm),        'U', -1.0_wp,   &
               &                      vv(:,:,:,Kmm),        'V', -1.0_wp,   &
               &                      rhop(:,:,:),          'T', 1.0_wp )
            CALL lbc_lnk( 'sao_read', ssh(:,:,Kmm),         'T', 1.0_wp )

            ! Compute sea-water density (used for freeboard conversion)
            CALL eos( ts(:,:,:,:,:), Kmm, rhd, rhop )
            WHERE( ts(:,:,:,jp_tem,Kmm) == fbrmdi .OR. ts(:,:,:,jp_sal,Kmm) == fbrmdi ) rhop(:,:,:) = fbrmdi

            ! Deallocate arrays
            DEALLOCATE( temp_tn, temp_sn, temp_sshn, ztmp_uu, ztmp_vv, STAT=istat )
            IF( istat /= 0 ) CALL ctl_stop( '   SAO - sao_read_file:', '      memory deallocation failed' )

#if defined key_si3
            ALLOCATE( ztmp_ai(A2D(0)), ztmp_vi(A2D(0)), ztmp_vs(A2D(0)), STAT=istat )
            IF( istat /= 0 ) CALL ctl_stop( '   SAO - sao_read_file:', '      memory allocation failed' )

            ! Initialise auxiliary arrays
            ztmp_ai(:,:) = fbrmdi
            ztmp_vi(:,:) = fbrmdi
            ztmp_vs(:,:) = fbrmdi

            ! Read in sea-ice area
            istat = nf90_inq_varid( ncid, 'siconc', varid )
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att( ncid, varid, '_FillValue', fill_val ), '' )
               CALL iom_nf90_check( nf90_get_var( ncid, varid, ztmp_ai, start_s, count_s ), '' )
               WHERE( ztmp_ai(:,:) == fill_val ) ztmp_ai(:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'at_i' not found in file "//TRIM( filename ) )
            END IF

            ! Read in sea-ice height (volume per area)
            istat = nf90_inq_varid( ncid, 'sivolu', varid )
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att( ncid, varid, '_FillValue', fill_val ), '' )
               CALL iom_nf90_check( nf90_get_var( ncid, varid, ztmp_vi, start_s, count_s ), '' )
               WHERE( ztmp_vi(:,:) == fill_val ) ztmp_vi(:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'vt_i' not found in file "//TRIM( filename ) )
            END IF

            ! Read in snow height (volume per area)
            istat = nf90_inq_varid( ncid, 'snvolu', varid )
            IF( istat == nf90_noerr ) THEN
               CALL iom_nf90_check( nf90_get_att( ncid, varid, '_FillValue', fill_val ), '' )
               CALL iom_nf90_check( nf90_get_var( ncid, varid, ztmp_vs, start_s, count_s ), '' )
               WHERE( ztmp_vs(:,:) == fill_val ) ztmp_vs(:,:) = fbrmdi
            ELSE
               CALL ctl_warn( '   SAO - sao_read_file:', "      variable 'vt_s' not found in file "//TRIM( filename ) )
            END IF

            ! Initialise model fields
            at_i(A2D(0)) = fbrmdi
            vt_i(A2D(0)) = fbrmdi
            vt_s(A2D(0)) = fbrmdi
            at_i(A2D(0)) = ztmp_ai(:,:) * tmask(A2D(0),1)
            vt_i(A2D(0)) = ztmp_vi(:,:) * tmask(A2D(0),1)
            vt_s(A2D(0)) = ztmp_vs(:,:) * tmask(A2D(0),1)

            ! Update lateral boundary
            CALL lbc_lnk( 'sao_read', at_i(:,:), 'T', 1.0_wp,   &
               &                      vt_i(:,:), 'T', 1.0_wp,   &
               &                      vt_s(:,:), 'T', 1.0_wp )

            ! Deallocate auxiliary arrays
            DEALLOCATE( ztmp_ai, ztmp_vi, ztmp_vs, STAT=istat )
            IF( istat /= 0 ) CALL ctl_stop( '   SAO - sao_read_file:', '      memory deallocation failed' )
#endif

         ELSE
            ! Mark all as missing data
            ts(:,:,:,:,Kmm) = fbrmdi
            ssh(:,:,Kmm)    = fbrmdi
            uu(:,:,:,Kmm)   = fbrmdi
            vv(:,:,:,Kmm)   = fbrmdi
            rhop(:,:,1)     = fbrmdi
#if defined key_si3
            at_i(:,:)       = fbrmdi
            vt_i(:,:)       = fbrmdi
            vt_s(:,:)       = fbrmdi
#endif
         ENDIF
         ! Close netcdf file
         IF(lwp) WRITE(numout,*) "Closing :", TRIM(filename)
         CALL iom_nf90_check( nf90_close(ncid), '' )
      END IF
      !
   END SUBROUTINE sao_read_file
   
   !!------------------------------------------------------------------------
END MODULE sao_read
