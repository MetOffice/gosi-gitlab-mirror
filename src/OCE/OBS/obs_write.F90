MODULE obs_write
   !!======================================================================
   !!                       ***  MODULE obs_write   ***
   !! Observation diagnosticss: Write observation related diagnostics
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   obs_wri_prof   : Write profile observations in feedback format
   !!   obs_wri_surf   : Write surface observations in feedback format
   !!   obs_wri_stats  : Print basic statistics on the data being written out
   !!----------------------------------------------------------------------

   !! * Modules used
   USE par_kind, ONLY : &   ! Precision variables
      & wp
   USE in_out_manager       ! I/O manager
   USE dom_oce              ! Ocean space and time domain variables
   USE obs_types            ! Observation type integer to character translation
   USE julian, ONLY : &         ! Julian date routines
      & greg2jul
   USE obs_utils, ONLY : &  ! Observation operator utility functions
      & chkerr
   USE obs_profiles_def     ! Type definitions for profiles
   USE obs_surf_def         ! Type defintions for surface observations
   USE obs_fbm              ! Observation feedback I/O
   USE obs_grid             ! Grid tools
   USE obs_conv             ! Conversion between units
   USE obs_const
   USE obs_mpp              ! MPP support routines for observation diagnostics
   USE lib_mpp		    ! MPP routines
   USE eosbn2, ONLY : &
      & ln_EOS80, ln_TEOS10, &
      & gsw_p_from_z, gsw_sa_from_sp, &
      & gsw_ct_from_pt, gsw_ct_from_t

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE
   PUBLIC obs_wri_prof, &    ! Write profile observation files
      &   obs_wri_surf, &    ! Write surface observation files
      &   obswriinfo

   TYPE obswriinfo
      INTEGER :: inum
      INTEGER, POINTER, DIMENSION(:) :: ipoint
      CHARACTER(len=ilenname), POINTER, DIMENSION(:) :: cdname
      CHARACTER(len=ilenlong), POINTER, DIMENSION(:,:) :: cdlong
      CHARACTER(len=ilenunit), POINTER, DIMENSION(:,:) :: cdunit
   END TYPE obswriinfo

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_wri_prof( profdata, cdfiletype, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_prof  ***
      !!
      !! ** Purpose : Write profile feedback files
      !!
      !! ** Method  : NetCDF
      !!
      !! ** Action  :
      !!
      !! History :
      !!      ! 06-04  (A. Vidard) Original
      !!      ! 06-04  (A. Vidard) Reformatted
      !!      ! 06-10  (A. Weaver) Cleanup
      !!      ! 07-01  (K. Mogensen) Use profile data types
      !!      ! 07-03  (K. Mogensen) General handling of profiles
      !!      ! 09-01  (K. Mogensen) New feedback format
      !!      ! 15-02  (M. Martin) Combined routine for writing profiles
      !!-----------------------------------------------------------------------

      !! * Arguments
      TYPE(obs_prof)            , INTENT(INOUT) :: profdata      ! Full set of profile data
      CHARACTER(LEN=25)         , INTENT(IN   ) :: cdfiletype    ! Base name for file name
      TYPE(obswriinfo), OPTIONAL, INTENT(IN   ) :: padd          ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL, INTENT(IN   ) :: pext          ! Extra info

      !! * Local declarations
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: clfname
      CHARACTER(LEN=ilenlong) :: cllongname  ! Long name of variable
      CHARACTER(LEN=ilenunit) :: clunits     ! Units of variable
      CHARACTER(LEN=ilengrid) :: clgrid      ! Grid of variable
      CHARACTER(LEN=12) :: clfmt            ! writing format
      INTEGER :: idg                        ! number of digits
      INTEGER :: ilevel
      INTEGER :: jvar
      INTEGER :: jvar2
      INTEGER :: jpsal
      INTEGER :: jpotm
      INTEGER :: jtemp
      INTEGER :: jo
      INTEGER :: jk
      INTEGER :: ik
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: iadd
      INTEGER :: iext
      REAL(wp) :: zpres

      IF ( PRESENT( padd ) ) THEN
         iadd = padd%inum
      ELSE
         iadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         iext = pext%inum
      ELSE
         iext = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      ! Find maximum level
      ilevel = 0
      DO jvar = 1, profdata%nvar
         ilevel = MAX( ilevel, MAXVAL( profdata%var(jvar)%nvlidx(:) ) )
      END DO

      CALL alloc_obfbdata( fbdata, profdata%nvar, profdata%nprof, ilevel, &
            &                 1 + iadd, iext, .TRUE. )
      fbdata%caddname(1)   = 'Hx'
      DO jvar = 1, profdata%nvar
         fbdata%cname(jvar)      = profdata%cvars(jvar)
         fbdata%coblong(jvar)    = profdata%clong(jvar)
         fbdata%cobunit(jvar)    = profdata%cunit(jvar)
         fbdata%cgrid(jvar)      = profdata%cgrid(jvar)
         fbdata%caddlong(1,jvar) = 'Model interpolated ' // TRIM(profdata%clong(jvar))
         fbdata%caddunit(1,jvar) = profdata%cunit(jvar)
         IF (iadd > 0) THEN
            DO ja = 1, iadd
               fbdata%caddname(1+ja) = padd%cdname(ja)
               fbdata%caddlong(1+ja,jvar) = padd%cdlong(ja,jvar)
               fbdata%caddunit(1+ja,jvar) = padd%cdunit(ja,jvar)
            END DO
         ENDIF
         IF (iext > 0) THEN
            DO je = 1, iext
               fbdata%cextname(je) = pext%cdname(je)
               fbdata%cextlong(je) = pext%cdlong(je,1)
               fbdata%cextunit(je) = pext%cdunit(je,1)
            END DO
         ENDIF
      END DO

      WRITE(clfname, FMT="(A,'fb_fdbk_',I4.4,'.nc')") TRIM(cdfiletype), narea - 1

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_wri_prof :'
         WRITE(numout,*) '~~~~~~~~~~~~~'
         WRITE(numout,*) 'Writing '//TRIM(cdfiletype)//' feedback file : ', TRIM(clfname)
      ENDIF

      ! Transform obs_prof data structure into obfb data structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, profdata%nprof
         fbdata%plam(jo)      = profdata%rlam(jo)
         fbdata%pphi(jo)      = profdata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') profdata%ntyp(jo)
         fbdata%ivqc(jo,:)    = profdata%ivqc(jo,:)
         fbdata%ivqcf(:,jo,:) = profdata%ivqcf(:,jo,:)
         IF ( profdata%nqc(jo) > 255 ) THEN
            fbdata%ioqc(jo)    = IBSET(profdata%nqc(jo),2)
            fbdata%ioqcf(1,jo) = profdata%nqcf(1,jo)
            fbdata%ioqcf(2,jo) = profdata%nqc(jo)
         ELSE
            fbdata%ioqc(jo)    = profdata%nqc(jo)
            fbdata%ioqcf(:,jo) = profdata%nqcf(:,jo)
         ENDIF
         fbdata%ipqc(jo)      = profdata%ipqc(jo)
         fbdata%ipqcf(:,jo)   = profdata%ipqcf(:,jo)
         fbdata%itqc(jo)      = profdata%itqc(jo)
         fbdata%itqcf(:,jo)   = profdata%itqcf(:,jo)
         fbdata%cdwmo(jo)     = profdata%cwmo(jo)
         fbdata%kindex(jo)    = profdata%npfil(jo)
         DO jvar = 1, profdata%nvar
            IF (ln_grid_global) THEN
               fbdata%iobsi(jo,jvar) = profdata%mi(jo,jvar)
               fbdata%iobsj(jo,jvar) = profdata%mj(jo,jvar)
            ELSE
               IF ( (profdata%mi(jo,jvar) > 0 ) .AND. &
                  & (profdata%mj(jo,jvar) > 0 ) ) THEN
                  fbdata%iobsi(jo,jvar) = mig(profdata%mi(jo,jvar),nn_hls)
                  fbdata%iobsj(jo,jvar) = mjg(profdata%mj(jo,jvar),nn_hls)
               ELSE
                  fbdata%iobsi(jo,jvar) = -1
                  fbdata%iobsj(jo,jvar) = -1
               ENDIF
            ENDIF
         END DO
         CALL greg2jul( 0, &
            &           profdata%nmin(jo), &
            &           profdata%nhou(jo), &
            &           profdata%nday(jo), &
            &           profdata%nmon(jo), &
            &           profdata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         ! Reform the profiles arrays for output
         DO jvar = 1, profdata%nvar
            DO jk = profdata%npvsta(jo,jvar), profdata%npvend(jo,jvar)
               ik = profdata%var(jvar)%nvlidx(jk)
               fbdata%padd(ik,jo,1,jvar) = profdata%var(jvar)%vmod(jk)
               fbdata%pob(ik,jo,jvar)    = profdata%var(jvar)%vobs(jk)
               fbdata%pdep(ik,jo)        = profdata%var(jvar)%vdep(jk)
               fbdata%idqc(ik,jo)        = profdata%var(jvar)%idqc(jk)
               fbdata%idqcf(:,ik,jo)     = profdata%var(jvar)%idqcf(:,jk)
               IF ( profdata%var(jvar)%nvqc(jk) > 255 ) THEN
                  fbdata%ivlqc(ik,jo,jvar) = IBSET(profdata%var(jvar)%nvqc(jk),2)
                  fbdata%ivlqcf(1,ik,jo,jvar) = profdata%var(jvar)%nvqcf(1,jk)
!$AGRIF_DO_NOT_TREAT
                  fbdata%ivlqcf(2,ik,jo,jvar) = IAND(profdata%var(jvar)%nvqc(jk),b'0000000011111111')
!$AGRIF_END_DO_NOT_TREAT
               ELSE
                  fbdata%ivlqc(ik,jo,jvar) = profdata%var(jvar)%nvqc(jk)
                  fbdata%ivlqcf(:,ik,jo,jvar) = profdata%var(jvar)%nvqcf(:,jk)
               ENDIF
               fbdata%iobsk(ik,jo,jvar)  = profdata%var(jvar)%mvk(jk)
               IF (iadd > 0) THEN
                  DO ja = 1, iadd
                     fbdata%padd(ik,jo,1+ja,jvar) = &
                        & profdata%var(jvar)%vadd(jk,padd%ipoint(ja))
                  END DO
               ENDIF
            END DO
         END DO
         IF (iext > 0) THEN
            DO jk = profdata%npvstaext(jo), profdata%npvendext(jo)
               ik = profdata%vext%nelidx(jk)
               DO je = 1, iext
                  fbdata%pext(ik,jo,je) = &
                     & profdata%vext%eobs(jk,pext%ipoint(je))
               END DO
            END DO
         ENDIF
      END DO

      ! identify index associated with PSAL, if it exists
      jpsal = 0
      DO jvar = 1, profdata%nvar
         IF ( TRIM(profdata%cvars(jvar)) == 'PSAL' ) THEN
            jpsal = jvar
            EXIT
         ENDIF
      END DO

      ! identify index associated with POTM, if it exists
      jpotm = 0
      DO jvar = 1, profdata%nvar
         IF ( TRIM(profdata%cvars(jvar)) == 'POTM' ) THEN
            jpotm = jvar
            EXIT
         ENDIF
      END DO

      ! identify extra index associated with (in-situ) TEMP, if it exists
      jtemp = 0
      IF (iext > 0) THEN
         DO je = 1, iext
            IF ( TRIM(fbdata%cextname(je)) == 'TEMP' ) THEN
               jtemp = je
               EXIT
            ENDIF
         END DO
      ENDIF

      IF ( ln_EOS80 ) THEN
         ! Convert insitu temperature to potential temperature using the model
         ! salinity if no potential temperature
         IF ( jpotm > 0 ) THEN           ! check for POTM
            IF ( jpsal > 0 ) THEN        ! check for PSAL
               IF ( jtemp > 0 ) THEN     ! check for INSITU-TEMP
                  DO jo = 1, fbdata%nobs
                      IF ( fbdata%pphi(jo) < 9999.0_wp ) THEN
                        DO jk = 1, fbdata%nlev
                           IF ( ( fbdata%pob(jk,jo,jpotm)   >= 9999.0_wp ) .AND. &
                              & ( fbdata%pdep(jk,jo)        < 9999.0_wp ) .AND. &
                              & ( fbdata%padd(jk,jo,1,jpotm) < 9999.0_wp ) .AND. &
                              & ( fbdata%pext(jk,jo,jtemp)     < 9999.0_wp ) ) THEN
                              IF ( fbdata%padd(jk,jo,1,jpsal) < 9999.0_wp ) THEN
                                 zpres = dep_to_p( REAL(fbdata%pdep(jk,jo),wp), &
                                    &              REAL(fbdata%pphi(jo),wp) )
                                 fbdata%pob(jk,jo,jpotm) = potemp( &
                                    &                     REAL(fbdata%padd(jk,jo,1,jpsal), wp), &
                                    &                     REAL(fbdata%pext(jk,jo,jtemp), wp),     &
                                    &                     zpres, 0.0_wp )
                              ELSE
                                 CALL ctl_stop( 'No salinity available for insitu to pot T obs conversion, try setting ln_all_at_all to true' )
                              ENDIF
                           ENDIF
                        END DO
                     ENDIF
                  END DO
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ( ln_TEOS10 ) THEN
         IF ( jpotm > 0 ) THEN
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'Converting profile obs from EOS80 variables to TEOS10 variables'
            ENDIF
            IF ( jpsal == 0 ) THEN
               CALL ctl_stop( 'No salinity available in obs structure, T obs conversion will fail ' )
            ENDIF
            ! Convert practical salinity to absolute salinity
            ! Convert potential or insitu temperature to conservative temperature
            ! where observed salinity doesn't exist, use model salinity.
            DO jo = 1, fbdata%nobs
               IF ( fbdata%pphi(jo) < 9999.0_wp ) THEN
                  DO jk = 1, fbdata%nlev
                     IF ( fbdata%pdep(jk,jo) < 9999.0_wp ) THEN
                        zpres = gsw_p_from_z( &
                                 & -1.0_wp * REAL(fbdata%pdep(jk,jo),wp), &
                                 & REAL(fbdata%pphi(jo),wp) )
                        IF ( fbdata%pob(jk,jo,jpsal) < 9999.0_wp ) THEN
                           ! convert salinity
                           fbdata%pob(jk,jo,jpsal) = gsw_sa_from_sp( &
                                                         & REAL(fbdata%pob(jk,jo,jpsal),wp), &
                                                         & zpres, REAL(fbdata%plam(jo),wp), &
                                                         & REAL(fbdata%pphi(jo),wp) )
                        ENDIF
                        IF ( fbdata%padd(jk,jo,1,jpotm) < 9999.0_wp ) THEN
                           IF ( fbdata%pob(jk,jo,jpotm) < 9999.0_wp ) THEN
                              ! use observed potential temperature
                              IF ( fbdata%pob(jk,jo,jpsal) < 9999.0_wp ) THEN
                                 ! use observed salinity
                                 fbdata%pob(jk,jo,jpotm) = gsw_ct_from_pt( &
                                                            & REAL(fbdata%pob(jk,jo,jpsal),wp), &
                                                            & REAL(fbdata%pob(jk,jo,jpotm),wp) )
                              ELSEIF ( fbdata%padd(jk,jo,1,jpsal) < 9999.0_wp ) THEN
                                 ! use model salinity
                                 fbdata%pob(jk,jo,jpotm) = gsw_ct_from_pt( &
                                                            & REAL(fbdata%padd(jk,jo,1,jpsal),wp), &
                                                            & REAL(fbdata%pob(jk,jo,jpotm),wp) )
                              ELSE
                                 CALL ctl_stop( 'No salinity available for pot to con T obs conversion, try setting ln_all_at_all to true ' )
                              ENDIF
                           ELSEIF ( jtemp > 0 ) THEN
                              ! use observed in-situ temperature

                              IF ( fbdata%pext(jk,jo,jtemp) < 9999.0_wp ) THEN
                                 ! Use observed insitu temperature
                                 IF ( fbdata%pob(jk,jo,jpsal) < 9999.0_wp ) THEN
                                    ! observed salinity exists
                                    fbdata%pob(jk,jo,jpotm) = gsw_ct_from_t( &
                                                               & REAL(fbdata%pob(jk,jo,jpsal),wp), &
                                                               & REAL(fbdata%pext(jk,jo,jtemp),wp), &
                                                               & zpres )
                                 ELSEIF ( fbdata%padd(jk,jo,1,2) < 9999.0_wp ) THEN
                                    ! use model salinity
                                    fbdata%pob(jk,jo,jpotm) = gsw_ct_from_t( &
                                                               & REAL(fbdata%padd(jk,jo,1,jpsal),wp), &
                                                               & REAL(fbdata%pext(jk,jo,jtemp),wp), &
                                                               & zpres )
                                 ELSE
                                    CALL ctl_stop( 'No salinity available for in-situ to con T obs conversion, try setting ln_all_at_all to true ' )
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  END DO
               ENDIF
            END DO
            ! Update names and attributes for converted T and S variables
            fbdata%cname(jpotm) = 'COTM'
            fbdata%cname(jpsal) = 'ASAL'
            fbdata%coblong(jpotm) = "conservative temperature"
            fbdata%coblong(jpsal) = "absolute salinity"
            fbdata%caddlong(1,jpotm) = 'Model interpolated ' // TRIM(fbdata%coblong(jpotm))
            fbdata%caddlong(1,jpsal) = 'Model interpolated ' // TRIM(fbdata%coblong(jpsal))
            fbdata%cobunit(jpsal)    = 'g/kg'
            fbdata%caddunit(1,jpsal)  = fbdata%cobunit(jpsal)
         ENDIF
      ENDIF

      ! Write the obfbdata structure
      CALL write_obfbdata( clfname, fbdata )

      ! Output some basic statistics
      CALL obs_wri_stats( fbdata )

      CALL dealloc_obfbdata( fbdata )

   END SUBROUTINE obs_wri_prof

   SUBROUTINE obs_wri_surf( surfdata, cdfiletype, padd, pext )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_surf  ***
      !!
      !! ** Purpose : Write surface observation files
      !!
      !! ** Method  : NetCDF
      !!
      !! ** Action  :
      !!
      !!      ! 07-03  (K. Mogensen) Original
      !!      ! 09-01  (K. Mogensen) New feedback format.
      !!      ! 15-02  (M. Martin) Combined surface writing routine.
      !!-----------------------------------------------------------------------

      !! * Modules used
      IMPLICIT NONE

      !! * Arguments
      TYPE(obs_surf)            , INTENT(INOUT) :: surfdata      ! Full set of surface data
      CHARACTER(LEN=25)         , INTENT(IN   ) :: cdfiletype    ! Base name for file name
      TYPE(obswriinfo), OPTIONAL, INTENT(IN   ) :: padd          ! Additional info for each variable
      TYPE(obswriinfo), OPTIONAL, INTENT(IN   ) :: pext          ! Extra info

      !! * Local declarations
      TYPE(obfbdata) :: fbdata
      CHARACTER(LEN=40) :: clfname         ! netCDF filename
      CHARACTER(LEN=ilenlong) :: cllongname  ! Long name of variable
      CHARACTER(LEN=ilenunit) :: clunits     ! Units of variable
      CHARACTER(LEN=ilengrid) :: clgrid      ! Grid of variable
      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_wri_surf'
      INTEGER :: jo
      INTEGER :: ja
      INTEGER :: je
      INTEGER :: jvar
      INTEGER :: iadd
      INTEGER :: iext
      REAL(wp) :: zpres

      IF ( PRESENT( padd ) ) THEN
         iadd = padd%inum
      ELSE
         iadd = 0
      ENDIF

      IF ( PRESENT( pext ) ) THEN
         iext = pext%inum
      ELSE
         iext = 0
      ENDIF

      CALL init_obfbdata( fbdata )

      CALL alloc_obfbdata( fbdata, surfdata%nvar, surfdata%nsurf, 1, &
            &                 1 + iadd, iext, .TRUE. )
      fbdata%caddname(1)   = 'Hx'
      DO jvar = 1, surfdata%nvar
         fbdata%cname(jvar)      = surfdata%cvars(jvar)
         fbdata%coblong(jvar)    = surfdata%clong(jvar)
         fbdata%cobunit(jvar)    = surfdata%cunit(jvar)
         fbdata%cgrid(jvar)      = surfdata%cgrid(jvar)
         fbdata%caddlong(1,jvar) = 'Model interpolated ' // TRIM(surfdata%clong(jvar))
         fbdata%caddunit(1,jvar) = surfdata%cunit(jvar)
         IF (iadd > 0) THEN
            DO ja = 1, iadd
               fbdata%caddname(1+ja) = padd%cdname(ja)
               fbdata%caddlong(1+ja,jvar) = padd%cdlong(ja,jvar)
               fbdata%caddunit(1+ja,jvar) = padd%cdunit(ja,jvar)
            END DO
         ENDIF
         IF (iext > 0) THEN
            DO je = 1, iext
               fbdata%cextname(je) = pext%cdname(je)
               fbdata%cextlong(je) = pext%cdlong(je,1)
               fbdata%cextunit(je) = pext%cdunit(je,1)
            END DO
         ENDIF
         IF ( surfdata%cvars(jvar) == 'SST' .AND. ln_TEOS10 ) THEN
            ! Update names and attributes for converted SST variables
            fbdata%cname(jvar) = 'CSST'
            fbdata%coblong(jvar) = 'Conservative sea surface temperature'
            fbdata%caddlong(1,jvar) = 'Model interpolated ' // TRIM(fbdata%coblong(jvar))
         ENDIF
         IF ( surfdata%cvars(jvar) == 'SSS' .AND. ln_TEOS10 ) THEN
            ! Update names and attributes for converted SSS variables
            fbdata%cname(jvar) = 'ASSS'
            fbdata%coblong(jvar) = 'Absolute sea surface salinity'
            fbdata%caddlong(1,jvar) = 'Model interpolated '// TRIM(fbdata%coblong(jvar))
         ENDIF
      END DO

      WRITE(clfname, FMT="(A,'fb_fdbk_',I4.4,'.nc')") TRIM(cdfiletype), narea - 1

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_wri_surf :'
         WRITE(numout,*) '~~~~~~~~~~~~~'
         WRITE(numout,*) 'Writing '//TRIM(surfdata%cvars(1))//' feedback file : ', TRIM(clfname)
      ENDIF

      ! Transform surf data structure into obfbdata structure
      fbdata%cdjuldref = '19500101000000'
      DO jo = 1, surfdata%nsurf
         fbdata%plam(jo)      = surfdata%rlam(jo)
         fbdata%pphi(jo)      = surfdata%rphi(jo)
         WRITE(fbdata%cdtyp(jo),'(I4)') surfdata%ntyp(jo)
         fbdata%ivqc(jo,:)    = 0
         fbdata%ivqcf(:,jo,:) = 0
         IF ( surfdata%nqc(jo) > 255 ) THEN
            fbdata%ioqc(jo)    = 4
            fbdata%ioqcf(1,jo) = 0
!$AGRIF_DO_NOT_TREAT
            fbdata%ioqcf(2,jo) = IAND(surfdata%nqc(jo),b'0000000011111111')
!$AGRIF_END_DO_NOT_TREAT
         ELSE
            fbdata%ioqc(jo)    = surfdata%nqc(jo)
            fbdata%ioqcf(:,jo) = 0
         ENDIF
         fbdata%ipqc(jo)      = 0
         fbdata%ipqcf(:,jo)   = 0
         fbdata%itqc(jo)      = 0
         fbdata%itqcf(:,jo)   = 0
         fbdata%cdwmo(jo)     = surfdata%cwmo(jo)
         fbdata%kindex(jo)    = surfdata%nsfil(jo)
         DO jvar = 1, surfdata%nvar
            IF (ln_grid_global) THEN
               fbdata%iobsi(jo,jvar) = surfdata%mi(jo,jvar)
               fbdata%iobsj(jo,jvar) = surfdata%mj(jo,jvar)
            ELSE
               IF ( (surfdata%mi(jo,jvar) > 0 ) .AND. &
                  & (surfdata%mj(jo,jvar) > 0 ) ) THEN
                  fbdata%iobsi(jo,jvar) = mig(surfdata%mi(jo,jvar),nn_hls)
                  fbdata%iobsj(jo,jvar) = mjg(surfdata%mj(jo,jvar),nn_hls)
               ELSE
                  fbdata%iobsi(jo,jvar) = -1
                  fbdata%iobsj(jo,jvar) = -1
               ENDIF
            ENDIF
         END DO
         CALL greg2jul( 0, &
            &           surfdata%nmin(jo), &
            &           surfdata%nhou(jo), &
            &           surfdata%nday(jo), &
            &           surfdata%nmon(jo), &
            &           surfdata%nyea(jo), &
            &           fbdata%ptim(jo),   &
            &           krefdate = 19500101 )
         fbdata%pdep(1,jo)     = 0.0_wp
         fbdata%idqc(1,jo)     = 0
         fbdata%idqcf(:,1,jo)  = 0
         DO jvar = 1, surfdata%nvar
            fbdata%padd(1,jo,1,jvar) = surfdata%rmod(jo,jvar)
            IF ( ( surfdata%cvars(jvar) == 'SSS' ) .AND. &
               & ( surfdata%robs(jo,jvar) < 9999.0_wp ) .AND. &
               & ln_TEOS10 ) THEN
               zpres = gsw_p_from_z( 0.0_wp, surfdata%rphi(jo) )
               fbdata%pob(1,jo,jvar) = gsw_sa_from_sp( surfdata%robs(jo,jvar), &
                  &                                    zpres, surfdata%rlam(jo), &
                  &                                    surfdata%rphi(jo) )
            ELSE
               fbdata%pob(1,jo,jvar)    = surfdata%robs(jo,jvar)
            ENDIF
            IF ( surfdata%nqc(jo) > 255 ) THEN
               fbdata%ivqc(jo,jvar)       = 4
               fbdata%ivlqc(1,jo,jvar)    = 4
               fbdata%ivlqcf(1,1,jo,jvar) = 0
!$AGRIF_DO_NOT_TREAT
               fbdata%ivlqcf(2,1,jo,jvar) = IAND(surfdata%nqc(jo),b'0000000011111111')
!$AGRIF_END_DO_NOT_TREAT
            ELSE
               fbdata%ivqc(jo,jvar)       = surfdata%nqc(jo)
               fbdata%ivlqc(1,jo,jvar)    = surfdata%nqc(jo)
               fbdata%ivlqcf(:,1,jo,jvar) = 0
            ENDIF
            fbdata%iobsk(1,jo,jvar)  = 0
            IF (iadd > 0) THEN
               DO ja = 1, iadd
                  fbdata%padd(1,jo,1+ja,jvar) = &
                     & surfdata%radd(jo,padd%ipoint(ja),jvar)
               END DO
            ENDIF
         END DO
         IF (iext > 0) THEN
            DO je = 1, iext
               fbdata%pext(1,jo,je) = &
                  & surfdata%rext(jo,pext%ipoint(je))
            END DO
         ENDIF
      END DO

      ! Write the obfbdata structure
      CALL write_obfbdata( clfname, fbdata )

      ! Output some basic statistics
      CALL obs_wri_stats( fbdata )

      CALL dealloc_obfbdata( fbdata )

   END SUBROUTINE obs_wri_surf

   SUBROUTINE obs_wri_stats( fbdata )
      !!-----------------------------------------------------------------------
      !!
      !!                     *** ROUTINE obs_wri_stats  ***
      !!
      !! ** Purpose : Output some basic statistics of the data being written out
      !!
      !! ** Method  :
      !!
      !! ** Action  :
      !!
      !!      ! 2014-08  (D. Lea) Initial version
      !!-----------------------------------------------------------------------

      !! * Arguments
      TYPE(obfbdata), INTENT(in) :: fbdata

      !! * Local declarations
      INTEGER :: jvar
      INTEGER :: jo
      INTEGER :: jk
      INTEGER :: inumgoodobs
      INTEGER :: inumgoodobsmpp
      REAL(wp) :: zsumx
      REAL(wp) :: zsumx2
      REAL(wp) :: zomb


      IF (lwp) THEN
         WRITE(numout,*) ''
         WRITE(numout,*) 'obs_wri_stats :'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
      ENDIF

      DO jvar = 1, fbdata%nvar
         zsumx = 0.0_wp
         zsumx2 = 0.0_wp
         inumgoodobs = 0
         DO jo = 1, fbdata%nobs
            DO jk = 1, fbdata%nlev
               IF ( ( fbdata%pob(jk,jo,jvar) < 9999.0_wp ) .AND. &
                  & ( fbdata%pdep(jk,jo) < 9999.0_wp ) .AND. &
                  & ( fbdata%padd(jk,jo,1,jvar) < 9999.0_wp ) ) THEN

                  zomb = fbdata%pob(jk, jo, jvar) - fbdata%padd(jk, jo, 1, jvar)
                  zsumx = zsumx + zomb
                  zsumx2 = zsumx2 + zomb**2
                  inumgoodobs = inumgoodobs + 1
               ENDIF
            ENDDO
         ENDDO

         CALL obs_mpp_sum_integer( inumgoodobs, inumgoodobsmpp )
         CALL mpp_sum('obs_write', zsumx)
         CALL mpp_sum('obs_write', zsumx2)

         IF (lwp) THEN
            WRITE(numout,*) 'Type: ', fbdata%cname(jvar), '  Total number of good observations: ', inumgoodobsmpp
            IF ( inumgoodobsmpp > 0 ) THEN
               WRITE(numout,*) 'Overall mean obs minus model of the good observations: ', zsumx / REAL(inumgoodobsmpp, wp)
               WRITE(numout,*) 'Overall RMS obs minus model of the good observations: ', sqrt( zsumx2 / REAL(inumgoodobsmpp, wp) )
            ENDIF
            WRITE(numout,*) ''
         ENDIF

      ENDDO

   END SUBROUTINE obs_wri_stats

END MODULE obs_write
