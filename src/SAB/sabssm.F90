MODULE sabssm
   !!======================================================================
   !!                       ***  MODULE  sabssm  ***
   !! Off-line : interpolation of the physical fields
   !!======================================================================
   !! History :  5.1  ! 2026-04 (J. Petit and P. Mathiot)  original code
   !!----------------------------------------------------------------------
   !!   sab_ssm_init  : initialization, namelist read, and SAVEs control
   !!   sab_ssm       : Interpolation of the fields
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean domain: variables
   USE sbc_oce        ! surface module: variables
   USE sbc_ice
   USE ice
   USE phycst         ! physical constants
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE icb_oce        ! for icebergs
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE lib_mpp        ! distributed memory computing library
   USE prtctl         ! print control
   USE fldread        ! read input fields
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE
   PUBLIC sab_ssm, sab_ssm_init

   CHARACTER(len=100) ::   cn_dir        ! Root directory for location of ssm files

   LOGICAL     ::   ln_sabread     ! flag to read input forcing file (T) or use an analytical initialisation (F)
   INTEGER     ::   nfld_3d        ! number of 3d forcings
   INTEGER     ::   nfld_2d        ! number of 2d forcings

   INTEGER     ::   jf_ssh         ! index of ss height
   INTEGER     ::   jf_sst         ! index of ss temperature
   INTEGER     ::   jf_sss         ! index of ss salinity
   INTEGER     ::   jf_fr_i        ! index of frac ice in cell 
   INTEGER     ::   jf_ssu, jf_ssv ! index of u and v velocity component
   INTEGER     ::   jf_utau, jf_vtau     ! index of wind velocity seen by icb
   INTEGER     ::   jf_uice, jf_vice     ! index of  u_ice, v_ice
   INTEGER     ::   jf_at_i, jf_vt_i     ! index of  at_i, vt_i
   INTEGER     ::   jf_uu, jf_vv, jf_t3D ! index of 3D uu,vv,ts 
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) :: sf_ssm_3d  ! structure of input fields (file information, fields read)
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) :: sf_ssm_2d  ! structure of input fields (file information, fields read)

   !! * Substitutions
# include "read_nml_substitute.h90"
# include "do_loop_substitute.h90"
   CONTAINS

   SUBROUTINE sab_ssm( kt )
   !-------------------------------------
   INTEGER, INTENT(in) ::   kt
   INTEGER             ::   ji,jj 
   REAL(wp), DIMENSION(jpi,jpj) :: zutau, zvtau 
   !-------------------------------------
   IF( ln_timing )   CALL timing_start( 'sab_ssm')

      IF ( ln_sabread ) THEN

         CALL fld_read( kt, 1, sf_ssm_2d )      !==   read data at kt time step   ==!
         !
         ! Assigns forcings to forcing arrays
         ! No default value set when 'NOT USED' used as file name
         ! => the user can easily provide a dummy inpout file
         sst_m(A2D(0)) = sf_ssm_2d(jf_sst)%fnow(A2D(0) ,1) * tmask(A2D(0),1) ! sea surface temperature
         sss_m(A2D(0)) = sf_ssm_2d(jf_sss)%fnow(A2D(0) ,1) * tmask(A2D(0),1) ! sea surface salinity
         ssh_m(A2D(0)) = sf_ssm_2d(jf_ssh)%fnow(A2D(0) ,1) * tmask(A2D(0),1) ! sea surface height
         fr_i(A2D(0))  = sf_ssm_2d(jf_fr_i)%fnow(A2D(0),1) * tmask(A2D(0),1) ! frac ice in cell 
 
         ssu_m(A2D(0)) = sf_ssm_2d(jf_ssu)%fnow(A2D(0),1) * umask(A2D(0),1)  ! sea surface u velocity
         ssv_m(A2D(0)) = sf_ssm_2d(jf_ssv)%fnow(A2D(0),1) * vmask(A2D(0),1)  ! sea surface v velocity

         zutau(A2D(0)) = sf_ssm_2d(jf_utau)%fnow(A2D(0),1) * tmask(A2D(0),1)  ! u wind stress over ocean at t-point
         zvtau(A2D(0)) = sf_ssm_2d(jf_vtau)%fnow(A2D(0),1) * tmask(A2D(0),1)  ! v wind stress over ocean at t-point
         
         ! Fill halos (really needed ?)
         CALL lbc_lnk( 'sabssm', ssh_m   , 'T',  1.0_wp, &
                              &  sst_m   , 'T',  1.0_wp, &
                              &  sss_m   , 'T',  1.0_wp, &
                              &  fr_i    , 'T',  1.0_wp, &
                              &  ssu_m   , 'U', -1.0_wp, &
                              &  ssv_m   , 'V', -1.0_wp, &
                              &  zutau   , 'T', -1.0_wp, &
                              &  zvtau   , 'T', -1.0_wp, ldfull = .TRUE. )

         ! Output forcing data
         CALL iom_put( 'ssu_m', ssu_m )
         CALL iom_put( 'ssv_m', ssv_m )
         CALL iom_put( 'sst_m', sst_m )
         CALL iom_put( 'sss_m', sss_m )
         CALL iom_put( 'ssh_m', ssh_m )
         CALL iom_put( 'ice_cover', fr_i(A2D(0)) )
         CALL iom_put( 'utau_oce' , zutau )
         CALL iom_put( 'vtau_oce' , zvtau )

         ! compute utau and vtau at u-v point
         ! save pure stresses (with no ice-ocean stress) for use by icebergs
         !     Note the use of 0.5*(2-umask) in order to unmask the stress along coastlines
         !      and the use of MAX(tmask(i,j),tmask(i+1,j) is to mask tau over ice shelves
         ! (PM) cannot be move to icb because we need pure stresses. Why not extract directly wind from sbcblk i
         ! (icb only need wind)
         DO_2D( 0, 0, 0, 0 )
            utau_icb(ji,jj) = 0.5_wp * ( zutau(ji,jj) + zutau(ji+1,jj) ) * &
               &                       ( 2. - umask(ji,jj,1) ) * MAX( tmask(ji,jj,1), tmask(ji+1,jj,1) ) * umask(ji,jj,1)
            vtau_icb(ji,jj) = 0.5_wp * ( zvtau(ji,jj) + zvtau(ji,jj+1) ) * &
               &                       ( 2. - vmask(ji,jj,1) ) * MAX( tmask(ji,jj,1), tmask(ji,jj+1,1) ) * vmask(ji,jj,1)
         END_2D
         CALL lbc_lnk( 'sabssm', utau_icb, 'U', -1.0_wp, vtau_icb, 'V', -1.0_wp, ldfull = .TRUE. )

         IF (ln_M2016) THEN

            CALL fld_read( kt, 1, sf_ssm_3d )      !==   read data at kt time step   ==!
            !
            ! Assigns forcings to forcing arrays
            ! No default value set when 'NOT USED' used as file name
            ! => the user can easily provide a dummy inpout file
            uu(A2D(0),:,1) = sf_ssm_3d(jf_uu)%fnow(A2D(0),:) * umask(A2D(0),:)
            vv(A2D(0),:,1) = sf_ssm_3d(jf_vv)%fnow(A2D(0),:) * vmask(A2D(0),:)
            ts(A2D(0),:,jp_tem,1) = sf_ssm_3d(jf_t3D)%fnow(A2D(0),:) * tmask(A2D(0),:)

            CALL lbc_lnk( 'sabssm', uu(:,:,:,1)       , 'U',  -1.0_wp, &
                                 &  vv(:,:,:,1)       , 'V',  -1.0_wp, &
                                 &  ts(:,:,:,jp_tem,1), 'T',   1.0_wp  )

            CALL iom_put( 'uoce' , uu(:,:,:,1) )
            CALL iom_put( 'voce' , vv(:,:,:,1) )
            CALL iom_put( 'toce' , ts(:,:,:,jp_tem,1) )

         ENDIF

         !! sea ice fields
#if defined key_si3
         ! Assigns forcings to forcing arrays
         ! No default value set when 'NOT USED' used as file name
         ! => the user can easily provide a dummy inpout file
         at_i(A2D(0)) = sf_ssm_2d(jf_at_i)%fnow(A2D(0),1) * tmask(A2D(0),1)    ! frac ice in cell 
         vt_i(A2D(0)) = sf_ssm_2d(jf_vt_i)%fnow(A2D(0),1) * tmask(A2D(0),1)    ! frac ice in cell 

         u_ice(A2D(0)) = sf_ssm_2d(jf_uice)%fnow(A2D(0),1) * umask(A2D(0),1)    ! sea surface u velocity
         v_ice(A2D(0)) = sf_ssm_2d(jf_vice)%fnow(A2D(0),1) * vmask(A2D(0),1)    ! sea surface v velocity

         CALL lbc_lnk( 'sabssm', u_ice, 'U', -1.0_wp, &
                              &  v_ice, 'V', -1.0_wp, &
                              &  at_i , 'T',  1.0_wp, &
                              &  vt_i , 'T',  1.0_wp, ldfull = .TRUE. )
        
         ! Output sea ice forcings              
         CALL iom_put( 'uice', u_ice )
         CALL iom_put( 'vice', v_ice )
         CALL iom_put( 'iceconc', at_i(A2D(0)) )
         CALL iom_put( 'icevolu', vt_i(A2D(0)) )
#endif
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop( 'sab_ssm')
      !
   END SUBROUTINE sab_ssm

   SUBROUTINE sab_ssm_init( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE sab_ssm_init  ***
      !!
      !! ** Purpose :   Initialisation of sea surface mean data
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm   ! ocean time level indices
      ! (not needed for SAS but needed to keep a consistent interface in sbcmod.F90)
      INTEGER  :: ierr, ierr0, ierr1                 ! return error code
      INTEGER  :: ifpr                               ! dummy loop indice
      INTEGER  :: inum, idv, idimv, jpm              ! local integer
      INTEGER  ::   ios                              ! Local integer output status for namelist read
      !!
      CHARACTER(len=100)                     ::  cn_dir       ! Root directory for location of core files
      TYPE(FLD_N), ALLOCATABLE, DIMENSION(:) ::  slf_3d       ! array of namelist information on the fields to read
      TYPE(FLD_N), ALLOCATABLE, DIMENSION(:) ::  slf_2d       ! array of namelist information on the fields to read
      TYPE(FLD_N) ::   sn_sst, sn_ssh, sn_sss, sn_fr_i        ! information about the fields to be read
      TYPE(FLD_N) ::   sn_ssu, sn_ssv
      TYPE(FLD_N) ::   sn_utau, sn_vtau
      TYPE(FLD_N) ::   sn_uice, sn_vice, sn_at_i, sn_vt_i
      TYPE(FLD_N) ::   sn_uu, sn_vv, sn_t3D
      !!
      NAMELIST/namsab/ ln_sabread, cn_dir,                     &
         &                 sn_sst, sn_ssh, sn_sss, sn_fr_i,    &
         &                 sn_ssu, sn_ssv, sn_utau, sn_vtau,   &
         &                 sn_uice, sn_vice, sn_at_i, sn_vt_i, & ! sea-ice fields for icebergs
         &                 sn_uu, sn_vv, sn_t3D                    ! 3D fields needed for icb with ln_M2016
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'sab_ssm_init : sea surface data reading from files '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !
      READ_NML_REF(numnam,namsab)
      READ_NML_CFG(numnam,namsab)
      IF(lwm) WRITE ( numond, namsab )
      !
      IF(lwp) THEN                              ! Control print
         WRITE(numout,*) '   Namelist namsab'
         WRITE(numout,*) '      Initialisation using an input file:           ln_sabread = ', ln_sabread
         WRITE(numout,*) '      Are we supplying a 3D uu, vv and ts(t) field: ln_M2016   = ', ln_M2016
      ENDIF
      !
      IF( ln_sabread ) THEN                       ! store namelist information in an array
         !
         !   ! by-hand declaration of fields filled by standalone sab
         jf_ssh  = 1 ; jf_sst  = 3   ! default 2D fields index
         jf_sss  = 2 ; jf_fr_i = 4  
         jf_ssu  = 5 ; jf_ssv  = 6
         jf_utau = 7 ; jf_vtau = 8
         !
         jf_uice = 9 ; jf_vice = 10
         jf_at_i = 11; jf_vt_i = 12
         !
         nfld_2d = 12      ! number of 2D fields to read
         !
         ALLOCATE( slf_2d(nfld_2d), STAT=ierr )         ! set slf structure
         IF( ierr > 0 ) THEN
            CALL ctl_stop( 'sab_ssm_init: unable to allocate slf 2d structure' )   ;   RETURN
         ENDIF
         !
         slf_2d(jf_ssh)  = sn_ssh     ; slf_2d(jf_sst)  = sn_sst   ; slf_2d(jf_sss) = sn_sss
         slf_2d(jf_fr_i) = sn_fr_i    ; slf_2d(jf_ssu)  = sn_ssu   ; slf_2d(jf_ssv) = sn_ssv 
         slf_2d(jf_utau) = sn_utau    ; slf_2d(jf_vtau) = sn_vtau
         slf_2d(jf_uice) = sn_uice    ; slf_2d(jf_vice) = sn_vice
         slf_2d(jf_at_i) = sn_at_i    ; slf_2d(jf_vt_i) = sn_vt_i

         ALLOCATE( sf_ssm_2d(nfld_2d), STAT=ierr )         ! set sf structure
         IF( ierr > 0 ) THEN
            CALL ctl_stop( 'sab_ssm_init: unable to allocate sf 2d structure' )   ;   RETURN
         ENDIF
         DO ifpr = 1, nfld_2d
            ierr1 = 0 ; ierr0 = 0
            ALLOCATE( sf_ssm_2d(ifpr)%fnow(jpi,jpj,1)    , STAT=ierr0 )
            IF( slf_2d(ifpr)%ln_tint )   ALLOCATE( sf_ssm_2d(ifpr)%fdta(jpi,jpj,1,2)  , STAT=ierr1 )
            IF( ierr0 + ierr1 > 0 ) THEN
               CALL ctl_stop( 'sab_ssm_init : unable to allocate sf_ssm_2d array structure' )   ;   RETURN
            ENDIF
         END DO
         !
         CALL fld_fill( sf_ssm_2d, slf_2d, cn_dir, 'sab_ssm_init', '2D Data in file', 'namsab_ssm' )
         !
         ! setting grid type and sign for fields defined on U and V grids
         sf_ssm_2d(jf_ssu)%cltype  = 'U' ; sf_ssm_2d(jf_ssu)%zsgn  = -1._wp
         sf_ssm_2d(jf_ssv)%cltype  = 'V' ; sf_ssm_2d(jf_ssv)%zsgn  = -1._wp

         sf_ssm_2d(jf_utau)%cltype = 'U' ; sf_ssm_2d(jf_utau)%zsgn = -1._wp  
         sf_ssm_2d(jf_vtau)%cltype = 'V' ; sf_ssm_2d(jf_vtau)%zsgn = -1._wp
          
         sf_ssm_2d(jf_uice)%cltype = 'U' ; sf_ssm_2d(jf_uice)%zsgn = -1._wp
         sf_ssm_2d(jf_vice)%cltype = 'V' ; sf_ssm_2d(jf_vice)%zsgn = -1._wp
         !
         DEALLOCATE( slf_2d, STAT=ierr )
         !
         ! initialising nfld_3d in default case:
         nfld_3d  = 0
         !
         IF( ln_M2016 ) THEN
            ! In case ln_M2016, 3d u, v and temperature are needed
            !
            nfld_3d = 3   ! number of 3D fields to read
            !
            jf_uu = 1   ;   jf_vv = 2   ; jf_t3D = 3      ! define 3D fields index
            !
            ALLOCATE( slf_3d(nfld_3d), STAT=ierr )         ! set slf structure
            IF( ierr > 0 ) THEN
               CALL ctl_stop( 'sab_ssm_init: unable to allocate slf 3d structure' )   ;   RETURN
            ENDIF
            !
            slf_3d(jf_uu)  = sn_uu
            slf_3d(jf_vv)  = sn_vv
            slf_3d(jf_t3D) = sn_t3D
            !
            ! allocating data structure for 3D case:
            ALLOCATE( sf_ssm_3d(nfld_3d), STAT=ierr )         ! set sf structure
            IF( ierr > 0 ) THEN
               CALL ctl_stop( 'sab_ssm_init: unable to allocate sf structure' )   ;   RETURN
            ENDIF
            DO ifpr = 1, nfld_3d
               ierr1 = 0; ierr0 = 0
               ALLOCATE( sf_ssm_3d(ifpr)%fnow(jpi,jpj,jpk)    , STAT=ierr0 )
               IF( slf_3d(ifpr)%ln_tint ) ALLOCATE( sf_ssm_3d(ifpr)%fdta(jpi,jpj,jpk,2), STAT=ierr1 )
               IF( ierr0 + ierr1 > 0 ) THEN
                  CALL ctl_stop( 'sab_ssm_init : unable to allocate sf_ssm_3d array structure' )   ;   RETURN
               ENDIF
            END DO
            !                                         ! fill sf with slf_i and control print
            CALL fld_fill( sf_ssm_3d, slf_3d, cn_dir, 'sab_ssm_init', '3D Data in file', 'namsab_ssm' )
            sf_ssm_3d(jf_uu)%cltype  = 'U';   sf_ssm_3d(jf_uu)%zsgn = -1._wp
            sf_ssm_3d(jf_vv)%cltype  = 'V';   sf_ssm_3d(jf_vv)%zsgn = -1._wp
            sf_ssm_3d(jf_t3D)%cltype = 'T';

            ! in offline mode, we don't take into account e3t variation
            ! we assume input data are gridded on a constant grid
            r3t(:,:,1) = 0.0_wp

            DEALLOCATE( slf_3d, STAT=ierr )

         END IF
         !
      ELSE
         ! Analytical formulation (to be customised if needed).
         !
         ssh_m(:,:)    = 34._wp
         sst_m(:,:)    = -1._wp
         sss_m(:,:)    =  0._wp
         fr_i(:,:)     =  0._wp
         ssu_m(:,:)    =  0._wp
         ssv_m(:,:)    =  0._wp
         utau_icb(:,:) =  0._wp
         vtau_icb(:,:) =  0._wp

#if defined key_si3
         u_ice(:,:)    =  0._wp
         v_ice(:,:)    =  0._wp
         at_i(:,:)     =  0._wp
         vt_i(:,:)     =  0._wp
#endif

         IF (ln_M2016) THEN
            uu(:,:,:,:) = 0._wp
            vv(:,:,:,:) = 0._wp
            ts(:,:,:,jp_tem,:) = -1._wp
            r3t(:,:,1)  = 0._wp
         ENDIF

      ENDIF
      !
      ! The ssh read is already the dynamical ssh, set snwice_mass to 0
      snwice_mass   = 0._wp
      snwice_mass_b = 0._wp 
      !
      ! these 3 lines should not be needed with SAB as emp is not computed by SAB but only the icb contribution
      ! best update emp outside ICB and set it in sbcmod to simplify the logic (specific ticket to open)
      ! reset emp and qns to 0
      qns(:,:)   = 0._wp
      emp(:,:)   = 0._wp 
      !
   END SUBROUTINE sab_ssm_init


END MODULE sabssm       
