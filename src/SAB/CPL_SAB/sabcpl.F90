MODULE sabcpl
   !!=============================================================================
   !!                       ***  MODULE  icbcpl  ***
   !! Initialisation of all fields needed for ICB to run in OASIS, outside of NEMO
   !!=============================================================================
   !!   namsab_cpl      : coupled formulation namlist
   !!   sab_cpl_init    : initialisation of the coupled exchanges (all fields received and send through oasis) 
   !!   sab_cpl_rcv     : receive fields from NEMO (ssh,sst, sss, ssu,ssv, fr_i ...) 
   !!   sab_cpl_snd     : send fields to NEMO (fresh water flx, heat flux)
   !!----------------------------------------------------------------------
 
   USE par_oce                             ! ocean parameters
   USE oce,    ONLY: ts, uu, vv
   USE dom_oce                             ! ocean domain
   USE in_out_manager                      ! IO parameters
   USE lib_mpp                             ! MPI code and lk_mpp in particular
   USE icb_oce                             ! define iceberg arrays
   USE sbc_oce                             ! ocean surface boundary conditions
   USE iom                                 ! IOM library
   USE fldread                             ! field read
   USE lbclnk                              ! lateral boundary condition - MPP link
   USE cpl_oasis3     ! OASIS3 coupling
   !
#if defined key_si3
   USE ice,     ONLY: u_ice, v_ice, at_i, vt_i  ! SI3 variables
   USE icevar                                   ! ice_var_sshdyn
   USE sbc_ice, ONLY: snwice_mass, snwice_mass_b
#endif


#if defined key_oasis3
   USE mod_oasis, ONLY : OASIS_Sent, OASIS_ToRest, OASIS_SentOut, OASIS_ToRestOut
#endif

# define sabrcv srcv(midsab)%fld 
# define sabsnd ssnd(midsab)%fld

   IMPLICIT NONE

   PUBLIC   sab_cpl_init  ! routine called in nemogcm.F90 module, through       icb_ini (modified version, forcément)
   PUBLIC   sab_cpl_alloc
   PUBLIC   sab_cpl_rcv       ! routine called in step_sab  
   PUBLIC   sab_cpl_snd       ! routine called in step_sab  

   !!----------------------------------------------------------------------
   !! NEMO/SAB 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !! 1)  DEFINE THE "jpr" or "jps" - like indexes of all exchanged fields.
   

   !! fields SENT by SAB
   !! are only in the interior (without halos)
  
   INTEGER, PARAMETER ::   jps_bgwf    =  1   ! iceberg fresh water flux (to be added to emp)
   INTEGER, PARAMETER ::   jps_bghf    =  2   ! iceberg fresh heat flux  (to be added to qns)

   INTEGER, PARAMETER ::   jpsnd_sab   =  2   ! total number of snt fields
 
   !! fields received by SAB
   !! are only in the interior (without halos)

   INTEGER, PARAMETER ::   jpr_ssh   =  1   ! sea surface height
   INTEGER, PARAMETER ::   jpr_sst   =  2   ! sea surface temperature
   INTEGER, PARAMETER ::   jpr_sss   =  3   ! sea surface salinity
   INTEGER, PARAMETER ::   jpr_fri   =  4   ! ice fraction 
   INTEGER, PARAMETER ::   jpr_ati   =  5   ! ice total fractional area
   INTEGER, PARAMETER ::   jpr_vti   =  6   ! ice volume per unit area
   INTEGER, PARAMETER ::   jpr_r3t   =  7   ! ssh/h_0 ratio
   INTEGER, PARAMETER ::   jpr_ssu   =  8   ! sea surface x velocity
   INTEGER, PARAMETER ::   jpr_utau  =  9   ! x wind stress
   INTEGER, PARAMETER ::   jpr_uice  =  10  ! ice x velocity
   INTEGER, PARAMETER ::   jpr_ssv   =  11  ! sea surface y velocity
   INTEGER, PARAMETER ::   jpr_vtau  =  12  ! y wind stress
   INTEGER, PARAMETER ::   jpr_vice  =  13  ! ice y velocity
   ! ocean 3D files (needed in icb_utl)
   INTEGER, PARAMETER ::   jpr_uu    =  15  ! 3D x velocity
   INTEGER, PARAMETER ::   jpr_vv    =  16  ! 3D y velocity
   INTEGER, PARAMETER ::   jpr_tt    =  17  ! 3D temperature
  

   INTEGER, PARAMETER ::   jprcv_sab =  17  ! max total number of sent fields

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


    INTEGER FUNCTION sab_cpl_alloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION sab_cpl_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      ! allocating the sending buffer only for the activated fields
      DO jn = 1, jpsnd_sab
         IF( sabsnd(jn)%laction ) ALLOCATE( sabsnd(jn)%z3(jpi,jpj,sabsnd(jn)%nlvl), STAT=ierr )
         sab_cpl_alloc = MAX(ierr,0)
      END DO
      

      ! allocating the reception buffer only for the activated fields
      DO jn = 1, jprcv_sab
         IF( sabrcv(jn)%laction ) ALLOCATE( sabrcv(jn)%z3(jpi,jpj,sabrcv(jn)%nlvl), STAT=ierr )
         sab_cpl_alloc = sab_cpl_alloc + MAX(ierr,0)
      END DO

      ALLOCATE( xcplmask(A2D(0),1,0:nn_cplmodel) , STAT=ierr )
      !
    END FUNCTION sab_cpl_alloc


    SUBROUTINE sab_cpl_init() !(maybe input arg ? dontknow yet)
    !!----------------------------------------------------------------------
    !!             ***  ROUTINE sab_cpl_init  ***
    !!
    !! ** Purpose :   Initialisation of send and received information from
    !!                the icb component
    !!
    !! ** Method  : * Read namsab_cpl namelist (to be written in the future ^^)
    !!              * define the receive interface
    !!              * define the send    interface
    !!              * initialise the OASIS coupler
    !!----------------------------------------------------------------------

    INTEGER ::   inum, jn   ! Local integer
    CHARACTER(LEN=64) ::   zclname
    
    IF (lwp) WRITE(numout,*) "sab_cpl_init : def of rcv + snd structures for NEMO-SAB coupling "
    IF (lwp) WRITE(numout,*) "               nb of coupled zooms = ", (nn_cplmodel-1)

    ! ================================ !
      !   Define the receive interface   !
      ! ================================ !
      ! MAYBE USEFUL ? nrcvinfo(:) = OASIS_idle   ! needed by nrcvinfo(jpr_otx1) if we do not receive ocean stress

      ! for each field
   ! LOGICAL               ::   laction   ! To be coupled or not
   ! CHARACTER(len = 8)    ::   clname    ! Field alias used by OASIS
   ! CHARACTER(len = 1)    ::   clgrid    ! Grid type
   ! REAL(wp)              ::   nsgn      ! Control of the sign change
   ! INTEGER               ::   nlvl      ! Number of grid level to exchange, set 1 for 2D fields

      ! -------------------------------- 
      ! DEFINING sending interface
      ! default definitions of ssnd
      
      ALLOCATE( sabsnd(jpsnd_sab) )
      sabsnd(:)%laction = .FALSE. ; sabsnd(:)%clgrid = 'T' ;   sabsnd(:)%nsgn      = 1.
      sabsnd(:)%nct     = 1       ; sabsnd(:)%nlvl   = 1   ;   sabsnd(:)%ncplmodel = 1
      
      !a) berg heat + fresh water flux 
      sabsnd(jps_bgwf)%clname = 'berg_wfx'   ! iceberg water flux
      sabsnd(jps_bghf)%clname = 'berg_hcfx'  ! iceberg heat flux
      DO jn = 1, jpsnd_sab
         zclname           = 'sab_'//TRIM(sabsnd(jn)%clname)
         sabsnd(jn)%clname = TRIM(zclname)
      ENDDO

      IF (.NOT. ln_passive_mode ) &
          sabsnd(jps_bgwf:jps_bghf)%laction = .TRUE. 
      
      ! --------------------------------
      ! DEFINING receiving interface
      ! default definitions of srcv
      ALLOCATE( sabrcv(jprcv_sab) )
      sabrcv(:)%laction = .FALSE. ; sabrcv(:)%clgrid = 'T' ; sabrcv(:)%nsgn      = 1.
      sabrcv(:)%nct     = 1       ; sabrcv(:)%nlvl   = 1   ; sabrcv(:)%ncplmodel = nn_cplmodel
      ! 1) Sea- surface fields + sea-ice fields

      ! a) with sea-ice T-grid Bundle
      sabrcv(jpr_ssh)%clname = 'ssh'
      sabrcv(jpr_sst)%clname = 'sst'
      sabrcv(jpr_sss)%clname = 'sss'
      sabrcv(jpr_fri)%clname = 'fr_i'
      sabrcv(jpr_ssh:jpr_fri)%laction = .TRUE.

      sabrcv(jpr_ati)%clname = 'at_i'
      sabrcv(jpr_vti)%clname = 'vt_i'

      ! b) with sea-ice U-grid Bundle
      sabrcv(jpr_ssu)%clname  = 'ssu'
      sabrcv(jpr_utau)%clname = 'utau'
      sabrcv(jpr_ssu:jpr_utau)%laction = .TRUE.

      sabrcv(jpr_uice)%clname = 'u_ice'
      sabrcv(jpr_ssu:jpr_uice)%clgrid  = 'U'
      sabrcv(jpr_ssu:jpr_uice)%nsgn    = -1 !change of sign at north fold !

      ! c) with sea-ice V-grid Bundle
      sabrcv(jpr_ssv)%clname  = 'ssv'
      sabrcv(jpr_vtau)%clname = 'vtau'
      sabrcv(jpr_ssv:jpr_vtau)%laction = .TRUE.

      sabrcv(jpr_vice)%clname = 'v_ice'
      sabrcv(jpr_ssv:jpr_vice)%clgrid  = 'V'
      sabrcv(jpr_ssv:jpr_vice)%nsgn    = -1 !change of sign at north fold !

#if defined key_si3 
      ! Additional coupling fields if ice
      sabrcv(jpr_ati:jpr_vti)%laction = .TRUE.
      sabrcv(jpr_uice)%laction = .TRUE.
      sabrcv(jpr_vice)%laction = .TRUE.
      IF (lwp) THEN
          WRITE(numout,*) ""
          WRITE(numout,*) " icb_cpl_init : including SI3 "
          WRITE(numout,*) " W A R N I N G : icb_at_i, icb_vt_i, icb_u_ice, icb_v_ice must be defined in namcouple, otherwise coupling will crash"
      ENDIF
#endif
      
      !2) 3D ocean fields for Merino 2016's option (+ grounding) 

      ! 3D field uu
      sabrcv(jpr_uu)%clname = 'uu_3D'
      sabrcv(jpr_uu)%clgrid = 'U'
      sabrcv(jpr_uu)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_uu)%nlvl   = nlvlsab_cpl

      ! 3D field vv
      sabrcv(jpr_vv)%clname = 'vv_3D'
      sabrcv(jpr_vv)%clgrid = 'V'
      sabrcv(jpr_vv)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_vv)%nlvl   = nlvlsab_cpl

      ! 3D field ts( only temp)
      sabrcv(jpr_tt)%clname = 'tt_3D'
      sabrcv(jpr_tt)%clgrid = 'T'
      sabrcv(jpr_tt)%nlvl   = nlvlsab_cpl

      ! 2D field r3t (e3t ~ r3t * e3t_0, so only r3t is sent, see domzgr_substitute.h90)    
      sabrcv(jpr_r3t)%clname = 'r3t'

      ! index OASIS namcouple variable name with icb ID
      DO jn = 1, jprcv_sab
         zclname           = 'sab_'//TRIM(sabrcv(jn)%clname)
         sabrcv(jn)%clname = TRIM(zclname)
      ENDDO

      IF( ln_M2016 ) THEN
         IF (lwp) THEN
            WRITE(numout,*) ""
            WRITE(numout,*) " sab_cpl_init : ln_M2016 = ",  ln_M2016
            WRITE(numout,*) " W A R N I N G : sab_r3t, sab_uu, sab_vv and sab_tt must be defined in namcouple, otherwise coupling will crash"
         ENDIF
         !
         sabrcv(jpr_uu)%laction  = .TRUE.
         sabrcv(jpr_vv)%laction  = .TRUE.
         sabrcv(jpr_tt)%laction  = .TRUE.
         sabrcv(jpr_r3t)%laction = .TRUE.

         ! initialising uu, vv, ts to zero once for all (security, normally it's useless if .NOT. ln_M2016)  
         uu(:,:,:,:) = 0._wp
         vv(:,:,:,:) = 0._wp
         ts(:,:,:,:,:) = 0._wp

      ENDIF

      ! =================================== !
      !   define variables for the coupler  !
      ! =================================== !
      CALL cpl_vardef(midsab)  !! " 1 " stands for number of models to couple with 

      ! CHECKING and allocating the 'z3' buffers to send and receive data
      IF( sab_cpl_alloc() /= 0 )  CALL ctl_stop( 'STOP', 'sab_cpl_alloc : unable to allocate arrays' )
      
      WRITE (numout,*) " sab_cpl_alloc : normal end of initialization, all arrays allocated correctly ! "

      IF( nn_cplmodel > 1 ) THEN
         CALL iom_open( 'icb_cplmask', inum )
         CALL iom_get( inum, jpdom_unknown, 'cplmask', xcplmask(A2D(0),1,1:nn_cplmodel),   &
            &          kstart = (/ mig(Nis0,0),mjg(Njs0,0),1 /), kcount = (/ Ni_0,Nj_0,nn_cplmodel /) )
         CALL iom_close( inum )
         xcplmask(A2D(0),1,0) = 1. - SUM( xcplmask(A2D(0),1,1:nn_cplmodel), dim = 3 )
      ELSE
         xcplmask(A2D(0),1,:) = 1.
      ENDIF

    END SUBROUTINE sab_cpl_init
     

    SUBROUTINE sab_cpl_rcv( kt )

      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sab_cpl_rcv  ***
      !!
      !! ** Purpose : receive all the activated coupled fields from NEMO
      !! ** Method  : use cpl_oasis interface (by A. Barge)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      !
      INTEGER :: isec, info, jn                       ! local integer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('sab_cpl_rcv')
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !
      ! ==========================
      !   Proceed all activated receptions
      ! ==========================
      !

        DO jn = 1, jprcv_sab
         IF( sabrcv(jn)%laction ) THEN
            CALL cpl_rcv( midsab, jn, isec, sabrcv(jn)%z3(A2D(0),1:sabrcv(jn)%nlvl), info, xcplmask(A2D(0),1:1,1:nn_cplmodel))
         ENDIF
        END DO

       ! Extract received fields :
       ! ATTENTION : keep the same order for receiving fields as the one used in NEMO for sending fields (for perfo)  
      
       !1) sea surface 2D fields :  
       ssh_m(A2D(0))    = sabrcv(jpr_ssh)%z3(A2D(0),1)
       sst_m(A2D(0))    = sabrcv(jpr_sst)%z3(A2D(0),1)
       sss_m(A2D(0))    = sabrcv(jpr_sss)%z3(A2D(0),1)
       fr_i(A2D(0))     = sabrcv(jpr_fri)%z3(A2D(0),1) 

       ssu_m(A2D(0))    = sabrcv(jpr_ssu)%z3(A2D(0),1)
       ssv_m(A2D(0))    = sabrcv(jpr_ssv)%z3(A2D(0),1)

       utau_icb(A2D(0)) = sabrcv(jpr_utau)%z3(A2D(0),1)
       vtau_icb(A2D(0)) = sabrcv(jpr_vtau)%z3(A2D(0),1)

       !2) sea-ice related fields :        
#if defined key_si3
       u_ice(A2D(0))    = sabrcv(jpr_uice)%z3(A2D(0),1)
       v_ice(A2D(0))    = sabrcv(jpr_vice)%z3(A2D(0),1)
       at_i(A2D(0))     = sabrcv(jpr_ati)%z3(A2D(0),1)
       vt_i(A2D(0))     = sabrcv(jpr_vti)%z3(A2D(0),1)
#endif

       !3) IF ln_M2016 : receive uu, vv and ts(only temp)

       IF( ln_M2016 ) THEN
           ! important to fill uu (resp vv + ts) only from z = 1 to z = uu%nlvl (critical if ln_cut_z700M
           uu(A2D(0),1:sabrcv(jpr_uu)%nlvl, Nbb)         =  sabrcv(jpr_uu)%z3(A2D(0),1:sabrcv(jpr_uu)%nlvl)
           vv(A2D(0),1:sabrcv(jpr_vv)%nlvl, Nbb)         =  sabrcv(jpr_vv)%z3(A2D(0),1:sabrcv(jpr_vv)%nlvl)
           ts(A2D(0),1:sabrcv(jpr_tt)%nlvl, jp_tem, Nbb) =  sabrcv(jpr_tt)%z3(A2D(0),1:sabrcv(jpr_tt)%nlvl)
           !
           r3t(A2D(0),Nbb) =  sabrcv(jpr_r3t)%z3(A2D(0),1)

       ENDIF

       CALL lbc_lnk( 'sabcpl', ssh_m, 'T', 1.0_wp, &
                            &  sst_m, 'T', 1.0_wp, &
                            &  sss_m, 'T', 1.0_wp, &
                            &  fr_i , 'T', 1.0_wp, &
                            &  ssu_m, 'U', -1.0_wp, &
                            &  ssv_m, 'V', -1.0_wp, &
                            &  utau_icb, 'U', -1.0_wp, &
                            &  vtau_icb, 'V', -1.0_wp, ldfull = .TRUE. )

#if defined key_si3
       CALL lbc_lnk( 'sabcpl', u_ice, 'U', -1.0_wp, &
                            &  v_ice, 'V', -1.0_wp, &
                            &  at_i, 'T', 1.0_wp,   &
                            &  vt_i, 'T', 1.0_wp, ldfull = .TRUE. )
#endif

       IF( ln_M2016 ) THEN
            CALL lbc_lnk( 'sabcpl', uu(:,:,:,Nbb), 'U', -1.0_wp, &
                                 &  vv(:,:,:,Nbb), 'V', -1.0_wp, ldfull = .TRUE.)
            CALL lbc_lnk( 'sabcpl',  ts(:,:,:,jp_tem,Nbb),'T', 1.0_wp, ldfull = .TRUE. )
            CALL lbc_lnk( 'sabcpl', r3t(:,:,Nbb),   'T', 1.0_wp, ldfull = .TRUE. )
            !
       ENDIF

       IF( ln_timing )   CALL timing_stop('sab_cpl_rcv')

   END SUBROUTINE sab_cpl_rcv
  !
  !
   SUBROUTINE sab_cpl_snd( kt )

      !!----------------------------------------------------------------------
      !!             ***  ROUTINE sab_cpl_snd  ***
      !!
      !! ** Purpose : send fresh water flux and heat flux of icebergs from SAB to NEMO
      !! ** Method  : use cpl_oasis interface (by A. Barge)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      INTEGER             :: isec, info, jn  ! local integer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('sab_cpl_snd')
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !

      ! =============
      ! fill sendings buffer with fresh water and heatflux
      IF (.NOT. ln_passive_mode ) THEN  
         sabsnd(jps_bgwf)%z3(A2D(0),1) = berg_grid%floating_melt(A2D(0))  
         sabsnd(jps_bghf)%z3(A2D(0),1) = berg_grid%calving_hflx(A2D(0))  ! contains the icebergs melting heat flux (name is  misleading, to be fixed in icbthm in future ticket !
      ENDIF

      ! ==========================
      !   Proceed sendings
      !   (currently only 2 fields, but keep this DOloop syntax, if we want to transfer more fields)
      ! ==========================
      !
      DO jn = 1, jpsnd_sab
         IF ( sabsnd(jn)%laction ) THEN
            CALL cpl_snd( midsab, jn, isec, sabsnd(jn)%z3(A2D(0),1:sabsnd(jn)%nlvl), info )
         ENDIF
      END DO

      IF( ln_timing )   CALL timing_stop('sab_cpl_snd')

    END SUBROUTINE sab_cpl_snd

END MODULE sabcpl
