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
   
   INTEGER, PARAMETER ::   jps_berg_fx   =  1   ! surface fresh water flux + heat flux sent to nemo (to be added to emp,qns)
   INTEGER, PARAMETER ::   jpsnd_sab = 1 ! total number of received fields
  
   !! fields received by SAB
   !! are only in the interior (without halos)
  
   !! 2D sea surface fields
   INTEGER, PARAMETER ::   jpr_ss_T   =  1   ! sea surface state grid T fields (ssh, sst, sss_fr_i) 
   INTEGER, PARAMETER ::   jpr_ss_U   =  2   ! sea surface state grid U fields (ssu_m, utau_icb)
   INTEGER, PARAMETER ::   jpr_ss_V   =  3   ! sea surface state grid V fields (ssv_m, vtau_icb)
   
   ! ocean 3D fields (needed in icb_utl)

   INTEGER, PARAMETER ::   jpr_Uu_oce  =  4   ! oce Uu velocity
   INTEGER, PARAMETER ::   jpr_Vv_oce  =  5   ! oce Vv velocity
   INTEGER, PARAMETER ::   jpr_Tt_oce  =  6   ! oce Ts (only 3D T)

   INTEGER, PARAMETER ::   jprcv_sab = 6 ! total number of sent fields
   
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
    
    IF (lwp) WRITE(numout,*) "sab_cpl_init : def of rcv + snd structures for NEMO-SAB coupling "
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
      sabsnd(:)%laction = .FALSE.   ;   sabsnd(:)%clgrid = 'T'   ;   sabsnd(:)%nsgn = 1.
      sabsnd(:)%nct = 1   ;   sabsnd(:)%nlvl = 1  ;   sabsnd(:)%ncplmodel = 1
      
      !a) berg heat + fresh water flux 
      sabsnd(jps_berg_fx)%clname = 'Bberg_fx'
      IF (.NOT. ln_passive_mode ) THEN 
          sabsnd(jps_berg_fx)%laction = .TRUE. 
          sabsnd(jps_berg_fx)%nlvl = 2  ! bundle 2D on 2 levels : fresh water flux, heat flux
      ENDIF 
      
      ! --------------------------------
      ! DEFINING receiving interface
      ! default definitions of srcv
      ALLOCATE( sabrcv(jprcv_sab) )
      sabrcv(:)%laction = .FALSE.   ;   sabrcv(:)%clgrid = 'T'   ;   sabrcv(:)%nsgn = 1.
      sabrcv(:)%nct = 1   ;   sabrcv(:)%nlvl = 1   ; sabrcv(:)%ncplmodel = 1
#if defined key_si3  
      ! 1) Sea- surface fields + sea-ice fields inside the same bundles 
      
      ! a) with sea-ice T-grid Bundle
      sabrcv(jpr_ss_T)%clname = 'B_ss_T'
      sabrcv(jpr_ss_T)%laction = .TRUE. 
      sabrcv(jpr_ss_T)%nlvl = 6     ! ssh,sst,sss,fr_i,at_i,vt_i

      ! b) with sea-ice U-grid Bundle
      sabrcv(jpr_ss_U)%clname = 'B_ss_U'
      sabrcv(jpr_ss_U)%laction = .TRUE.
      sabrcv(jpr_ss_U)%clgrid = 'U'
      sabrcv(jpr_ss_U)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_ss_U)%nlvl   = 3  ! utau_icb + ssu, u_ice

      ! c) with sea-ice V-grid Bundle
      sabrcv(jpr_ss_V)%clname = 'B_ss_V'
      sabrcv(jpr_ss_V)%laction = .TRUE.
      sabrcv(jpr_ss_V)%clgrid = 'V'
      sabrcv(jpr_ss_V)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_ss_V)%nlvl   = 3 ! vtau_icb, ssv, v_ice  !
#else
      ! 1 bis) only sea surf fields
      
      ! a) no sea-ice T-grid Bundle 
      sabrcv(jpr_ss_T)%clname = 'B_ss_T'
      sabrcv(jpr_ss_T)%laction = .TRUE.
      sabrcv(jpr_ss_T)%nlvl = 4     ! ssh,sst,sss,fr_i
    
      ! b) no sea-ice U-grid Bundle
      sabrcv(jpr_ss_U)%clname = 'B_ss_U'
      sabrcv(jpr_ss_U)%laction = .TRUE.
      sabrcv(jpr_ss_U)%clgrid = 'U'
      sabrcv(jpr_ss_U)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_ss_U)%nlvl   = 2  ! utau_icb + ssu
     
      ! c) no sea-ice V-grid Bundle
      sabrcv(jpr_ss_V)%clname = 'B_ss_V'
      sabrcv(jpr_ss_V)%laction = .TRUE.
      sabrcv(jpr_ss_V)%clgrid = 'V'
      sabrcv(jpr_ss_V)%nsgn   = -1 !change of sign at north fold !
      sabrcv(jpr_ss_V)%nlvl   = 2 ! vtau_icb, ssv 
#endif
      
      !2) 3D ocean fields for Merino 2016's option (+ grounding) 
      IF( ln_M2016 ) THEN
              ! 3D field uu
              sabrcv(jpr_Uu_oce)%clname = 'B_Uu_3D'
              sabrcv(jpr_Uu_oce)%laction = .TRUE.
              sabrcv(jpr_Uu_oce)%clgrid = 'U'
              sabrcv(jpr_Uu_oce)%nsgn   = -1 !change of sign at north fold !
              sabrcv(jpr_Uu_oce)%nlvl   = nlvlsab_cpl 
             
              ! 3D field vv
              sabrcv(jpr_Vv_oce)%clname = 'B_Vv_3D'
              sabrcv(jpr_Vv_oce)%laction = .TRUE.
              sabrcv(jpr_Vv_oce)%clgrid = 'V'
              sabrcv(jpr_Vv_oce)%nsgn   = -1 !change of sign at north fold !
              sabrcv(jpr_Vv_oce)%nlvl   = nlvlsab_cpl              
             
              ! 3D field ts( only temp)
              sabrcv(jpr_Tt_oce)%clname = 'B_Tt_3D'
              sabrcv(jpr_Tt_oce)%laction = .TRUE.
              sabrcv(jpr_Tt_oce)%clgrid = 'T'
              sabrcv(jpr_Tt_oce)%nlvl   = nlvlsab_cpl
              ! 
              !2D field r3t (e3t ~ r3t * e3t_0, so only r3t is sent, see domzgr_substitute.h90)
               sabrcv(jpr_ss_T)%nlvl = 7    ! ss_Tgrid bundle: ssh,sst,sss,fr_i,at_i,vt_i AND r3t

               ! WARNING : there might be a problem if there is no sea-ice 
      ELSE
              sabrcv(jpr_Uu_oce)%clname = 'B_Uu_3D'
              sabrcv(jpr_Vv_oce)%clname = 'B_Vv_3D'
              sabrcv(jpr_Tt_oce)%clname = 'B_Tt_3D'
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
            CALL cpl_rcv( midsab, jn, isec, sabrcv(jn)%z3(A2D(0),1:sabrcv(jn)%nlvl), info)
         ENDIF
        END DO

       ! Extract received fields :
       ! ATTENTION : keep the same order for receiving fields as the one used in NEMO for sending fields (for perfo)  
      
       !1) sea surface 2D fields :  
       ssh_m(A2D(0)) = sabrcv(jpr_ss_T)%z3(A2D(0),1)
       sst_m(A2D(0)) = sabrcv(jpr_ss_T)%z3(A2D(0),2)
       sss_m(A2D(0)) = sabrcv(jpr_ss_T)%z3(A2D(0),3)
       fr_i(A2D(0))  = sabrcv(jpr_ss_T)%z3(A2D(0),4) 

       ssu_m(A2D(0)) = sabrcv(jpr_ss_U)%z3(A2D(0),1)
       ssv_m(A2D(0)) = sabrcv(jpr_ss_V)%z3(A2D(0),1)

       utau_icb(A2D(0)) = sabrcv(jpr_ss_U)%z3(A2D(0),2)
       vtau_icb(A2D(0)) = sabrcv(jpr_ss_V)%z3(A2D(0),2)

       !2) sea-ice related fields :        
#if defined key_si3
       u_ice(A2D(0)) = sabrcv(jpr_ss_U)%z3(A2D(0),3)
       v_ice(A2D(0)) = sabrcv(jpr_ss_V)%z3(A2D(0),3)
       at_i(A2D(0)) = sabrcv(jpr_ss_T)%z3(A2D(0),5)
       vt_i(A2D(0)) = sabrcv(jpr_ss_T)%z3(A2D(0),6)
#endif

       !3) IF ln_M2016 : receive uu, vv and ts(only temp)

       IF( ln_M2016 ) THEN
           ! important to fill uu (resp vv + ts) only from z = 1 to z = uu%nlvl (critical if ln_cut_z700M
           uu(A2D(0),1:sabrcv(jpr_Uu_oce)%nlvl, Nbb) =  sabrcv(jpr_Uu_oce)%z3(A2D(0),1:sabrcv(jpr_Uu_oce)%nlvl)
           vv(A2D(0),1:sabrcv(jpr_Vv_oce)%nlvl, Nbb) =  sabrcv(jpr_Vv_oce)%z3(A2D(0),1:sabrcv(jpr_Vv_oce)%nlvl)
           ts(A2D(0),1:sabrcv(jpr_Tt_oce)%nlvl, jp_tem, Nbb) =  sabrcv(jpr_Tt_oce)%z3(A2D(0),1:sabrcv(jpr_Tt_oce)%nlvl)
           !
           r3t(A2D(0),Nbb) =  sabrcv(jpr_ss_T)%z3(A2D(0),7)

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
         sabsnd(jps_berg_fx)%z3(A2D(0),1) = berg_grid%floating_melt(A2D(0))  
         sabsnd(jps_berg_fx)%z3(A2D(0),2) = berg_grid%calving_hflx(A2D(0))  ! contains the icebergs melting heat flux (name is  misleading, to be fixed in icbthm in future ticket !
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
