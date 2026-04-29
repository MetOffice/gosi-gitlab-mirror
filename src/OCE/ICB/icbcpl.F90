MODULE icbcpl
   !!=============================================================================
   !!                       ***  MODULE  icbcpl  ***
   !! Initialisation of all fields needed for ICB to run in OASIS, outside of NEMO
   !!=============================================================================
   !!   icb_cpl_init    : initialisation of the coupled exchanges (all fields received and send through oasis) 
   !!   icb_cpl_rcv     : receive fields from NEMO (ssh,sst, sss, ssu,ssv, fr_i ...) 
   !!   icb_cpl_snd     : send fields to NEMO (fresh water flx, heat flux)
   !!----------------------------------------------------------------------
#if ! defined key_sab

# define icbrcv srcv(midicb)%fld 
# define icbsnd ssnd(midicb)%fld

! do not compile this module if component is sab (jp_iam_icb)
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
   USE icbini, ONLY: icb_nlvl700_cpl,icb_set_nlvlcpl ! set number of vertical lvls in 3D coupling fields  
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

   IMPLICIT NONE
   PUBLIC   icb_cpl_init  ! routine called in nemogcm.F90 module, through       icb_ini (modified version, forcément)
   PUBLIC   icb_cpl_alloc
   PUBLIC   icb_cpl_rcv       ! routine called in stprk3.F90 
   PUBLIC   icb_cpl_snd       ! routine called in stprk3.F90  

    CHARACTER(len=100)                                 ::   cn_dir = './'   !: Root directory for location of icb files
    TYPE(FLD_N)                                        ::   sn_icb          !: information about the calving file to be read
   !!----------------------------------------------------------------------
   !! NEMO/SAB 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !! 1)  DEFINE THE "jpr" or "jps" - like indexes of all exchanged fields.
   
   !! fields RECEIVED by NEMO (ocean)
   !! are only in the interior (without halos)
   
   INTEGER, PARAMETER ::   jpr_berg_fx   =  1   ! surface fresh water flux and heat flux sent (to be added to emp,qns)

   INTEGER, PARAMETER ::   jprcv_icb = 1 ! total number of received fields
  
   !! fields SENT by NEMO (ocean)
   !! are only in the interior (without halos)
  
   !! 2D sea surface fields
   INTEGER, PARAMETER ::   jps_ss_T   =  1   ! sea surface state grid T fields (ssh, sst, sss_fr_i) 
   INTEGER, PARAMETER ::   jps_ss_U   =  2   ! sea surface state grid U fields(ssu_m, utau_icb)
   INTEGER, PARAMETER ::   jps_ss_V   =  3   ! sea surface state grid V fields (ssv_m, vtau_icb)

   ! ocean 3D files (needed in icb_utl)
   INTEGER, PARAMETER ::   jps_Uu_oce  =  4   ! oce Uu velocity
   INTEGER, PARAMETER ::   jps_Vv_oce  =  5   ! oce Vv velocity
   INTEGER, PARAMETER ::   jps_Tt_oce  =  6   ! oce Ts

   INTEGER, PARAMETER ::   jpsnd_icb = 6 ! total number of sent fields
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


    INTEGER FUNCTION icb_cpl_alloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION icb_cpl_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      ! allocating the sending buffer only for the activated fields
      DO jn = 1, jpsnd_icb
         IF( icbsnd(jn)%laction ) ALLOCATE( icbsnd(jn)%z3(jpi,jpj,icbsnd(jn)%nlvl), STAT=ierr )
         icb_cpl_alloc = MAX(ierr,0)
      END DO
      

      ! allocating the reception buffer only for the activated fields
      DO jn = 1, jprcv_icb
         IF( icbrcv(jn)%laction ) ALLOCATE( icbrcv(jn)%z3(jpi,jpj,icbrcv(jn)%nlvl), STAT=ierr )
         icb_cpl_alloc = icb_cpl_alloc + MAX(ierr,0)
      END DO
      !
      ! allocating nrcvinfo, useful for icb_cpl_rcv
     ! ALLOCATE( nrcvinfo(jprcv),  STAT=ierr )
     ! icb_cpl_alloc = icb_cpl_alloc + MAX(ierr,0)

    END FUNCTION icb_cpl_alloc
    !
    SUBROUTINE icb_cpl_init()
    !!----------------------------------------------------------------------
    !!             ***  ROUTINE icb_cpl_init  ***
    !!
    !! ** Purpose :   Initialisation of send and received information from
    !!                the icb component
    !!
    !! ** Method  : * Read namicb_cpl namelist (to be written in the future ^^)
    !!              * define the receive interface
    !!              * define the send    interface
    !!              * initialise the OASIS coupler
    !!----------------------------------------------------------------------
      INTEGER  ::   ios     ! Local integer output status for namelist read
      NAMELIST/namberg/ ln_icebergs    , ln_bergdia     , rn_sample_rate_days , rn_initial_mass      ,   &
         &              rn_distribution, rn_mass_scaling, rn_initial_thickness, rn_verbose_write_days,   &
         &              rn_rho_bergs   , rn_LoW_ratio   , nn_verbose_level    , ln_operator_splitting,   &
         &              rn_bits_erosion_fraction        , rn_sicn_shift       , ln_passive_mode      ,   &
         &              ln_time_average_weight          , nn_test_icebergs    , rn_test_box          ,   &
         &              ln_use_calving , rn_speed_limit , cn_dir, sn_icb      , ln_M2016             ,   &
         &              cn_icbrst_indir, cn_icbrst_in   , cn_icbrst_outdir    , cn_icbrst_out        ,   &
         &              ln_icb_grd     , ln_use_test    , ln_icb_bas          , ln_rst_test_bas      ,   &
         &              cn_icbbasins_file, cn_icbbasins_var2d  , cn_icbbasins_var1d   , nn_icb_basins,   &
         &              ln_berg_cpl, ln_cpl_asynchrone, ln_cpl_nlvlcut, nn_lvlcut_cpl 
      !!----------------------------------------------------------------------
    !!----------------------------------------------------------------------
    ! value of ln_M2016 needed here, since icb_cpl_init is called before icb_ini in nemo_init (see nemogcm.F90)
  
    READ_NML_REF(numnam,namberg)
    READ_NML_CFG(numnam,namberg)
 
    IF ( ln_icebergs .AND. ln_berg_cpl ) THEN  !! IF NO ICEBERG COUPLING, do not initialize SAB coupling
       !
       IF (lwp) WRITE(numout,*) "icb_cpl_init : def of rcv + snd structures for NEMO-SAB coupling "
        !
        CALL icb_set_nlvlcpl()         ! setting value of nlvlsab_cpl
        !
        ! initialising buffers to import iceberg fluxes to NEMO's emp and qns 
        icb_wflx(:,:) = 0._wp
        icb_hcflx(:,:) = 0._wp

      ! ================================ !
      !   Define the receive interface   !
      ! ================================ !

   ! for each field

   ! LOGICAL               ::   laction   ! To be coupled or not
   ! CHARACTER(len = 8)    ::   clname    ! Field alias used by OASIS
   ! CHARACTER(len = 1)    ::   clgrid    ! Grid type
   ! REAL(wp)              ::   nsgn      ! Control of the sign change
   ! INTEGER               ::   nlvl      ! Number of grid level to exchange, set 1 for 2D fields

      ! -------------------------------- 
      ! DEFINING sending interface
      ! default definitions of ssnd
      
      ALLOCATE( icbsnd(jpsnd_icb) )
      icbsnd(:)%laction = .FALSE.   ;   icbsnd(:)%clgrid = 'T'   ;   icbsnd(:)%nsgn = 1.
      icbsnd(:)%nct = 1   ;   icbsnd(:)%nlvl = 1 ; ; icbsnd(:)%ncplmodel = 1

#if defined key_si3 
      ! 1) Sea- surface fields + sea-ice fields inside the same bundles 
      
      ! a) with sea-ice T-grid Bundle
      icbsnd(jps_ss_T)%clname = 'O_ss_T'
      icbsnd(jps_ss_T)%laction = .TRUE.
      icbsnd(jps_ss_T)%nlvl = 6    !  ssh,sst,sss,fr_i,at_i,vt_i 

      ! b) with sea-ice U-grid Bundle
      icbsnd(jps_ss_U)%clname = 'O_ss_U'
      icbsnd(jps_ss_U)%laction = .TRUE.
      icbsnd(jps_ss_U)%clgrid = 'U'
      icbsnd(jps_ss_U)%nsgn   = -1 !change of sign at north fold !
      icbsnd(jps_ss_U)%nlvl   = 3 !ssu_m, utau_icb, u_Ice

      ! c) with sea-ice V-grid Bundle
      icbsnd(jps_ss_V)%clname = 'O_ss_V'
      icbsnd(jps_ss_V)%laction = .TRUE.
      icbsnd(jps_ss_V)%clgrid = 'V'
      icbsnd(jps_ss_V)%nsgn   = -1 !change of sign at north fold !
      icbsnd(jps_ss_V)%nlvl   = 3  !ssv_m, vtau_icb, v_Ice

#else      
      ! 1 bis) only sea surf fields

      ! a) no sea-ice T-grid Bundle
      icbsnd(jps_ss_T)%clname = 'O_ss_T' 
      icbsnd(jps_ss_T)%laction = .TRUE.
      icbsnd(jps_ss_T)%nlvl = 4    ! ssh,sst,sss,fr_i 
       
      ! b) no sea-ice U-grid Bundle
      icbsnd(jps_ss_U)%clname = 'O_ss_U'
      icbsnd(jps_ss_U)%laction = .TRUE.
      icbsnd(jps_ss_U)%clgrid = 'U'
      icbsnd(jps_ss_U)%nsgn   = -1 !change of sign at north fold !
      icbsnd(jps_ss_U)%nlvl   = 2 !ssu_m, utau_icb

      ! c) no sea-ice V-grid Bundle
      icbsnd(jps_ss_V)%clname = 'O_ss_V'
      icbsnd(jps_ss_V)%laction = .TRUE.
      icbsnd(jps_ss_V)%clgrid = 'V'
      icbsnd(jps_ss_V)%nsgn   = -1 !change of sign at north fold !
      icbsnd(jps_ss_V)%nlvl   = 2  !ssv_m, vtau_icb

#endif

      !2) 3D ocean fields for Merino 2016's option (+ grounding) 
      IF( ln_M2016 ) THEN
      IF (lwp) THEN
          WRITE(numout,*) " icb_cpl_init : ln_M2016 = ",  ln_M2016
          WRITE(numout,*) " W A R N I N G : O_Uu_3D, O_VV_3D and O_Tt_3D must be defined in namcouple, otherwise oasis will crash"
      ENDIF
      !
              ! 3D field uu
              icbsnd(jps_Uu_oce)%clname = 'O_Uu_3D'
              icbsnd(jps_Uu_oce)%laction = .TRUE.
              icbsnd(jps_Uu_oce)%clgrid = 'U'
              icbsnd(jps_Uu_oce)%nsgn   = -1 !change of sign at north fold !
              icbsnd(jps_Uu_oce)%nlvl   = nlvlsab_cpl 
             
              ! 3D field vv
              icbsnd(jps_Vv_oce)%clname = 'O_Vv_3D'
              icbsnd(jps_Vv_oce)%laction = .TRUE.
              icbsnd(jps_Vv_oce)%clgrid = 'V'
              icbsnd(jps_Vv_oce)%nsgn   = -1 !change of sign at north fold !
              icbsnd(jps_Vv_oce)%nlvl   = nlvlsab_cpl 
 
              ! 3D field ts( only temp)
              icbsnd(jps_Tt_oce)%clname = 'O_Tt_3D'
              icbsnd(jps_Tt_oce)%laction = .TRUE.
              icbsnd(jps_Tt_oce)%clgrid = 'T'
              icbsnd(jps_Tt_oce)%nlvl   = nlvlsab_cpl 
              !
              ! 2D field r3t (e3t ~ r3t * e3t_0, so only r3t is sent, see domzgr_substitute.h90)    
               icbsnd(jps_ss_T)%nlvl = 7    ! T-grid bundle: ssh,sst,sss,fr_i,at_i,vt_i AND r3t

               ! WARNING : there might be a problem if there is no sea-ice (cf icb_cpl_snd routine) !! 
      ELSE
              icbsnd(jps_Uu_oce)%clname = 'O_Uu_3D'
              icbsnd(jps_Vv_oce)%clname = 'O_Vv_3D'
              icbsnd(jps_Tt_oce)%clname = 'O_Tt_3D'

      ENDIF

      ! --------------------------------
      ! DEFINING receiving interface
      ! default definitions of srcv

      ALLOCATE( icbrcv(jprcv_icb) )
      icbrcv(:)%laction = .FALSE.   ;   icbrcv(:)%clgrid = 'T'   ;   icbrcv(:)%nsgn = 1. 
      icbrcv(:)%nct = 1   ;   icbrcv(:)%nlvl = 1   ; icbrcv(:)%ncplmodel = 1

      !a) berg fresh water flux, heat flux 
      icbrcv(jpr_berg_fx)%clname = 'Oberg_fx'
      IF (.NOT. ln_passive_mode) THEN
          icbrcv(jpr_berg_fx)%laction = .TRUE.
          icbrcv(jpr_berg_fx)%nlvl = 2 
      ENDIF
      ! =================================== !
      !   define variables for the coupler  !
      ! =================================== !
      CALL cpl_vardef(midicb)  !! " 1 " stands for number of models to couple with 
 
      ! CHECKING and allocating the 'z3' buffers to send and receive data
      IF( icb_cpl_alloc() /= 0 )  CALL ctl_stop( 'STOP', 'icb_cpl_alloc : unable to allocate arrays' )
      
      WRITE (numout,*) " icb_cpl_alloc : normal end of initialization, all arrays allocated correctly ! "
      !
     ENDIF ! ENDIF ln_icebergs .AND. ln_berg_cpl
      
    END SUBROUTINE icb_cpl_init
     

    SUBROUTINE icb_cpl_rcv( kt )

      !!----------------------------------------------------------------------
      !!             ***  ROUTINE icb_cpl_rcv  ***
      !!
      !! ** Purpose : receive all the activated coupled fields from NEMO
      !! ** Method  : use cpl_oasis interface (by A. Barge)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      !
      INTEGER :: isec, info, jn,ji,jj                     ! local integer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('icb_cpl_rcv')
      !
      ! Optimisation : going through all of icb_cpl_rcv only if sbc has been called
      ! WARNING : it might be too restrictive for high resolution configurations
      !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !
      ! ==========================
      !   Proceed all activated receptions
      ! ==========================
      !
        DO jn = 1, jprcv_icb
         IF( icbrcv(jn)%laction ) THEN
            CALL cpl_rcv( midicb, jn, isec, icbrcv(jn)%z3(A2D(0),1:icbrcv(jn)%nlvl), info)
         ENDIF
        END DO
     
        ! storing icbrcv(jpr_berg_fx)%z3 fluxes into buffer to add them later to emp and qns (cf icbstp.F90)
        ! this works, regardless of the value of ln_cpl_asynchrone (cf icbstp.F90) 
        IF (.NOT. ln_passive_mode) THEN
           icb_wflx(A2D(0)) =  icbrcv(jpr_berg_fx)%z3(A2D(0),1)
           icb_hcflx(A2D(0)) =  icbrcv(jpr_berg_fx)%z3(A2D(0),2)
        ENDIF
      !
      ENDIF ! ENDIF( MOD( kt-1, nn_fsbc ) == 0 )

      IF( ln_timing )   CALL timing_stop('icb_cpl_rcv')

   END SUBROUTINE icb_cpl_rcv
  !
  !
   SUBROUTINE icb_cpl_snd( kt )

      !!----------------------------------------------------------------------
      !!             ***  ROUTINE icb_cpl_snd  ***
      !!
      !! ** Purpose : send fresh water flux and heat flux of icebergs from SAB to NEMO
      !! ** Method  : use cpl_oasis interface (by A. Barge)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      !
      INTEGER :: isec, info, jn                      ! local integer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('icb_cpl_snd')
      !
      ! Optimisation : going through all of icb_cpl_rcv only if sbc has been called
      ! it might be too restrictive for high resolution configurations
      !
      IF( MOD( kt-1, nn_fsbc ) == 0 ) THEN
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !

      ! =============
      ! fill sendings buffer with :  

       !1) sea surface 2D fields :  
       icbsnd(jps_ss_T)%z3(A2D(0),1) = ssh_m(A2D(0))
       icbsnd(jps_ss_T)%z3(A2D(0),2) = sst_m(A2D(0))
       icbsnd(jps_ss_T)%z3(A2D(0),3) = sss_m(A2D(0))
       icbsnd(jps_ss_T)%z3(A2D(0),4) = fr_i(A2D(0))

       icbsnd(jps_ss_U)%z3(A2D(0),1) = ssu_m(A2D(0))
       icbsnd(jps_ss_U)%z3(A2D(0),2) = utau_icb(A2D(0))

       icbsnd(jps_ss_V)%z3(A2D(0),1) = ssv_m(A2D(0))
       icbsnd(jps_ss_V)%z3(A2D(0),2) = vtau_icb(A2D(0))

       !2) sea-ice related fields
#if defined key_si3
       icbsnd(jps_ss_U)%z3(A2D(0),3) = u_ice(A2D(0))
       icbsnd(jps_ss_V)%z3(A2D(0),3) = v_ice(A2D(0))
       icbsnd(jps_ss_T)%z3(A2D(0),5) =  at_i(A2D(0))
       icbsnd(jps_ss_T)%z3(A2D(0),6) =  vt_i(A2D(0))
#endif

       !3) IF ln_M2016 : send uu, vv and ts(only temp) and r3t
       
       IF( ln_M2016 ) THEN
       !
       ! important to fill uu (resp vv + ts) only from z = 1 to z = uu%nlvl (critical if ln_cut_z700M
            icbsnd(jps_Uu_oce)%z3(A2D(0),1:icbsnd(jps_Uu_oce)%nlvl) = uu(A2D(0),1:icbsnd(jps_Uu_oce)%nlvl, Nbb)
            icbsnd(jps_Vv_oce)%z3(A2D(0),1:icbsnd(jps_Vv_oce)%nlvl) = vv(A2D(0),1:icbsnd(jps_Vv_oce)%nlvl, Nbb)
            icbsnd(jps_Tt_oce)%z3(A2D(0),1:icbsnd(jps_Tt_oce)%nlvl) = ts(A2D(0),1:icbsnd(jps_Tt_oce)%nlvl, jp_tem,Nbb)
            ! adding r3t to T-grid surface state bundle
            icbsnd(jps_ss_T)%z3(A2D(0),7) = r3t(A2D(0),Nbb)
            ! WARNING : add a case if there is no key_si3 : r3t must be stored on 5th level of grid-T bundle and not the 7th one
       ENDIF 

      ! ==========================
      !   Proceed sendings
      ! ==========================
      !
      DO jn = 1, jpsnd_icb
         IF ( icbsnd(jn)%laction ) THEN
            CALL cpl_snd( midicb, jn, isec, icbsnd(jn)%z3(A2D(0),1:icbsnd(jn)%nlvl), info)
         ENDIF
      END DO
    
    ENDIF ! ENDIF( MOD( kt-1, nn_fsbc ) == 0 )
    !
    IF( ln_timing )   CALL timing_stop('icb_cpl_snd')

    END SUBROUTINE icb_cpl_snd

#endif 
!endif ! key_sab
END MODULE icbcpl
