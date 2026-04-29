MODULE sabgcm
   !!======================================================================
   !!                       ***  MODULE sabgcm   ***
   !! StandAlone iceBergs module : deals only with iceberg float. it can run offline
   !!======================================================================
   !! History :  3.6  ! 2011-11  (S. Alderson, G. Madec) original code
   !!             -   ! 2013-06  (I. Epicoco, S. Mocavero, CMCC) nemo_northcomms: setup avoiding MPI communication
   !!             -   ! 2014-12  (G. Madec) remove KPP scheme and cross-land advection (cla)
   !!            4.0  ! 2016-10  (G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sab_gcm      : solve ocean dynamics, tracer, biogeochemistry and/or sea-ice
   !!   sab_init     : initialization of the NEMO system
   !!   sab_ctl      : initialisation of the contol print
   !!   sab_closefile: close remaining open files
   !!   sab_alloc    : dynamical allocation
   !!----------------------------------------------------------------------
   USE phycst         ! physical constant                  (par_cst routine)
   USE domain         ! domain initialization   (dom_init & dom_cfg routines)
   USE daymod         ! calendar
   USE step_sab           ! NEMO time-stepping                 (stp     routine)
   USE cpl_oasis3     !
   USE sabcpl         ! for sab_cpl_init : initialize coupling with  
   USE eosbn2, ONLY : eos_init ! equation of state of sea-water
# if defined key_si3
   USE ice
   USE sbc_ice
# endif 
   USE sbc_oce, ONLY: Nbb, Nnn, Naa, Nrhs
   USE icbini
   USE icbstp, ONLY : icb_end
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE in_out_manager ! I/O manager
   USE iom            !
   USE lib_mpp        ! distributed memory computing
   USE mppini         ! shared/distributed memory setting (mpp_init routine)
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
  
   USE lbclnk
   USE timing          ! Timing
   USE xios            ! I/O server

   USE halo_mng

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sab_gcm    ! called by sab.F90

   CHARACTER(lc) ::   cform_aaa="( /, 'AAAAAAAA', / ) "     ! flag for output listing


#if ! defined key_mpi_off
   ! need MPI_Wtime
   INCLUDE 'mpif.h'
#endif


      !! * Substitutions
#  include "read_nml_substitute.h90"


   !!----------------------------------------------------------------------
   !! NEMO/SAS 5.0 , NEMO Consortium (2022)
   !! $Id: sabgcm (CHANGE NAME, sab is not a gcm) $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sab_gcm
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_gcm  ***
      !!
      !! ** Purpose :   NEMO solves the primitive equations on an orthogonal
      !!              curvilinear mesh on the sphere.
      !!
      !! ** Method  : - model general initialization
      !!              - launch the time-stepping (stp routine)
      !!              - finalize the run by closing files and communications
      !!
      !! References : Madec, Delecluse, Imbard, and Levy, 1997:  internal report, IPSL.
      !!              Madec, 2008, internal report, IPSL.
      !!----------------------------------------------------------------------
      INTEGER ::   istp   ! time step index
      REAL(wp)::   zstptiming   ! elapsed time for 1 time step
      !!----------------------------------------------------------------------
      !
      CALL timing_start( 'full code' )     ! do it as soon as possible, no need to test ln_timing (that is not yet defined)
      CALL timing_start( 'before step_sab' )
      !

      !                            !-----------------------!
      CALL sab_init               !==  Initialisations  ==!
      !                            !-----------------------!
      !
      ! check that all process are still there... If some process have an error,
      ! they will never enter in step and other processes will wait until the end of the cpu time!
      CALL mpp_max( 'sabgcm', nstop )

      IF(lwp) WRITE(numout,cform_aaa)   ! Flag AAAAAAA
      CALL timing_stop( 'before step_sab' )
      !                            !-----------------------!
      !                            !==   time stepping   ==!
      !                            !-----------------------!
      !
      !                                               !== set the model time-step  ==!
      !
      istp = nit000
      !
      !
      DO WHILE( istp <= nitend .AND. nstop == 0 )

         CALL timing_start( 'step_sab', istp, nit000, nitend, 1000 )
         CALL stp( istp )
         CALL timing_stop( 'step_sab', istp )
         istp = istp + 1

      END DO
      !
      IF( ln_icebergs )   CALL icb_end( nitend )
      !
      !                            !------------------------!
      !                            !==  finalize the run  ==!
      !                            !------------------------!
      IF(lwp) WRITE(numout,cform_aaa)        ! Flag AAAAAAA
      !
      IF( nstop /= 0 .AND. lwp ) THEN        ! error print
         WRITE(ctmp1,*) '   ==>>>   sab_gcm: a total of ', nstop, ' errors have been found'
         IF( ngrdstop > 0 ) THEN
            WRITE(ctmp9,'(i2)') ngrdstop
            WRITE(ctmp2,*) '           E R R O R detected in grid '//TRIM(ctmp9)
            WRITE(ctmp3,*) '           Look for "E R R O R" messages in all existing '//TRIM(ctmp9)//'_ocean_output* files'
            CALL ctl_stop( ' ', ctmp1, ' ', ctmp2, ' ', ctmp3 )
         ELSE
            WRITE(ctmp2,*) '           Look for "E R R O R" messages in all existing ocean_output* files'
            CALL ctl_stop( ' ', ctmp1, ' ', ctmp2 )
         ENDIF
      ENDIF
      !
      CALL timing_stop( 'full code', ld_finalize = .TRUE. )
      !
      CALL sab_closefile
      !
      CALL xios_finalize  ! end mpp communications with xios
      IF( lk_oasis ) CALL cpl_finalize   ! end coupling and mpp communications with OASIS

      !
      IF(lwm) THEN
         IF( nstop == 0 ) THEN   ;   STOP 0
         ELSE                    ;   STOP 123
         ENDIF
      ENDIF
      !
   END SUBROUTINE sab_gcm


   SUBROUTINE sab_init
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_init  ***
      !!
      !! ** Purpose :   initialization of the SAB GCM
      !!----------------------------------------------------------------------
      INTEGER ::   ios, ilocal_comm   ! local integers
      !!
      NAMELIST/namctl/ sn_cfctl, ln_timing, ln_diacfl,                                &
         &             nn_isplt,  nn_jsplt,  nn_ictls, nn_ictle, nn_jctls, nn_jctle
      NAMELIST/namcfg/ ln_read_cfg, cn_domcfg, ln_closea, ln_write_cfg, cn_domcfg_out 
      !
      !
      cxios_context = 'sab'
      !
      !
      !
      !
      !                             !-------------------------------------------------!
      !                             !     set communicator & select the local rank    !
      !                             !  must be done as soon as possible to get narea  !
      !                             !-------------------------------------------------!
      !
      !

#if defined key_xios
      IF( Agrif_Root() ) THEN
         IF( lk_oasis ) THEN
            CALL cpl_init( "sab", ilocal_comm )                               ! nemo local communicator given by oasis
# if defined key_xios3
            CALL xios_initialize( "sab"         , local_comm =ilocal_comm )   ! send nemo communicator to xios
# else   
            CALL xios_initialize( "not used"       , local_comm =ilocal_comm )   ! send nemo communicator to xios

# endif
         ELSE
            CALL xios_initialize( "for_xios_mpi_id", return_comm=ilocal_comm )   ! nemo local communicator given by xios
         ENDIF
      ENDIF
      CALL mpp_start( ilocal_comm )
#else 
      IF( lk_oasis ) THEN
         IF( Agrif_Root() ) THEN
            CALL cpl_init( "sab", ilocal_comm )          ! nemo local communicator given by oasis
         ENDIF
         CALL mpp_start( ilocal_comm )
      ELSE
         CALL mpp_start( )
      ENDIF
#endif
      !
      narea = mpprank + 1                                   ! mpprank: the rank of proc (0 --> mppsize -1 )
      lwm = (narea == 1)                ! control of output namelists
      !
      !                             !---------------------------------------------------------------!
      !                             ! Open output files, reference and configuration namelist files !
      !                             !---------------------------------------------------------------!
      !
      ! open sab.output as soon as possible to get all output prints (including errors messages)
      
      IF( lwm )   CALL ctl_opn(      numout,            'sab.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open reference and configuration namelist files
      CALL load_nml( numnam_ref,        'namelist_sab_ref',                                           -1, lwm )
      CALL load_nml( numnam_cfg,        'namelist_sab_cfg',                                           -1, lwm )
      IF( lwm )   CALL ctl_opn(      numond, 'output.namelist_dom', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      ! open /dev/null file to be able to supress output write easily
                     CALL ctl_opn(     numnul,               '/dev/null', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, -1, .FALSE. )
      !
      !                             !--------------------!
      !                             ! Open listing units !  -> need sn_cfctl from namctl to define lwp
      !                             !--------------------!
      !
      READ_NML_REF(numnam,namctl)
      READ_NML_CFG(numnam,namctl)

      !
      ! finalize the definition of namctl variables
      IF( narea < sn_cfctl%procmin .OR. narea > sn_cfctl%procmax .OR. MOD( narea - sn_cfctl%procmin, sn_cfctl%procincr ) /= 0 )   &
         &   CALL sab_set_cfctl( sn_cfctl, .FALSE. )
      !
      lwp = (narea == 1) .OR. sn_cfctl%l_oceout    ! control of all listing output print
      !
      IF(lwm) THEN                      ! open listing units
         !
         WRITE(numout,*)
         WRITE(numout,*) '                        SAB '
         WRITE(numout,*) '               Iceberg Lagrangian model '
         WRITE(numout,*) '            A standalone version of NEMO/ICB'
         WRITE(numout,*) '                version 0.1  (2024) '
         !
         WRITE(numout,cform_aaa)                                        ! Flag AAAAAAA
         !
      ENDIF
      !
      IF(lwm) WRITE( numond, namctl )
      !
      !                             !------------------------------------!
      !                             !  Set global domain size parameters !
      !                             !------------------------------------!
      !
      
      READ_NML_REF(numnam,namcfg)
      READ_NML_CFG(numnam,namcfg)

      CALL domain_cfg ( cn_cfg, nn_cfg, Ni0glo, Nj0glo, jpkglo, l_Iperio, l_Jperio, l_NFold, c_NFtype )
      !
      IF(lwm)   WRITE( numond, namcfg )
      !
      !                             !-----------------------------------------!
      !                             ! mpp parameters and domain decomposition !
      !                             !-----------------------------------------!
      CALL mpp_init

      CALL halo_mng_init()
      ! Now we know the dimensions of the grid and numout has been set: we can allocate arrays
            
      CALL sab_alloc()
      
      IF( lk_oasis     )   CALL cpl_domdef   ! Define grid for coupling

      ! Initialise time level indices
      Nbb = 1; Nnn = 2; Naa = 3; Nrhs = Naa
      !                             !-------------------------------!
      !                             !  NEMO general initialization  !
      !                             !-------------------------------!

      CALL sab_ctl                          ! Control prints
      !
      !                                      ! General initialization
                           CALL timing_open( lwp, mpi_comm_oce, "timing_sab.output" )   ! open timing report file

      IF( ln_timing    )   CALL timing_start( 'sab_init')

                           CALL phy_cst         ! Physical constants

                           CALL eos_init        ! Equation of seawater
                           CALL dom_init( Nbb, Nnn, Naa ) ! Domain

                           CALL icb_init( rn_Dt, nit000, Nnn)   ! initialise icebergs instance

      IF( lk_oasis     )   CALL sab_cpl_init()                  ! initialise icebergs-NEMO coupling

      IF( lk_oasis     )   CALL cpl_enddef                       ! terminate coupling initialization


      IF( sn_cfctl%l_prtctl )   &
         &                 CALL prt_ctl_init        ! Print control
           

                           CALL day_init        ! model calendar (using both namelist and restart infos)
      
      !
      IF(lwp) WRITE(numout,cform_aaa)           ! Flag AAAAAAA
      !
      IF( ln_timing    )   CALL timing_stop( 'sab_init')
      !
   END SUBROUTINE sab_init


   SUBROUTINE sab_ctl
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_ctl  ***
      !!
      !! ** Purpose :   control print setting
      !!
      !! ** Method  : - print namctl and namcfg information and check some consistencies
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'sab_ctl: Control prints'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist namctl'
         WRITE(numout,*) '                              sn_cfctl%l_runstat = ', sn_cfctl%l_runstat
         WRITE(numout,*) '                              sn_cfctl%l_trcstat = ', sn_cfctl%l_trcstat
         WRITE(numout,*) '                              sn_cfctl%l_oceout  = ', sn_cfctl%l_oceout
         WRITE(numout,*) '                              sn_cfctl%l_layout  = ', sn_cfctl%l_layout
         WRITE(numout,*) '                              sn_cfctl%l_prtctl  = ', sn_cfctl%l_prtctl
         WRITE(numout,*) '                              sn_cfctl%l_prttrc  = ', sn_cfctl%l_prttrc
         WRITE(numout,*) '                              sn_cfctl%l_oasout  = ', sn_cfctl%l_oasout
         WRITE(numout,*) '                              sn_cfctl%procmin   = ', sn_cfctl%procmin
         WRITE(numout,*) '                              sn_cfctl%procmax   = ', sn_cfctl%procmax
         WRITE(numout,*) '                              sn_cfctl%procincr  = ', sn_cfctl%procincr
         WRITE(numout,*) '                              sn_cfctl%ptimincr  = ', sn_cfctl%ptimincr
         WRITE(numout,*) '      timing by routine               ln_timing  = ', ln_timing
         WRITE(numout,*) '      CFL diagnostics                 ln_diacfl  = ', ln_diacfl
      ENDIF
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist namcfg'
         WRITE(numout,*) '      read domain configuration file                ln_read_cfg      = ', ln_read_cfg
         WRITE(numout,*) '         filename to be read                           cn_domcfg     = ', TRIM(cn_domcfg)
         WRITE(numout,*) '      create a configuration definition file        ln_write_cfg     = ', ln_write_cfg
         WRITE(numout,*) '         filename to be written                        cn_domcfg_out = ', TRIM(cn_domcfg_out)
      ENDIF
      !
      IF( 1._wp /= SIGN(1._wp,-0._wp)  )   CALL ctl_stop( 'sab_ctl: The intrinsec SIGN function follows f2003 standard.',  &
         &                                                'Compile with key_nosignedzero enabled:',   &
         &                                                '--> add -Dkey_nosignedzero to the definition of %CPP in your arch file' )
      !
      !
   END SUBROUTINE sab_ctl


   SUBROUTINE sab_closefile
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_closefile  ***
      !!
      !! ** Purpose :   Close the files
      !!----------------------------------------------------------------------
      !
      IF( lk_mpp )   CALL mppsync
      !
      CALL iom_close                                 ! close all input/output files managed by iom_*
      !
      IF( numstp          /= -1 )   CLOSE( numstp          )   ! time-step file
      IF( numrun          /= -1 )   CLOSE( numrun          )   ! run statistics file
      IF( lwm.AND.numond  /= -1 )   CLOSE( numond          )   ! oce output namelist
      IF( lwm.AND.numoni  /= -1 )   CLOSE( numoni          )   ! ice output namelist
      IF( numevo_ice      /= -1 )   CLOSE( numevo_ice      )   ! ice variables (temp. evolution)
      IF( numout          /=  6 )   CLOSE( numout          )   ! standard model output file
      !
      numout = 6                                     ! redefine numout in case it is used after this point...
      !
   END SUBROUTINE sab_closefile


   SUBROUTINE sab_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the SAB modules
      !!
      !! ** Method  :
      !!----------------------------------------------------------------------
      USE dom_oce   , ONLY : dom_oce_alloc
      USE bdy_oce   , ONLY : ln_bdy, bdy_oce_alloc
      USE oce       ! mandatory to allocate 3D array if needed
      USE sbc_oce
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      ierr = 0
      !
      ierr = ierr + dom_oce_alloc()          ! ocean domain
      ierr = ierr + oce_alloc    ()          ! (uu, vv and ts) needed for ICB (if ln_M2016)
      ierr = ierr + sbc_oce_alloc()
      ierr = ierr + bdy_oce_alloc()          ! bdy masks (incl. initialization)

# if defined key_si3       
      ierr = ierr + ice_alloc() + sbc_ice_alloc() 
# endif
      !
      CALL mpp_sum( 'sab_alloc', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sab_alloc: unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE sab_alloc

   SUBROUTINE sab_set_cfctl(sn_cfctl, setto )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE sab_set_cfctl  ***
      !!
      !! ** Purpose :   Set elements of the output control structure to setto.
      !!
      !! ** Method  :   Note this routine can be used to switch on/off some
      !!                types of output for selected areas.
      !!----------------------------------------------------------------------
      TYPE(sn_ctl), INTENT(inout) :: sn_cfctl
      LOGICAL     , INTENT(in   ) :: setto
      !!----------------------------------------------------------------------
      sn_cfctl%l_runstat = setto
      sn_cfctl%l_trcstat = setto
      sn_cfctl%l_oceout  = setto
      sn_cfctl%l_layout  = setto
      sn_cfctl%l_prtctl  = setto
      sn_cfctl%l_prttrc  = setto
      sn_cfctl%l_oasout  = setto
   END SUBROUTINE sab_set_cfctl

   !!======================================================================
END MODULE sabgcm
