MODULE step_sab
   !!======================================================================
   !!                       ***  MODULE step_sab  ***
   !! Time-stepping    : manager of the ocean, tracer and ice time stepping
   !!                    version for standalone surface scheme
   !!======================================================================
   !! History :  OPA  !  1991-03  (G. Madec)  Original code
   !!             .   !    .
   !!             .   !    .
   !!   NEMO     3.5  !  2012-03  (S. Alderson)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp             : OCE system time-stepping
   !!----------------------------------------------------------------------
   USE dom_oce          ! ocean space and time domain variables
   USE daymod           ! calendar                         (day     routine)
   !
   USE sabcpl           ! receive and send fields to/from NEMO
   USE sbc_oce, ONLY: Nbb
   !
   USE in_out_manager   ! I/O manager
   USE prtctl           ! Print control                    (prt_ctl routine)
   USE iom              !
   USE lbclnk           !
   USE timing           ! Timing
   !
   USE xios
   USE icbstp

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp   ! called by sabgcm.F90
   !!----------------------------------------------------------------------
   !! NEMO 5.0 , NEMO Consortium (2022)
   !! $Id: step.F90 14239 2020-12-23 08:57:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp  ***
      !!
      !! ** Purpose : - Time stepping of SBC (surface boundary)
      !!
      !! ** Method  : -1- Update forcings and data
      !!              -2- Outputs and diagnostics
      !!----------------------------------------------------------------------
      !
      IF( kstp == nit000 )   CALL iom_init( cxios_context ) ! iom_put initialization (must be done after sab_init for AGRIF+XIOS+OASIS)
      CALL iom_setkt( kstp - nit000 + 1, cxios_context )   ! tell iom we are at time step kstp

      IF( kstp /= nit000 )   CALL day( kstp )             ! Calendar (day was already called at nit000 in day_init) 
                                                          ! "day" calls sab_rst : checks if rstart must be done at current time -step
      Nbb = 1      ! forcing Nbb to 1 to match the structure of uu, vv, ts ::  (:,:,:,Nbb)  
      !

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Coupled mode
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<     

      IF ( lk_oasis ) CALL sab_cpl_rcv(kstp) ! receives sea-surface state + sea-ice infos from NEMO 
      !   
      CALL icb_stp(kstp,Nbb)                 ! runs icebergs time-step (cf ICB) 
       
      IF ( lk_oasis ) CALL sab_cpl_snd(kstp) ! sends fresh water + heat flux at the surface to NEMO 
       
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Control
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      ! CALL stp_ctl( kstp, Nnn ) ! TO BE ADAPTED FOR SAB in future ISSUE (cf standalone version) 
      
      ! updating nitrst if a rstart has just been written (by icb_rst_wri, cf icbrst.F90)
      IF ( lrst_oce ) THEN
         lrst_oce = .FALSE.             !otherwise a rstart will be written at each following time-step 
         IF( ln_rst_list ) THEN
            nrst_lst = MIN(nrst_lst + 1, SIZE(nn_stocklist,1))
            nitrst   = nn_stocklist( nrst_lst )
            IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
         ENDIF
      ENDIF
      !
      IF( kstp == nitend .OR. nstop > 0 ) THEN
         CALL iom_context_finalize( cxios_context ) ! needed for XIOS+AGRIF
      ENDIF
      !
   END SUBROUTINE stp
   !!======================================================================
END MODULE step_sab
