MODULE restart_sab
   !!======================================================================
   !!                       ***  MODULE restart_sab  ***
   !!                       only prepares potential restart(s) for SAB
   !!                       before icb write them with icb_rst_wri
   !!----------------------------------------------------------------------
   USE dom_oce          ! ocean space and time domain variables
   !
   USE in_out_manager   ! I/O manager
   USE prtctl           ! Print control                    (prt_ctl routine)
   USE iom              !
   !
   IMPLICIT NONE
   PRIVATE

   PUBLIC   sab_rst ! called by daymod.F90
   !!----------------------------------------------------------------------
   !! NEMO 5.0 , NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sab_rst( kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE sab_rst  ***
      !!
      !! ** Purpose : + initialization (should be read in the namelist) of nitrst
      !!              + open the restart when we are one time step before nitrst
      !!                   - restart header is defined when kt = nitrst-1
      !!                   - restart data  are written when kt = nitrst
      !!              + define lrst_oce to .TRUE. when we need to define or write the restart
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! ocean time-step
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN   ! default definitions
         lrst_oce = .FALSE.
         IF( ln_rst_list ) THEN
            nrst_lst = 1
            nitrst = nn_stocklist( nrst_lst )
         ELSE
            nitrst = nitend
         ENDIF
      ENDIF
      !
      IF (kt == nitrst ) lrst_oce = .true.  ! activating restart flag for icb 

      ! frequency-based restart dumping (nn_stock)
      IF( .NOT. ln_rst_list ) THEN
         IF  ( nn_stock  == 0 ) THEN
            nitrst = nitend
         ELSEIF  (MOD( kt - 1, nn_stock ) == 0 ) THEN
            ! we use kt - 1 and not kt - nit000 to keep the same periodicity from the beginning of the experiment
            nitrst = kt + nn_stock - 1                  ! define the next value of nitrst for restart writing
            IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
         ENDIF
      ENDIF
      !
   END SUBROUTINE sab_rst
!================================================
END MODULE restart_sab
