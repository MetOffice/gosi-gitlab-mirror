MODULE ice
   !!======================================================================
   !!                        ***  MODULE  ice  ***
   !!   sea-ice:  ice variables defined in memory
   !!======================================================================
   !! History :  3.0  !  2008-03  (M. Vancoppenolle) Original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_alloc   ! called by icestp.F90

   !!======================================================================
   !!                                                                     |
   !!              I C E   S T A T E   V A R I A B L E S                  |
   !!                                                                     |
   !! Introduction :                                                      |
   !! --------------                                                      |
   !! Every ice-covered grid cell is characterized by a series of state   |
   !! variables. To account for unresolved spatial variability in ice     |
   !! thickness, the ice cover in divided in ice thickness categories.    |
   !!                                                                     |
   !! Sea ice state variables depend on the ice thickness category        |
   !!                                                                     |
   !! Those variables are divided into two groups                         |
   !! * Extensive (or global) variables.                                  |
   !!   These are the variables that are transported by all means         |
   !! * Intensive (or equivalent) variables.                              |
   !!   These are the variables that are either physically more           |
   !!   meaningful and/or used in ice thermodynamics                      |
   !!                                                                     |
   !! List of ice state variables :                                       |
   !! -----------------------------                                       |
   !!                                                                     |
   !!-------------|-------------|---------------------------------|-------|
   !!   name in   |   name in   |              meaning            | units |
   !! 2D routines | 1D routines |                                 |       |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Dynamical variables (prognostic)                    *** |
   !! ******************************************************************* |
   !!                                                                     |
   !! u_ice       |      -      |    ice velocity in i-direction  | m/s   |
   !! v_ice       |      -      |    ice velocity in j-direction  | m/s   |
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Category-summed state variables (diagnostic)        *** |
   !! ******************************************************************* |
   !! at_i        | at_i_1d     |    Total ice concentration      |       |
   !! vt_i        |      -      |    Total ice vol. per unit area | m     |
   !!----------------------------------------------------------------------
   !! * Share Module variables
   !!----------------------------------------------------------------------
   !                                     !!** define arrays
   !! Variables summed over all categories, or associated to all the ice in a single grid cell
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   u_ice, v_ice  !: components of the ice velocity                          (m/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   vt_i   !: ice and snow total volume per unit area                 (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   at_i          !: ice total fractional area (ice concentration)
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION ice_alloc()
      !!-----------------------------------------------------------------
      !!               *** Routine ice_alloc ***
      !!-----------------------------------------------------------------
      INTEGER :: ice_alloc
      !
      INTEGER :: ii
      !!-----------------------------------------------------------------
      ii = 0
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      
      ! * Ice global state variables
      ALLOCATE( u_ice(jpi,jpj) , v_ice(jpi,jpj), at_i(jpi,jpj), vt_i(jpi,jpj), STAT=ii )
     
      ice_alloc = ii
      CALL mpp_sum ( 'ice', ice_alloc )
      IF( ice_alloc /= 0 )   CALL ctl_stop( 'STOP', 'ice_alloc: failed to allocate arrays.' )
      !

   END FUNCTION ice_alloc
#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE ice
