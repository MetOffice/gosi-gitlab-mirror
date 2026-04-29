MODULE sbc_ice
   !!======================================================================
   !!                 ***  MODULE  sbc_ice  ***
   !! Surface module - SI3 & CICE: parameters & variables defined in memory
   !!======================================================================
   !! History :  3.0   !  2006-08  (G. Madec)        Surface module
   !!            3.2   !  2009-06  (S. Masson)       merge with ice_oce
   !!            3.3.1 !  2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.4   !  2011-11  (C. Harris)       CICE added as an option
   !!            4.0   !  2018     (many people)     SI3 compatibility
   !!----------------------------------------------------------------------
   USE lib_mpp          ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ice_alloc   ! called in sbcmod.F90 or sbcice_cice.F90
   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass        !: mass of snow and ice at current  ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass_b      !: mass of snow and ice at previous ice time step   [Kg/m2]

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_ice_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION sbc_ice_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ii
      !!----------------------------------------------------------------------
      ii = 0

      ALLOCATE( snwice_mass(jpi,jpj) , snwice_mass_b(jpi,jpj) , STAT=ii )

      sbc_ice_alloc = ii
      CALL mpp_sum ( 'sbc_ice', sbc_ice_alloc )
      IF( sbc_ice_alloc > 0 )   CALL ctl_warn('sbc_ice_alloc: allocation of arrays failed')
   END FUNCTION sbc_ice_alloc

END MODULE sbc_ice
