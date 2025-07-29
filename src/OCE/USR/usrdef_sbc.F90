MODULE usrdef_sbc
   !!======================================================================
   !!                     ***  MODULE  usrdef_sbc  ***
   !!
   !!                     ===  GYRE configuration  ===
   !!
   !! User defined :   surface forcing of a user configuration
   !!======================================================================
   !! History :  4.0   ! 2016-03  (S. Flavoni, G. Madec)  user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usrdef_sbc    : user defined surface bounday conditions in GYRE case
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran    !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usrdef_sbc_oce       ! routine called in sbcmod module
   PUBLIC   usrdef_sbc_ice_tau   ! routine called by icestp.F90 for ice dynamics
   PUBLIC   usrdef_sbc_ice_flx   ! routine called by icestp.F90 for ice thermo

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "single_precision_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_sbc.F90 15145 2021-07-26 16:16:45Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usrdef_sbc_oce( kt, Kbb )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE usrdef_sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the GYRE surface boundary
      !!              condition, i.e. the momentum, heat and freshwater fluxes.
      !!
      !! ** Method  :   analytical seasonal cycle for GYRE configuration.
      !!                CAUTION : never mask the surface stress field !
      !!
      !! ** Action  : - set the ocean surface boundary condition, i.e.   
      !!                   utau, vtau, taum, wndm, qns, qsr, emp, sfx
      !!
      !! Reference : Hazeleger, W., and S. Drijfhout, JPO, 30, 677-695, 2000.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb  ! ocean time index
      !!
      INTEGER  ::   ji, jj                 ! dummy loop indices
      !!----------------------------------------------------------------------

      ! freshwater (mass flux) and update of qns with heat content of emp
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         emp (ji,jj) = 0.0_wp        ! freshwater flux
         sfx (ji,jj) = 0.0_wp        ! no salt flux
         qns (ji,jj) = 0.0_wp        ! non solar heat flux
         qsr (ji,jj) = 0.0_wp        ! solar heat flux
      END_2D

      ! ---------------------------- !
      !       momentum fluxes        !
      ! ---------------------------- !
      !DO_2D( 1, 1, 1, 1 )
      !   utau(ji,jj) = 0.0_wp
      !   vtau(ji,jj) = 0.0_wp
      !END_2D
      DO_2D( 0, 0, 0, 0 )
         utau(ji,jj) = 0.0_wp
         vtau(ji,jj) = 0.0_wp
         taum(ji,jj) = 0.0_wp
         wndm(ji,jj) = 0.0_wp
      END_2D
      !CALL lbc_lnk( 'usrdef_sbc', taum(:,:), 'T', 1. , wndm(:,:), 'T', 1.)
 
      ! ---------------------------------- !
      !  control print at first time-step  !
      ! ---------------------------------- !
      IF( kt == nit000 .AND. lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*)'usrdef_sbc_oce : all surface fluxes set to 0'
         WRITE(numout,*)'~~~~~~~~~~~ '
      ENDIF
      !
   END SUBROUTINE usrdef_sbc_oce


   SUBROUTINE usrdef_sbc_ice_tau( kt )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
   END SUBROUTINE usrdef_sbc_ice_tau


   SUBROUTINE usrdef_sbc_ice_flx( kt, phs, phi )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phs    ! snow thickness
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phi    ! ice thickness
   END SUBROUTINE usrdef_sbc_ice_flx

   !!======================================================================
END MODULE usrdef_sbc
