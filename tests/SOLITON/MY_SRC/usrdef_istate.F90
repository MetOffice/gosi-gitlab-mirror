MODULE usrdef_istate
   !!======================================================================
   !!                     ***  MODULE usrdef_istate   ***
   !!
   !!                      ===  SOLITON configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!======================================================================
   !! History :  NEMO ! 2026-06  (J. Chanut) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE dom_oce , ONLY : glamt, gphit, glamu, gphiu, glamv, gphiv  
   USE phycst         ! physical constants
   USE eosbn2  , ONLY : rn_T0, rn_S0
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! to use sign with key_nosignedzero
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   !   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate       ! called by istate.F90
   PUBLIC   usr_def_istate_ssh   ! called by domqco.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here SOLITON configuration 
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s] 
      !
      INTEGER  :: ji, jj, jk  ! dummy loop indices
      REAL(wp) :: zx, zy, zval1, zval2, zval3, zval4
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : SOLITON configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   '
      !
      ! Homogenous T/S::  
      pts(:,:,:,jp_sal) = rn_S0 * ptmask(:,:,:) 
      pts(:,:,:,jp_tem) = rn_T0 * ptmask(:,:,:) 
      !
      ! velocities:
      zval1 = 0.395_wp
      zval2 = 0.771_wp*(zval1*zval1)

      DO_2D( 0, 0, 0, 0 )
         zx = glamu(ji,jj)
         zy = gphiu(ji,jj)
         zval3 = EXP(-zval1*zx)
         zval4 = zval2*((2.0_wp*zval3/(1.0_wp+(zval3*zval3)))**2)
         DO jk=1, jpk
          pu(ji,jj,jk) = 0.25_wp * zval4 * (6.0_wp*zy*zy-9.0_wp)        &
          &                      * EXP(-0.5_wp*zy*zy)                   &
          &                      * ptmask(ji,jj,jk) * ptmask(ji+1,jj,jk)
         END DO
      END_2D

      DO_2D( 0, 0, 0, 0 )
         zx = glamv(ji,jj)
         zy = gphiv(ji,jj)
         zval3 = EXP(-zval1*zx)
         zval4 = zval2*((2.0_wp*zval3/(1.0_wp+(zval3*zval3)))**2)
         DO jk=1, jpk
          pv(ji,jj,jk) = 2.0_wp * zval4 * zy * (-2.0_wp*zval1*TANH(zval1*zx))  &
          &                     * EXP(-0.5_wp*zy*zy)                           & 
          &                     * ptmask(ji,jj,jk) * ptmask(ji,jj+1,jk)
         END DO
      END_2D
      !
      CALL lbc_lnk( 'usrdef_istate', pu, 'U', -1._wp, pv, 'V', -1._wp )
      !   
   END SUBROUTINE usr_def_istate


   SUBROUTINE usr_def_istate_ssh( ptmask, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of ssh
      !!                Here SOLITON configuration 
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask   [m]
      REAL(wp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height   [m]
      !
      INTEGER  :: ji, jj ! dummy loop indices
      REAL(wp) :: zx, zy, zval1, zval2, zval3, zval4
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate_ssh : SOLITON configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   '
      !
      zval1 = 0.395_wp
      zval2 = 0.771_wp*(zval1*zval1)

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zx = glamt(ji,jj)
         zy = gphit(ji,jj)
         zval3 = EXP(-zval1*zx)
         zval4 = zval2*((2.0_wp*zval3/(1.0_wp+(zval3*zval3)))**2)
         pssh(ji,jj) = 0.25_wp * zval4 * (6.0_wp*zy*zy+3.0_wp)    &
         &                    * EXP(-0.5_wp*zy*zy)                &
         &                    * ptmask(ji,jj,1)
     END_2D
      
   END SUBROUTINE usr_def_istate_ssh

   !!======================================================================
END MODULE usrdef_istate
