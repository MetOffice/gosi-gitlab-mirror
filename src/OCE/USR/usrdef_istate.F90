MODULE usrdef_istate
   !!======================================================================
   !!                   ***  MODULE  usrdef_istate   ***
   !!
   !!                     ===  GYRE configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!======================================================================
   !! History :  4.0  ! 2016-03  (S. Flavoni) Original code
   !!                 ! 2020-11  (S. Techene, G. Madec) separate tsuv from ssh
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate       ! called in istate.F90
   PUBLIC   usr_def_istate_ssh   ! called by domqco.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_istate.F90 14834 2021-05-11 09:24:44Z hadcv $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   ELEMENTAL FUNCTION ana_prof(pz, pb, pa, pzt, pdt, pg, pzm, pw, pc, ph) RESULT(pf)
      REAL(wp), INTENT(in) :: pz, pb, pa, pzt, pdt, pg, pzm, pw, pc, ph
      REAL(wp)             :: pf
      pf = pb + 0.5_wp * pa * (1._wp - TANH((pz - pzt) / pdt))   &
         &    + pg * EXP(-((pz - pzm) / pw)**2)                  &
         &    + pc * EXP(-pz / ph)
   END FUNCTION ana_prof

   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here GYRE configuration example : (double gyre with rotated domain)
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity   field
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(dp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(dp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s]
      REAL(dp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s]
      !
      ! Conservative temperature coefficients
      REAL(wp), PARAMETER :: rt_b  =    1.5196_wp, rt_a  =    1.8005_wp
      REAL(wp), PARAMETER :: rt_zt =  124.95_wp,   rt_dt =   36.73_wp
      REAL(wp), PARAMETER :: rt_g  =    3.4597_wp, rt_zm = 1224.21_wp, rt_w = 505.54_wp
      REAL(wp), PARAMETER :: rt_c  =   15.0181_wp, rt_h  = 1160.54_wp

      ! Absolute salinity coefficients
      REAL(wp), PARAMETER :: rs_b  =   35.0910_wp, rs_a  =    0.5879_wp
      REAL(wp), PARAMETER :: rs_zt =  150.97_wp,   rs_dt =   92.20_wp
      REAL(wp), PARAMETER :: rs_g  =    0.8449_wp, rs_zm = 1184.02_wp, rs_w = 507.13_wp
      REAL(wp), PARAMETER :: rs_c  =    1.0404_wp, rs_h  = 1069.86_wp

      INTEGER :: ji, jj, jk  ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : analytical definition of initial state '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   Ocean at rest, with an horizontally uniform T and S profiles'
      !
      pu  (:,:,:) = 0._wp           ! ocean at rest
      pv  (:,:,:) = 0._wp
      !
      !  Analytical potential temperature / salinity profiles representative
      !  of a Mediterranean Overflow Water (MOW) environment.
      !
      !  F(z) = Fb + A/2 [1 - tanh((z-zt)/dt)] + B exp(-((z-zm)/w)^2) + C exp(-z/H)
      !
      !  z is depth [m], positive downward. Coefficients were fitted to a
      !  reference profile with the constraint that the resulting sigma0
      !  (TEOS10) increases monotonically with depth (dsigma0/dz > 0).
      !
      
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         pts(ji,jj,jk,jp_sal) = ana_prof(pdept(ji,jj,jk), rs_b, rs_a, rs_zt, rs_dt, &
                 &                       rs_g, rs_zm, rs_w, rs_c, rs_h)
         pts(ji,jj,jk,jp_tem) = ana_prof(pdept(ji,jj,jk), rt_b, rt_a, rt_zt, rt_dt, &
                 &                       rt_g, rt_zm, rt_w, rt_c, rt_h)
      END_3D

      !   
   END SUBROUTINE usr_def_istate

   
   SUBROUTINE usr_def_istate_ssh( ptmask, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate_ssh  ***
      !! 
      !! ** Purpose :   Initialization of ssh
      !!
      !! ** Method  :   Set ssh as null, ptmask is required for test cases
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask   [m]
      REAL(dp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height   [m]
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate_ssh : GYRE configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~   Ocean at rest, ssh is zero'
      !
      ! Sea level:
      pssh(:,:) = 0._wp
      !
   END SUBROUTINE usr_def_istate_ssh

   !!======================================================================
END MODULE usrdef_istate
