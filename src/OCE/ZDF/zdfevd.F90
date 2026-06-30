MODULE zdfevd
   !!======================================================================
   !!                       ***  MODULE  zdfevd  ***
   !! Ocean physics: parameterization of convection through an enhancement
   !!                of vertical eddy mixing coefficient
   !!======================================================================
   !! History :  OPA  !  1997-06  (G. Madec, A. Lazar)  Original code
   !!   NEMO     1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!            3.2  !  2009-03  (M. Leclair, G. Madec, R. Benshila) test on both before & after
   !!            4.0  !  2017-04  (G. Madec)  evd applied on avm (at t-point) 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_evd       : increase the momentum and tracer Kz at the location of
   !!                   statically unstable portion of the water column (ln_zdfevd=T)
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables
   USE zdf_oce         ! ocean vertical physics variables
   USE trd_oce         ! trends: ocean variables
   USE trdtra          ! trends manager: tracers 
   !
   USE in_out_manager  ! I/O manager
   USE iom             ! for iom_put
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_evd    ! called by zdfphy.F90
   
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) ::   avt_evd   !tracer evd mixing coefficient [m2/s]
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_evd( kt, Kmm, Krhs, p_avm, p_avt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_evd  ***
      !!                   
      !! ** Purpose :   Local increased the vertical eddy viscosity and diffu-
      !!      sivity coefficients when a static instability is encountered.
      !!
      !! ** Method  :   tracer (and momentum if nn_evdm=1) vertical mixing 
      !!              coefficients are set to rn_evd (namelist parameter) 
      !!              if the water column is statically unstable.
      !!                The test of static instability is performed using
      !!              Brunt-Vaisala frequency (rn2 < -1.e-12) of to successive
      !!              time-step : before time-step.
      !!
      !! ** Action  :   avt, avm   enhanced where static instability occurs
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   kt             ! ocean time-step indexocean time step
      INTEGER                         , INTENT(in   ) ::   Kmm, Krhs      ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   p_avm          ! vertical eddy viscosity (w-points)
      REAL(wp), DIMENSION(A2D(0) ,jpk), INTENT(inout) ::   p_avt          ! vertical eddy diffusivity (w-points)
      !
      INTEGER       ::   ji, jj, jk   ! dummy loop indices
      LOGICAL, SAVE ::   l_diag
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'zdf_evd : Enhanced Vertical Diffusion (evd)'
            IF(lwp) WRITE(numout,*) '~~~~~~~ '
            IF(lwp) WRITE(numout,*)
            l_diag = l_trdtra .OR. iom_use('avt_evd')      .OR. iom_use('avm_evd') &
               &              .OR. iom_use('ttrd_zdf_evd') .OR. iom_use('strd_zdf_evd')
            IF( l_diag )   ALLOCATE( avt_evd(A2D(0),jpk) ) 
        ENDIF
      ENDIF
      !
 
      !==  enhance tracer Kz  ==!   (if rn2<-1.e-12)
      IF( l_diag ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk )
            avt_evd(ji,jj,jk) = p_avt(ji,jj,jk)         ! set avt prior to evd application
         END_3D
      ENDIF
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         IF( rn2b(ji,jj,jk) <= -1.e-12 )   p_avt(ji,jj,jk) = rn_evd * wmask(ji,jj,jk)
      END_3D

      IF( l_diag ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk )
            avt_evd(ji,jj,jk) = p_avt(ji,jj,jk) - avt_evd(ji,jj,jk)        ! change in avt due to evd
         END_3D
         CALL iom_put( "avt_evd", avt_evd )             ! output this change
      ENDIF

      !==  enhance momentum Kz  ==!   (if rn2<-1.e-12) (note that avm_evd = avt_evd)
      IF( nn_evdm == 1 ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( rn2b(ji,jj,jk) <= -1.e-12 )   p_avm(ji,jj,jk) = rn_evd * wmask(ji,jj,jk)
         END_3D
         !
         IF( l_diag ) THEN
            CALL iom_put( "avm_evd", avt_evd )   ! note that avm_evd = avt_evd
         ENDIF
      ENDIF
      !
   END SUBROUTINE zdf_evd

   !!======================================================================
END MODULE zdfevd
