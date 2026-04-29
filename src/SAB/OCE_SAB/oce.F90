MODULE oce
   !!======================================================================
   !!                      ***  MODULE  oce  ***
   !! Ocean        :  dynamics and active tracers defined in memory 
   !!======================================================================
   !! History :  1.0  !  2002-11  (G. Madec)  F90: Free form and module
   !!            3.1  !  2009-02  (G. Madec, M. Leclair)  pure z* coordinate
   !!            3.3  !  2010-09  (C. Ethe) TRA-TRC merge: add ts, gtsu, gtsv 4D arrays
   !!            3.7  !  2014-01  (G. Madec) suppression of curl and before hdiv from in-core memory
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rename prognostic variables in preparation for new time scheme
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC oce_alloc       ! routine called by nemo_init in     nemogcm.F90
   PUBLIC oce_dealloc     ! routine called by nemo_init in     nemogcm.F90

   !! dynamics and tracer fields
   !! --------------------------                            
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:), TARGET   ::   uu   ,  vv   !: horizontal velocities        [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) ::   ts             !: 4D T-S fields                  [Celsius,psu] 

#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION oce_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  FUNCTION oce_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER               ::   ii
      INTEGER, DIMENSION(7) :: ierr
      !!----------------------------------------------------------------------
      !
      ii = 0   ;   ierr(:) = 0 
      !
      ii = ii+1
      ALLOCATE( uu   (jpi,jpj,jpk,jpt)  , vv   (jpi,jpj,jpk,jpt)           ,     &          
         &      ts   (jpi,jpj,jpk,jpts,jpt) )
         !
      ii = ii+1
      !
      oce_alloc = MAXVAL( ierr )
      IF( oce_alloc /= 0 )   CALL ctl_stop( 'STOP', 'oce_alloc: failed to allocate arrays' )
      !
   END FUNCTION oce_alloc


   SUBROUTINE oce_dealloc()
      DEALLOCATE( uu, vv, ts )
         !
   END SUBROUTINE oce_dealloc

   !!======================================================================
END MODULE oce
