MODULE isftrc_cpl
   !!======================================================================
   !!                       ***  MODULE  isfcpl  ***
   !!
   !! iceshelf coupling module : module managing the coupling between NEMO and an ice sheet model
   !!
   !!======================================================================
   !! History :  4.1  !  2019-07  (P. Mathiot) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfrst        : read/write iceshelf variables in/from restart
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
#if defined key_qco
   USE domqco  , ONLY : dom_qco_zgr      ! vertical scale factor interpolation
#elif defined key_linssh
   !                                     ! fix in time coordinate
#else
   USE domvvl  , ONLY : dom_vvl_zgr      ! vertical scale factor interpolation
#endif
   USE domutl  , ONLY : dom_ngb          ! find the closest grid point from a given lon/lat position
   USE isf_oce        ! ice shelf variable
   USE isftrc_oce     ! trc shelf variable 
   USE isfutils, ONLY : debug
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE lib_mpp , ONLY : mpp_sum, mpp_max ! mpp routine
   !
   USE par_trc , ONLY : jptra, jpmaxtrc
   USE isftrc_oce
   !

   IMPLICIT NONE

   PRIVATE
   !
   PUBLIC isftrc_cpl_init, update_isfptr,get_correction_pt  ! iceshelf restart read and write
   !
   !!---------------------------------------------------------------------
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "single_precision_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcisf.F90 10536 2019-01-16 19:21:09Z mathiot $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE isftrc_cpl_init(Kbb, Kmm, Kaa)
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE iscpl_init  ***
      !!
      !! ** Purpose : correct ocean state for new wet cell and horizontal divergence
      !!              correction for the dynamical adjustement
      !!
      !! ** Action : - compute ssh on new wet cell
      !!             - compute T/S on new wet cell
      !!             - compute horizontal divergence correction as a volume flux
      !!             - compute the T/S/vol correction increment to keep trend to 0
      !!
      !!---------------------------------------------------------------------
      USE trc,         ONLY: tr
      USE isfcpl,      ONLY: isfcpl_tr, isfcpl_cons, id ! extend into new opened cells.
      !! 
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa      ! ocean time level indices
      !!----------------------------------------------------------------------
      !
      ! allocation and initialisation to 0
      CALL isftrc_alloc_cpl()
      !
      IF(lwp) WRITE(numout,*) ' isftrc_cpl_init:', id
      IF (id == 0) THEN
         IF(lwp) WRITE(numout,*) ' isftrc_cpl_init: restart variables for ice sheet coupling are missing, skip coupling for this leg '
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'
         IF(lwp) WRITE(numout,*) ''
      ELSE
         !
         ! extrapolation tracer properties
         !CALL isfcpl_tr(Kmm,'TRA',ts,2)
         !
         ! correction of the horizontal divergence and associated temp. and salt content flux
         ! Need to : - include in the cpl cons the risfcpl_vol/tsc contribution
         !           - decide how to manage thickness level change in conservation
         !CALL isfcpl_vol(Kmm)
         !
         ! apply the 'conservation' method
         !IF ( ln_isfcpl_cons ) CALL isfcpl_cons(Kmm,'TRA',ts,2)
         !
         IF( ln_isfcpl ) THEN
            IF (id == 0) THEN
              IF(lwp) WRITE(numout,*) ' trc_ini_state: restart variables for ice sheet coupling are missing, skip coupling for this leg '
              IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~'
              IF(lwp) WRITE(numout,*) ' '
            ELSE
              !! run isfcpl.
              !! but first - check the inventory before, just to make sure all is OK
              !CALL trc_ini_inv( Kmm ) !! check no NaNs before isfcpl_tr call 
              !CALL flush(numout)
              CALL isfcpl_tr(Kmm, 'TRC', tr, jptra)
              IF(lwp) WRITE(numout,*) ' trcini -- isfcpl_tr done'
              !         !
              ! apply the 'conservation' method
              IF(lwp) WRITE(numout,*) ' trcini -- isfcpl_cons starts '
              IF ( ln_isfcpl_cons ) CALL isfcpl_cons(Kmm,'TRC', tr, jptra)
              IF(lwp) WRITE(numout,*) ' trcini -- isfcpl_cons done '
            ENDIF !! id
         ENDIF  !! ln_isfcpl

         !
      END IF
      !
      !
      ! all before fields set to now values
      tr  (:,:,:,:,Kbb) = tr  (:,:,:,:,Kmm)
      !uu   (:,:,:,Kbb)   = uu   (:,:,:,Kmm)
      !vv   (:,:,:,Kbb)   = vv   (:,:,:,Kmm)
      !ssh (:,:,Kbb)     = ssh (:,:,Kmm)
   END SUBROUTINE isftrc_cpl_init


   SUBROUTINE update_isfptr(sisfpts, kpts, ki, kj, kk, pdtr, pratio, kfind)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE update_isfpts  ***
      !!
      !! ** Purpose : if a cell become dry, we need to put the corrective increment elsewhere
      !!
      !! ** Action  : update the list of point
      !!
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      TYPE(isfconspt), DIMENSION(:), INTENT(inout) :: sisfpts
      INTEGER                      , INTENT(inout) :: kpts
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   )           :: ki, kj, kk !    target location (kfind=0)
      !                                                                 ! or source location (kfind=1)
      INTEGER                   , INTENT(in   ), OPTIONAL :: kfind      ! 0  target cell already found
      !                                                                 ! 1  target to be determined
      REAL(wp), DIMENSION(jpmaxtrc), INTENT(in   )        :: pdtr       ! passive tracer increment
      REAL(wp)                  , INTENT(in   )           :: pratio     ! and ratio in case increment span over multiple cells.
      !!----------------------------------------------------------------------
      INTEGER :: ifind, jn
      !!----------------------------------------------------------------------
      !
      ! increment position
      kpts = kpts + 1
      !
      ! define if we need to look for closest valid wet cell (no neighbours or neighbourg on halo)
      IF ( PRESENT(kfind) ) THEN
         ifind = kfind
      ELSE
         ifind = ( 1 - tmask_i(ki,kj) ) * tmask(ki,kj,kk)
      END IF
      !
      ! update isfpts structure
      sisfpts(kpts) = isfconspt(mig(ki), mjg(kj), kk, pratio * pdtr, glamt(ki,kj), gphit(ki,kj), ifind )
      !
   END SUBROUTINE update_isfptr
   !
   SUBROUTINE get_correction_pt( ki, kj, kk, plon, plat, ptrlinc, kfind)
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE get_correction  ***
      !!
      !! ** Action : - Find the closest valid cell if needed (wet and not on the halo)
      !!             - Scale the correction depending of pratio (case where multiple wet neighbourgs)
      !!             - Fill the correction array
      !!
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in) :: ki, kj, kk, kfind        ! target point indices
      REAL(wp)                  , INTENT(in) :: plon, plat               ! target point lon/lat
      REAL(wp), DIMENSION(jpmaxtrc), INTENT(in) :: ptrlinc                  ! correction increment for vol/temp/salt
      !!----------------------------------------------------------------------
      INTEGER :: jj, ji, jn, iig, ijg
      !!----------------------------------------------------------------------
      !
      ! define global indice of correction location
      iig = ki ; ijg = kj
      IF ( kfind == 1 ) CALL dom_ngb( plon, plat, iig, ijg,'T', kk)
      !
      ! fill the correction array
      DO jj = mj0(ijg),mj1(ijg)
         DO ji = mi0(iig),mi1(iig)
            ! correct the vol_flx and corresponding heat/salt flx in the closest cell
            DO jn = 1,jptra
               risfcpl_cons_trc(ji,jj,kk,jn) =  risfcpl_cons_trc(ji,jj,kk,jn) + ptrlinc(jn)
            ENDDO
         END DO
      END DO

   END SUBROUTINE get_correction_pt
   !
END MODULE isftrc_cpl
