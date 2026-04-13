MODULE tslsal
   !!======================================================================
   !!                       ***  MODULE  tslsal  ***
   !! Tides, self-attraction, and loading: parameterisations of the
   !!   self-attraction and loading (SAL) potential
   !!======================================================================
   !! History : 5.1  ! 2025  (S. Mueller)  Separation of SAL-parameterisation initialisations from the NEMO tides module
   !!----------------------------------------------------------------------
   !!   tsl_sal_init : Initialisation of the SAL-potential parameterisation
   !!----------------------------------------------------------------------
   USE par_oce          ! Ocean parameters
   USE in_out_manager   ! I/O unit numbers and flags
   USE tsltde           ! Access to the tides module is required for the tidal oscillatory SAL perameterisation
   USE iom              ! Input routines

   IMPLICIT NONE
   PRIVATE

   ! Configuration of the SAL potential
   LOGICAL,  PUBLIC ::   ln_tslsal          ! SAL potential activation state
   LOGICAL,  PUBLIC ::   ln_tslsal_scalar   !: Activation state of the scalar SAL-potential parameterisation (proportional to SSH)
   REAL(wp), PUBLIC ::   rn_tslsal_scalar   !: Proportionality coefficient for scalar SAL-potential parameterisation
   LOGICAL,  PUBLIC ::   ln_tslsal_osc      ! Activation state of the tidal oscillatory SAL-potential parameterisation
   CHARACTER(lc)    ::   cn_tslsal_osc      ! Input filename for the tidal oscillatory SAL-potential parameterisation

   PUBLIC tsl_sal_init   ! Called by nemo_init (module nemogcm)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.1.a, NEMO Consortium (2026)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tsl_sal_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE  tsl_sal_init  ***
      !!----------------------------------------------------------------------
      INTEGER ::   ios                                   ! Input status flag
      INTEGER ::   inum                                  ! Input-file unit identifier
      INTEGER ::   ivar                                  ! Variant counter
      INTEGER ::   ji, jj, jtde                          ! Loop indices
      REAL(wp), DIMENSION(jpi,jpj)   ::   zreal, zimag   ! Phasor map for tidal oscillatory SAL potential
      REAL(wp), DIMENSION(jpi,jpj,2) ::   zampphi        ! Amplitude and phase maps for tidal oscillatory SAL potential
      !
      NAMELIST/namtsl_sal/ln_tslsal,                          &
         &                ln_tslsal_scalar, rn_tslsal_scalar, &
         &                ln_tslsal_osc,    cn_tslsal_osc
      !!----------------------------------------------------------------------
      !
      ! Read namelist namtsl_sal
      READ_NML_REF(numnam,namtsl_sal)
      READ_NML_CFG(numnam,namtsl_sal)
      IF(lwm) WRITE( numond, namtsl_sal )
      !
      IF( .NOT. ln_tslsal ) THEN
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'tslsal_init : no self-attraction and loading (SAL) potential (ln_tslsal = F)'
            WRITE(numout,*) '~~~~~~~~~~~'
         END IF
         !
         ! Ensure SAL parameterisations are disabled
         ln_tslsal_scalar = .FALSE.
         ln_tslsal_osc    = .FALSE.
      ELSE
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'tslsal_init : initialisation of self-attraction and loading (SAL) potential'
            WRITE(numout,*) '~~~~~~~~~~~'
            WRITE(numout,*) '   Namelist namtsl_sal'
            WRITE(numout,*) '      Activate SAL                                        ln_tslsal        = ', ln_tslsal
            WRITE(numout,*) '         Scalar parameterisation (SSH proportionality)    ln_tslsal_scalar = ', ln_tslsal_scalar
            WRITE(numout,*) '            SSH-proportionality coefficient               rn_tslsal_scalar = ', rn_tslsal_scalar
            WRITE(numout,*) '         Tidal oscillatory SAL-potential parameterisation ln_tslsal_osc    = ', ln_tslsal_osc
            WRITE(numout,*) '            Filename for tidal-analysis input             cn_tslsal_osc    = ', TRIM( cn_tslsal_osc )
         END IF
         !
         ! Dependencies, prerequisites, and warnings
         IF( ln_tslsal_osc .AND. .NOT. ln_tsltde ) CALL ctl_stop( 'ln_tslsal_osc requires active tidal constituents (ln_tsltde=T)' )
         ivar = 0
         IF( ln_tslsal_scalar ) ivar = ivar + 1
         IF( ln_tslsal_osc )    ivar = ivar + 1
         IF( ivar /= 1 ) CALL ctl_stop( 'tslsal_init: no or multiple SAL-potential options have been selected' )
      END IF
      !
      ! Initialise tidal oscillatory SAL potential
      IF( ln_tslsal_osc ) THEN
         !
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'tslsal_init : initialisation of tidal oscillatory SAL-potential parameterisation'
            WRITE(numout,*) '~~~~~~~~~~~'
         END IF
         !
         ! Read in a tidal-harmonic analysis for each active tidal constituent
         CALL iom_open( cn_tslsal_osc, inum )
         DO jtde = 1, SIZE( stsltde_harmonics(:)%cname_tide )
            ! Read in a phasor map,
            CALL iom_get( inum, jpdom_global, TRIM( stsltde_harmonics(jtde)%cname_tide ) // '_z1', zreal(:,:) )
            CALL iom_get( inum, jpdom_global, TRIM( stsltde_harmonics(jtde)%cname_tide ) // '_z2', zimag(:,:) )
            !    convert it into a map of amplitude and phase pairs, and
            DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
               zampphi(ji,jj,1) = SQRT( zreal(ji,jj)**2 + zimag(ji,jj)**2 )
               zampphi(ji,jj,2) = ATAN2( -1.0_wp * zreal(ji,jj), zimag(ji,jj) )
            END_2D
            !    supply the map as an external contribution to the tidal-potential computation
            CALL tsl_tde_init_pot_ext( jtde, zampphi )
         END DO
         CALL iom_close( inum )
         !
      END IF
      !
   END SUBROUTINE tsl_sal_init

END MODULE tslsal
