MODULE tsltde
   !!======================================================================
   !!                       ***  MODULE  tsltde  ***
   !! Compute nodal modulations corrections and pulsations
   !!======================================================================
   !! History :  1.0  !  2007  (O. Le Galloudec)  Original code
   !!                 !  2019  (S. Mueller)
   !!----------------------------------------------------------------------
   !!
   !! ** Reference :
   !!       S58) Schureman, P. (1958): Manual of Harmonic Analysis and
   !!            Prediction of Tides (Revised (1940) Edition (Reprinted 1958
   !!            with corrections). Reprinted June 2001). U.S. Department of
   !!            Commerce, Coast and Geodetic Survey Special Publication
   !!            No. 98. Washington DC, United States Government Printing
   !!            Office. 317 pp. DOI: 10.25607/OBP-155.
   !!----------------------------------------------------------------------

   USE oce, ONLY : ssh        ! sea-surface height
   USE par_oce                ! ocean parameters
   USE phycst, ONLY : rpi, rad, rday
   USE daymod, ONLY : ndt05   ! half-length of time step
   USE in_out_manager         ! I/O units
   USE iom                    ! xIOs server

   IMPLICIT NONE
   PRIVATE

   ! Public procedures
   PUBLIC   tsl_tde_init           ! Called by nemo_init (module nemogcm)
   PUBLIC   tsl_tde_init_osc       ! Called by dia_detide_init (module diadetide) and dia_mlr_init (module diamlr)
   PUBLIC   tsl_tde_stp            ! Called by stp (module stprk3)
   PUBLIC   tsl_tde_pot_upd        ! Called by dyn_spg (module dynspg) and dyn_spg_ts (module dynspg_ts)
   PUBLIC   tsl_tde_init_pot_ext   ! Called by tsl_sal_init (module tslsal)

   INTEGER, PUBLIC, PARAMETER ::   jptsltde_max = 64   !: maximum number of tidal constituents

   TYPE ::    tide
      CHARACTER(LEN=4) ::   cname_tide = ''
      REAL(wp)         ::   equitide
      INTEGER          ::   nt, ns, nh, np, np1, shift
      INTEGER          ::   nksi, nnu0, nnu1, nnu2, R
      INTEGER          ::   nformula
   END TYPE tide

   TYPE(tide), ALLOCATABLE, DIMENSION(:) ::   tide_components   !: Array of selected tidal component parameters

   TYPE, PUBLIC ::   tsltde_harmonic     !:   Oscillation parameters of a harmonic tidal constituent
      CHARACTER(LEN=4) ::   cname_tide   !    Name of component
      REAL(wp)         ::   equitide     !    Amplitude of equilibrium tide
      REAL(wp)         ::   f            !    Node factor
      REAL(wp)         ::   omega        !    Angular velocity
      REAL(wp)         ::   v0           !    Initial phase at prime meridian
      REAL(wp)         ::   u            !    Phase correction
   END type tsltde_harmonic

   TYPE(tsltde_harmonic), PUBLIC, ALLOCATABLE, DIMENSION(:) ::   stsltde_harmonics   !: Osc. parameters of the selected constituents

   ! Variables in namelist group namtsl_tde
   LOGICAL , PUBLIC ::   ln_tsltde           !: Flag to indicate the activation of tidal constituents
   INTEGER          ::   nn_tsltde_var       !  Variant of tidal parameter set and tide-potential computation
   LOGICAL , PUBLIC ::   ln_tsltde_pot       !: Flag to indicate the application of a tide-generating potential
   REAL(wp)         ::   rn_tsltde_gamma     !  Tidal tilt factor
   LOGICAL , PUBLIC ::   ln_tsltde_ramp      !: Flag to indicate phasing in of tidal forcing
   REAL(wp), PUBLIC ::   rn_tsltde_ramp_dt   !: Phasing-in duration

   LOGICAL          ::   ll_diatide          !  Diagnostic-output flag
   INTEGER , PUBLIC ::   ntsltde_harmo       !: Number of active tidal constituents

   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)     ::   tsltde_pot         !: tide-generating potential
   REAL(wp),         ALLOCATABLE, DIMENSION(:,:,:)   ::   pot_astro_comp     ! Tidal-potential components for diagnostic output
   REAL(wp),         ALLOCATABLE, DIMENSION(:,:,:)   ::   amp_pot, phi_pot   ! Amplitudes and phases for tidal potential
   REAL(wp),         ALLOCATABLE, DIMENSION(:,:,:,:) ::   ampphi_pot_ext     ! External contributions to the tidal potential

   !! * Substitutions
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.1.a, NEMO Consortium (2026)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tsl_tde_init
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE tsl_tde_init  ***
      !!----------------------------------------------------------------------      
      INTEGER  :: ji, jk
      CHARACTER(LEN=4), DIMENSION(jptsltde_max) ::   cn_tsltde_constituent   ! Names of selected tidal components
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      !
      NAMELIST/namtsl_tde/ln_tsltde,      cn_tsltde_constituent, nn_tsltde_var, ln_tsltde_pot, rn_tsltde_gamma,   &
         &                ln_tsltde_ramp, rn_tsltde_ramp_dt
      !!----------------------------------------------------------------------
      !
      ! Initialise the elements of array cn_tsltde_constituent (typically, not all entries are set via namelist-group input)
      cn_tsltde_constituent(:) = ''
      ! Read namelist group namtsl_tde
      READ_NML_REF(numnam,namtsl_tde)
      READ_NML_CFG(numnam,namtsl_tde)
      IF(lwm) WRITE ( numond, namtsl_tde )
      !
      IF( ln_tsltde ) THEN
         IF (lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'tsl_tde_init : Initialization of the tidal components'
            WRITE(numout,*) '~~~~~~~~~~~~ '
            WRITE(numout,*) '   Namelist namtsl_tde'
            WRITE(numout,*) '      Use tidal components                       ln_tsltde         = ', ln_tsltde
            WRITE(numout,*) '         Variant (1: default; 0: legacy option)  nn_tsltde_var     = ', nn_tsltde_var
            WRITE(numout,*) '         Apply astronomical potential            ln_tsltde_pot     = ', ln_tsltde_pot
            WRITE(numout,*) '            Tidal tilt factor                    rn_tsltde_gamma   = ', rn_tsltde_gamma
            WRITE(numout,*) '         Apply ramp on tides at startup          ln_tsltde_ramp    = ', ln_tsltde_ramp
            WRITE(numout,*) '            Duration (days) of ramp              rn_tsltde_ramp_dt = ', rn_tsltde_ramp_dt
         ENDIF
      ELSE
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'tsl_tde_init : tidal components not used (ln_tsltde = F)'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~ '
         RETURN
      ENDIF
      !
      IF( ln_tsltde_ramp .AND. ( ( nitend - nit000 + 1 ) * rn_Dt / rday < rn_tsltde_ramp_dt ) )   &
         &   CALL ctl_stop( 'rn_tsltde_ramp_dt must be less than the run length' )
      IF( ln_tsltde_ramp .AND. ( rn_tsltde_ramp_dt < 0.0_wp ) ) CALL ctl_stop( 'rn_tsltde_ramp_dt must be positive' )
      !
      ! Initialise array used to store tidal oscillation parameters (frequency,
      ! amplitude, phase); also retrieve and store array of information about
      ! selected tidal components
      CALL tsl_tde_init_osc( cn_tsltde_constituent, stsltde_harmonics, tide_components )
      !
      ! Number of active tidal components
      ntsltde_harmo = SIZE( tide_components )
      !       
      ! Ensure that tidal components have been set in namelist_cfg
      IF( ntsltde_harmo == 0 )   CALL ctl_stop( 'tsl_tde_init : No tidal components set in namtsl_tde' )
      !
      ALLOCATE( amp_pot(jpi,jpj,ntsltde_harmo), phi_pot(jpi,jpj,ntsltde_harmo), tsltde_pot(jpi,jpj) )
      !
      ! Disable diagnostic output by default
      ll_diatide = .FALSE.
      !
   END SUBROUTINE tsl_tde_init


   SUBROUTINE tsl_tde_init_osc( pcnames, sdtide_osc, sdtide_const )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tsl_tde_init_osc  ***
      !!
      !! Returns pointers to two structure arrays of type 'tide' and
      !! 'tsltde_harmonic' that contain information about the selected tidal
      !! constituents and their current oscillation parameters, respectively
      !! ----------------------------------------------------------------------
      CHARACTER(LEN=4),                   DIMENSION(jptsltde_max), INTENT(in   ) ::   pcnames        ! Names of selected components
#if ! defined key_agrif
      TYPE(tsltde_harmonic), ALLOCATABLE, DIMENSION(:),            INTENT(  out) ::   sdtide_osc     ! Oscillation parameters
      TYPE(tide), TARGET,    ALLOCATABLE, DIMENSION(:), OPTIONAL,  INTENT(  out) ::   sdtide_const   ! Selected components
#else
      TYPE(tsltde_harmonic), POINTER,     DIMENSION(:),            INTENT(  out) ::   sdtide_osc     ! Oscillation parameters
      TYPE(tide),            POINTER,     DIMENSION(:), OPTIONAL,  INTENT(  out) ::   sdtide_const   ! Selected components
#endif
      !
      TYPE(tide), ALLOCATABLE, DIMENSION(:) ::   tide_components     ! All available constituents
      TYPE(tide), POINTER,     DIMENSION(:) ::   slcsel              ! Selected constituents
      INTEGER,    ALLOCATABLE, DIMENSION(:) ::   icomppos            ! Indices of selected components
      INTEGER                               ::   icomp, jk, jj, ji   ! Miscellaneous integers
      INTEGER                               ::   istat               ! Memory-allocation status
      LOGICAL                               ::   llmatch             ! Local variables used for
      INTEGER                               ::   ic1, ic2            !    string comparison
      
      ! Populate local array with information about all available tidal
      ! constituents
      !
      ! Note, here 'tide_components' locally overrides the global module
      ! variable of the same name to enable the use of the global name in the
      ! include file that contains the initialisation of elements of array
      ! 'tide_components'
      ALLOCATE( tide_components(jptsltde_max), icomppos(jptsltde_max), STAT=istat )
      IF( istat /= 0 ) CALL ctl_stop( 'tsl_tde_init_osc: memory allocation failed' )
      ! Include tidal component parameters for all available components
      IF( nn_tsltde_var < 1 ) THEN
#define TSLTDE_VAR_0
#include "tsltde_constituents.h90"
#undef TSLTDE_VAR_0
      ELSE
#include "tsltde_constituents.h90"
      END IF
      ! Initialise array of indices of the selected componenents
      icomppos(:) = 0
      ! Identify the selected components that are availble
      icomp = 0
      DO jk = 1, jptsltde_max
         IF( TRIM( pcnames(jk) ) /= '' .AND. TRIM( pcnames(jk) ) /= 'n/a') THEN
            DO jj = 1, jptsltde_max
               ! Find matches between selected and available constituents
               ! (ignore capitalisation unless legacy variant has been selected)
               IF (nn_tsltde_var < 1) THEN
                  llmatch = ( TRIM( pcnames(jk) ) == TRIM( tide_components(jj)%cname_tide ) )
               ELSE
                  llmatch = .TRUE.
                  ji = MAX( LEN_TRIM( pcnames(jk) ), LEN_TRIM( tide_components(jj)%cname_tide ) )
                  DO WHILE (llmatch.AND.(ji > 0))
                     ic1 = IACHAR( pcnames(jk)(ji:ji) )
                     IF( ( ic1 >= 97 ) .AND. ( ic1 <= 122 ) ) ic1 = ic1 - 32
                     ic2 = IACHAR( tide_components(jj)%cname_tide(ji:ji) )
                     IF( ( ic2 >= 97 ) .AND. ( ic2 <= 122 ) ) ic2 = ic2 - 32
                     llmatch = (ic1 == ic2)
                     ji = ji - 1
                  END DO
               END IF
               IF( llmatch ) THEN
                  ! Count and record the match
                  icomp = icomp + 1
                  icomppos(icomp) = jj
                  ! Set the capitalisation of the tidal constituent identifier
                  ! as specified in the namelist
                  tide_components(jj)%cname_tide = pcnames(jk)
                  IF (lwp) WRITE(numout, '(10X,"Tidal component #",I2.2,36X,"= ",A4)') icomp, tide_components(jj)%cname_tide
                  EXIT
               END IF
            END DO
            IF( lwp .AND. ( jj > jptsltde_max ) ) WRITE(numout, '(10X,"Tidal component ",A4," is not available!")') pcnames(jk)
         END IF
      END DO
      
      ! Allocate and populate the reduced list of constituents and the
      ! associated list of oscillation parameters (if requested, the
      ! tide-constituent information is stored in a returned structure,
      ! otherwise kept locally)
      IF( PRESENT( sdtide_const ) ) THEN
         ALLOCATE( sdtide_const(icomp), sdtide_osc(icomp), STAT=istat )
         slcsel => sdtide_const
      ELSE
         ALLOCATE( slcsel(icomp), sdtide_osc(icomp), STAT=istat )
      END IF
      IF( istat /= 0 ) CALL ctl_stop( 'tsl_tde_init_osc: memory allocation failed' )
      DO jk = 1, icomp
         slcsel(jk) = tide_components(icomppos(jk))
         sdtide_osc(jk)%cname_tide = tide_components(icomppos(jk))%cname_tide
         sdtide_osc(jk)%equitide   = tide_components(icomppos(jk))%equitide
      END DO
      CALL tsl_tde_osc( slcsel, sdtide_osc )   ! Initialise oscillation parameters
      
      ! Deallocate local arrays
      IF( PRESENT( sdtide_const ) ) THEN
         DEALLOCATE( tide_components, icomppos, STAT=istat )
      ELSE
         DEALLOCATE( tide_components, icomppos, slcsel, STAT=istat )
      END IF
      IF( istat /= 0 ) CALL ctl_stop( 'tsl_tde_init_osc: memory deallocation failed' )

   END SUBROUTINE tsl_tde_init_osc


   SUBROUTINE tide_init_potential
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_potential  ***
      !!
      !! ** Purpose : Compute the tidal potential at midnight GMT
      !!
      !! ** Reference :
      !!      CT71) Cartwright, D. E. and Tayler, R. J. (1971): New computations of
      !!            the Tide-generating Potential. Geophys. J. R. astr. Soc. 23,
      !!            pp. 45-74. DOI: 10.1111/j.1365-246X.1971.tb01803.x
      !!
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jtde              ! Loop indices
      LOGICAL  ::   ll_ext                    ! External-contribution flag
      REAL(wp) ::   zcons, zlat, zlon, zarg   ! Local scalars
      REAL(wp) ::   zcs,   zcos, zsin, zamp   ! Local scalars
      !!----------------------------------------------------------------------
      !
      ll_ext = ALLOCATED( ampphi_pot_ext )
      !
      ! External contribution to the tidal potential (if any, e.g., from a SAL parameterisation); an external contribution will
      ! result in a potential even if the astronomical potential has not been activated (ln_tsltde_pot=F)
      IF( ll_ext ) THEN
         DO jtde = 1, ntsltde_harmo
            amp_pot(:,:,jtde) = stsltde_harmonics(jtde)%f * ampphi_pot_ext(:,:,1,jtde)
            phi_pot(:,:,jtde) = ampphi_pot_ext(:,:,2,jtde) + stsltde_harmonics(jtde)%v0 + stsltde_harmonics(jtde)%u
         END DO
      END IF
      !
      ! Compute the tidal potential associated with individual tidal constituents (with or without external contribution)
      IF( ln_tsltde_pot ) THEN
         DO jtde = 1, ntsltde_harmo
            zcons = rn_tsltde_gamma * tide_components(jtde)%equitide * stsltde_harmonics(jtde)%f
            DO ji = 1, jpi
               DO jj = 1, jpj
                  zlat = gphit(ji,jj) * rad   ! latitude en radian
                  zlon = glamt(ji,jj) * rad   ! longitude en radian
                  zarg = stsltde_harmonics(jtde)%v0 + stsltde_harmonics(jtde)%u + tide_components(jtde)%nt * zlon
                  ! le potentiel est composé des effets des astres:
                  SELECT CASE( tide_components(jtde)%nt )
                  CASE( 0 )                                               !   long-periodic constituents (included unless compat.
                     zcs = zcons * ( 0.5_wp - 1.5_wp * SIN( zlat )**2 )   !   with the original formulation is requested);
                     IF ( nn_tsltde_var < 1 ) zcs = 0.0_wp
                  CASE( 1 )                                               !   diurnal constituents;
                     zcs = zcons * SIN( 2.0_wp * zlat )
                  CASE( 2 )                                               !   semi-diurnal constituents;
                     zcs = zcons * COS( zlat )**2
                  CASE( 3 )                                               !   terdiurnal constituents, the colatitude-dependent
                     zcs = zcons * COS( zlat )**3                         !   factor is sin(theta)^3 (Table 2 of CT71);
                  CASE DEFAULT                                            !   constituents of higher frequency are not included
                     zcs = 0.0_wp
                  END SELECT
                  zcos =           zcs * COS( zarg )
                  zsin = -1.0_wp * zcs * SIN( zarg )
                  IF( ll_ext ) THEN   ! With external potential contribution
                     zcos = zcos + amp_pot(ji,jj,jtde) * COS( phi_pot(ji,jj,jtde) )
                     zsin = zsin - amp_pot(ji,jj,jtde) * SIN( phi_pot(ji,jj,jtde) )
                  END IF
                  zamp = SQRT( zcos * zcos + zsin * zsin )
                  amp_pot(ji,jj,jtde) = zamp
                  phi_pot(ji,jj,jtde) = ATAN2( -1.0_wp * zsin / MAX( 1.e-10_wp, zamp ), zcos / MAX( 1.e-10_wp, zamp ) )
               END DO
            END DO
         END DO
      END IF
      !
   END SUBROUTINE tide_init_potential

   SUBROUTINE tsl_tde_init_pot_ext( kcomp, pampphi_pot )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_pot_ext  ***
      !!
      !! ** Purpose : initialisation of an external contribution to the tidal
      !!              potential pampphi_pot (map of amplitude and phase pairs)
      !!              at midnight GMT for the specified tidal constituent kcomp
      !!
      !!----------------------------------------------------------------------
      INTEGER,                        INTENT(in   ) ::   kcomp         ! Tidal component index
      REAL(wp), DIMENSION(jpi,jpj,2), INTENT(in   ) ::   pampphi_pot   ! Tidal amplitudes and phases
      !
      INTEGER ::   istat   ! Status flag
      !!----------------------------------------------------------------------
      !
      IF( .NOT. ALLOCATED( ampphi_pot_ext ) ) THEN
         ALLOCATE( ampphi_pot_ext(jpi,jpj,2,ntsltde_harmo), STAT=istat )
         IF( istat .NE. 0 ) CALL ctl_stop( 'tsl_tde_init_pot_ext: memory allocation failed' )
         ampphi_pot_ext(:,:,:,:) = 0.0_wp
      END IF
      !
      ampphi_pot_ext(:,:,:,kcomp) = pampphi_pot(:,:,:)
      !
   END SUBROUTINE tsl_tde_init_pot_ext

   SUBROUTINE tsl_tde_stp( kt )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE tsl_tde_stp  ***
      !!
      !! ** Purpose : tidal-constituent updates at time step kt
      !!
      !!----------------------------------------------------------------------      
      INTEGER, INTENT(in   ) ::   kt   ! Time-step index
      INTEGER                ::   jk   ! Loop index
      !!----------------------------------------------------------------------
      !
      ! At the first time step
      IF( kt == nit000 ) THEN
         ! Update the diagnostic-output flag and, if required, allocate a temporary array for diagnostic output
         IF ( iom_use( 'tide_pot' ) )   ll_diatide = .TRUE.
         DO jk = 1, ntsltde_harmo
            IF( iom_use( 'tide_pot_' // TRIM( stsltde_harmonics(jk)%cname_tide ) ) )   ll_diatide = .TRUE.
         END DO
         IF( ll_diatide )   ALLOCATE( pot_astro_comp(jpi,jpj,ntsltde_harmo) )
      END IF
      !
      ! At the start of a new day (midnight GMT)
      IF( nsec_day == ndt05 .OR. kt == nit000 ) THEN
         !
         CALL tsl_tde_osc( tide_components, stsltde_harmonics )   ! Start-of-day update of tidal-constituent oscillation parameters
         !
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'tsl_tde_stp : update of the tidal constituents and potential at kt=', kt
            WRITE(numout,*) '~~~~~~~~~~~ '
            DO jk = 1, ntsltde_harmo
               WRITE(numout,*) stsltde_harmonics(jk)%cname_tide, stsltde_harmonics(jk)%u, &
                  &            stsltde_harmonics(jk)%f,stsltde_harmonics(jk)%v0, stsltde_harmonics(jk)%omega
            END DO
         ENDIF
         !
         CALL tide_init_potential   ! Initialise the tide potential, either for the selected tidal constituents (ln_tsltde_pot = T),
         !                          !    from external contributions (added via procedure tide_init_pot_ext), or both
         !
      ENDIF
      !
   END SUBROUTINE tsl_tde_stp


   SUBROUTINE tsl_tde_osc( sdtide_comp, sdtide_harmo )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE tsl_tde_osc  ***
      !!                      
      !! ** Purpose : Update the tidal-constituent oscillation parameters for
      !!              the current model day
      !!
      !! ** Reference :
      !!       S58) Schureman, P. (1958): Manual of Harmonic Analysis and
      !!            Prediction of Tides (Revised (1940) Edition (Reprinted 1958
      !!            with corrections). Reprinted June 2001). U.S. Department of
      !!            Commerce, Coast and Geodetic Survey Special Publication
      !!            No. 98. Washington DC, United States Government Printing
      !!            Office. 317 pp. DOI: 10.25607/OBP-155.
      !!----------------------------------------------------------------------
      TYPE(tide),            DIMENSION(:), INTENT(in   ) ::   sdtide_comp    ! Array of selected tidal component parameters
      TYPE(tsltde_harmonic), DIMENSION(:), INTENT(inout) ::   sdtide_harmo   ! Osc. parameters of selected tidal components
      !
      INTEGER  ::   jtde                                              ! Loop index
      REAL(wp) ::   zt                                                ! Time from 1 Jan 1900, 00h (hours)
      REAL(wp) ::   zsh_T,  zsh_s,    zsh_h,      zsh_p,     zsh_p1   ! Astronomic angles
      REAL(wp) ::   zsh_xi, zsh_nu,   zsh_nuprim, zsh_nusec, zsh_R    ! Astronomic angles
      REAL(wp) ::   zsh_I,  zsh_x1ra, zsh_N                           ! Astronomic angles
      REAL(wp) ::   zp,     zq,       zt2,        zs2,       ztgI2    ! Temporary scalars
      REAL(wp) ::   zP1,    ztgn2,    zat1,       zat2,      zscale   ! Temporary scalars
      INTEGER  ::   iny,    ind,      inl                             ! Temporary scalars
      !
      ! Named constants
      !
      ! Longitudes on 1 January 1900 at 00h GMT (values extracted from a subroutine of the NEMO-4.0 version of this module)
      REAL(wp), PARAMETER ::   pplon00_N  = 259.1560564_wp      ! Longitude of ascending lunar node
      REAL(wp), PARAMETER ::   pplon00_T  = 180.0_wp            ! Mean solar angle
      REAL(wp), PARAMETER ::   pplon00_h  = 280.1895014_wp      ! Mean solar longitude
      REAL(wp), PARAMETER ::   pplon00_s  = 277.0256206_wp      ! Mean lunar longitude
      REAL(wp), PARAMETER ::   pplon00_p1 = 281.2208569_wp      ! Longitude of solar perigee
      REAL(wp), PARAMETER ::   pplon00_p  = 334.3837214_wp      ! Longitude of lunar perigee
      ! Angular velocities (values extracted from a subroutine of the NEMO-4.0 version of this module)
      REAL(wp), PARAMETER ::   ppomega_N  =  -0.0022064139_wp   ! Longitude of ascending lunar node
      REAL(wp), PARAMETER ::   ppomega_T  =  15.0_wp            ! Mean solar angle
      REAL(wp), PARAMETER ::   ppomega_h  =   0.0410686387_wp   ! Mean solar longitude
      REAL(wp), PARAMETER ::   ppomega_s  =   0.549016532_wp    ! Mean lunar longitude
      REAL(wp), PARAMETER ::   ppomega_p1 =   0.000001961_wp    ! Longitude of solar perigee
      REAL(wp), PARAMETER ::   ppomega_p  =   0.004641834_wp    ! Longitude of lunar perigee
      ! cos(i)*cos(epsilon) and sin(i)*sin(epsilon), where i is the inclination of the orbit of the Moon w.r.t. the ecliptic and
      ! epsilon the obliquity of the ecliptic on 1 January 1900, 00h GMT (values extracted from a subroutine of the NEMO-4.0 version of
      ! this module)
      REAL(wp), PARAMETER ::   ppcice     =   0.913694997_wp    ! cos(i)*cos(epsilon)
      REAL(wp), PARAMETER ::   ppsise     =   0.035692561_wp    ! sin(i)*cos(epsilon)
      ! Coefficients used to compute sh_xi and sh_nu according to two equations given in the explanation of Table 6 of S58
      REAL(wp), PARAMETER ::   ppxinu1    = COS( 0.5_wp * ( ABS( ACOS( ppcice + ppsise ) ) ) ) /   &
         &                                  COS( 0.5_wp * ( ACOS( ppcice - ppsise ) ) )
      REAL(wp), PARAMETER ::   ppxinu2    = SIN( 0.5_wp * ( ABS( ACOS( ppcice + ppsise ) ) ) ) /   &
         &                                  SIN( 0.5_wp * ( ACOS( ppcice - ppsise ) ) )
      ! Number of days in previous months of the same year (excl. leap day)
      INTEGER, DIMENSION(12), PARAMETER ::   jpmd = (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)
      !!----------------------------------------------------------------------
      !
      ! Time offset from 1 Jan 1900, 00h in hours (Julian calendar)
      iny = nyear - 1900                                        ! Number of years since 1 Jan 1900, 00h
      ind = jpmd(nmonth) + nday                                 ! Current day of the year excluding leap day (if any)
      inl = INT( REAL( nyear - 1901, KIND=wp ) / 4.0_wp )       ! Number of leap days since 1 Jan 1900, 00h in past years
      IF( MOD( iny, 4 ) == 0 .AND. nmonth > 2 ) inl = inl + 1   ! Leap-day-number adjustment for current year
      zt = REAL( iny * 365 + ind + inl - 1, KIND=wp ) * rjjhh   ! Julian-date offset from 1 Jan 1900, 00h in hours
      !
      ! Longitudes in radians
      zsh_N  = MOD( ( pplon00_N  + ppomega_N  * zt     ) * rad, 2.0_wp * rpi )   ! Longitude of ascending lunar node
      zsh_T  =        pplon00_T                          * rad                   ! Mean solar angle (Greenwhich time)
      zsh_h  = MOD( ( pplon00_h  + ppomega_h  * zt     ) * rad, 2.0_wp * rpi )   ! Mean solar Longitude
      zsh_s  = MOD( ( pplon00_s  + ppomega_s  * zt     ) * rad, 2.0_wp * rpi )   ! Mean lunar Longitude
      zsh_p1 = MOD( ( pplon00_p1 + ppomega_p1 * zt     ) * rad, 2.0_wp * rpi )   ! Longitude of solar perigee
      zsh_p  = MOD( ( pplon00_p  + ppomega_p  * zt     ) * rad, 2.0_wp * rpi )   ! Longitude of lunar perigee
      !
      ! Inclination of the orbit of the moon w.r.t. the celestial equator, see explanation of Table 6 of S58
      zsh_I = ACOS( ppcice - ppsise * COS( zsh_N ) )
      !
      ! Computation of sh_xi and sh_nu, see explanation of Table 6 of S58
      ztgn2 = TAN( zsh_N / 2.0_wp )
      zat1 = ATAN( ppxinu1 * ztgn2 )
      zat2 = ATAN( ppxinu2 * ztgn2 )
      zsh_xi = zsh_N - zat1 - zat2
      IF( zsh_N > rpi ) zsh_xi = zsh_xi - 2.0_wp * rpi
      zsh_nu = zat1 - zat2
      !
      ! Computation of sh_x1ra, sh_R, sh_nuprim, and sh_nusec used for tidal constituents L2, K1, and K2
      ! Computation of sh_x1ra and sh_R (Equations 204, 213, and 214 of S58)
      ztgI2 = tan( zsh_I / 2.0_wp )
      zP1 = zsh_p - zsh_xi
      zt2 = ztgI2 * ztgI2
      zsh_x1ra = SQRT( 1.0 - 12.0 * zt2 * COS( 2.0_wp * zP1 ) + 36.0_wp * zt2 * zt2 )
      zp = SIN( 2.0_wp * zP1 )
      zq = 1.0_wp / ( 6.0_wp * zt2 ) - COS( 2.0_wp * zP1 )
      zsh_R = ATAN( zp / zq )
      ! Computation of sh_nuprim (Equation 224 of S58)
      zp = SIN( 2.0_wp * zsh_I ) * SIN( zsh_nu )
      zq = SIN( 2.0_wp * zsh_I ) * COS( zsh_nu ) + 0.3347_wp
      zsh_nuprim = ATAN( zp / zq )
      ! Computation of sh_nusec  (Equation 232 of S58)
      zs2 = SIN( zsh_I ) * SIN( zsh_I )
      zp = zs2 * SIN( 2.0_wp * zsh_nu )
      zq = zs2 * COS( 2.0_wp * zsh_nu ) + 0.0727_wp
      zsh_nusec = 0.5_wp * ATAN( zp / zq )
      !
      ! Update of the oscillation parameters omega, v0, u, and f
      zscale =  rad / 3600.0_wp
      DO jtde = 1, SIZE( sdtide_harmo )
         ! Tidal angular frequency omega
         sdtide_harmo(jtde)%omega = ( ppomega_T  * sdtide_comp(jtde)%nT + &
            &                         ppomega_s  * sdtide_comp(jtde)%ns + &
            &                         ppomega_h  * sdtide_comp(jtde)%nh + &
            &                         ppomega_p  * sdtide_comp(jtde)%np + &
            &                         ppomega_p1 * sdtide_comp(jtde)%np1 ) * zscale
         ! Phase of the tidal potential relative to the Greenwhich meridian (e.g., the position of the fictuous celestial body) v0
         ! in units of radians
         sdtide_harmo(jtde)%v0 = zsh_T  * sdtide_comp(jtde)%nT  + &
            &                    zsh_s  * sdtide_comp(jtde)%ns  + &
            &                    zsh_h  * sdtide_comp(jtde)%nh  + &
            &                    zsh_p  * sdtide_comp(jtde)%np  + &
            &                    zsh_p1 * sdtide_comp(jtde)%np1 + &
            &                             sdtide_comp(jtde)%shift * rad
         ! Phase correction u due to nodal motion in units of radians
         sdtide_harmo(jtde)%u = zsh_xi     * sdtide_comp(jtde)%nksi + &
            &                   zsh_nu     * sdtide_comp(jtde)%nnu0 + &
            &                   zsh_nuprim * sdtide_comp(jtde)%nnu1 + &
            &                   zsh_nusec  * sdtide_comp(jtde)%nnu2 + &
            &                   zsh_R      * sdtide_comp(jtde)%R
         ! Nodal correction factor f
         CALL nodal_factort( sdtide_comp(jtde)%nformula, sdtide_harmo(jtde)%f )
      END DO

   CONTAINS

      ! Recursive function for the computation of the nodal correction factor
      RECURSIVE SUBROUTINE nodal_factort( kformula, pf )
         !!----------------------------------------------------------------------
         !!                  ***  ROUTINE nodal_factort  ***
         !!
         !! ** Purpose : Compute amplitude correction factors due to nodal motion
         !!----------------------------------------------------------------------
         INTEGER,  INTENT(in   ) ::   kformula   ! Formula identifier
         REAL(wp), INTENT(  out) ::   pf         ! Scaling factor
         !
         REAL(wp)         ::   zs, zf1, zf2   ! Temporary scalar
         CHARACTER(LEN=3) ::   clformula      ! Temporary string
         !!----------------------------------------------------------------------
         !
         SELECT CASE( kformula )
         !
         CASE( 0 )                  ! Formula 0, solar waves
            pf = 1.0
            !
         CASE( 1 )                  ! Formula 1, compound waves (78 x 78)
            CALL nodal_factort( 78, pf )
            pf = pf * pf
            !
         CASE ( 4 )                 ! Formula 4,  compound waves (78 x 235) 
            CALL nodal_factort( 78, zf1 )
            CALL nodal_factort( 235, pf )
            pf  = zf1 * pf
            !
         CASE( 18 )                 ! Formula 18,  compound waves (78 x 78 x 78 )
            CALL nodal_factort( 78, zf1 )
            pf  = zf1 * zf1 * zf1
            !
         CASE( 20 )                 ! Formula 20, compound waves ( 78 x 78 x 78 x 78 )
            CALL nodal_factort( 78, zf1 )
            pf  = zf1 * zf1 * zf1 * zf1
            !
         CASE( 73 )                 ! Formula 73 of S58
            zs = SIN( zsh_I )
            pf = ( 2.0_wp / 3.0_wp - zs * zs ) / 0.5021_wp
            !
         CASE( 74 )                 ! Formula 74 of S58
            zs = SIN( zsh_I )
            pf = zs * zs / 0.1578_wp
            !
         CASE( 75 )                 ! Formula 75 of S58
            zs = COS( zsh_I / 2.0_wp )
            pf = SIN( zsh_I ) * zs * zs / 0.3800_wp
            !
         CASE( 76 )                 ! Formula 76 of S58
            pf = SIN( 2.0_wp * zsh_I ) / 0.7214_wp
            !
         CASE( 78 )                 ! Formula 78 of S58
            zs = COS( zsh_I / 2.0_wp )
            pf = zs * zs * zs * zs / 0.9154_wp
            !
         CASE( 149 )                ! Formula 149 of S58
            zs = COS( zsh_I / 2.0_wp )
            pf = zs * zs * zs * zs * zs * zs / 0.8758_wp
            !
         CASE( 215 )                ! Formula 215 of S58 with typo correction (0.9154 instead of 0.9145)
            zs = COS( zsh_I / 2.0_wp )
            pf = zs * zs * zs * zs / 0.9154_wp * zsh_x1ra
            !
         CASE( 227 )                ! Formula 227 of S58
            zs = SIN( 2.0_wp * zsh_I )
            pf = SQRT( 0.8965_wp * zs * zs + 0.6001_wp * zs * COS( zsh_nu ) + 0.1006_wp )
            !
         CASE ( 235 )               ! Formula 235 of S58
            zs = SIN( zsh_I )
            pf = SQRT( 19.0444_wp * zs * zs * zs * zs + 2.7702_wp * zs * zs * cos( 2.0_wp * zsh_nu ) + 0.0981_wp )
            !
         CASE DEFAULT
            WRITE( clformula, '(I3)' ) kformula
            CALL ctl_stop('nodal_factort: formula ' // clformula // ' is not available')
         END SELECT
         !
      END SUBROUTINE nodal_factort

   END SUBROUTINE tsl_tde_osc


   SUBROUTINE tsl_tde_pot_upd( kt, pdelta, Kmm, psal_scalar )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tsl_tde_pot_upd  ***
      !!
      !! ** Purpose :   update of the astronomical tide-generating potential
      !!                pdelta seconds after the start of the current time step
      !!
      !! ** Method  :   computation of the potential from tidal-constituent
      !!                amplitude and phase maps evaluated at midnight (GMT)
      !!
      !! ** Action  : - accumulation of the potential contribution from each
      !!                tidal constituent, evaluated at pdelta seconds after the
      !!                start of the current time step
      !!              - if requested, attenuation of the potential during the
      !!                initial phase of the model run
      !!              - if requested, diagnostic output of the tide-generating
      !!                potential
      !!
      !!----------------------------------------------------------------------      
      INTEGER,            INTENT(in   ) ::   kt            ! Time-step index
      REAL(wp),           INTENT(in   ) ::   pdelta        ! Temporal offset from the start of the current time step in seconds
      INTEGER,            INTENT(in   ) ::   Kmm           ! Time-level index
      REAL(wp), OPTIONAL, INTENT(in   ) ::   psal_scalar   ! Coefficient for scalar SAL approximation
      !
      INTEGER                            ::   jtde                 ! Dummy loop index
      REAL(wp)                           ::   zt, zramp, zscalar   ! Local scalars
      REAL(wp), DIMENSION(ntsltde_harmo) ::   zwt                  ! Temporary array
      !!----------------------------------------------------------------------      
      !
      ! Update of the tide-generating potential (sum of all harmonics) at pdelta seconds after the start of the current time step
      zwt(:) = stsltde_harmonics(:)%omega * ( REAL( nsec_day - ndt05, KIND=wp ) + pdelta )
      tsltde_pot(:,:) = 0.0_wp
      DO jtde = 1, ntsltde_harmo
         IF ( .NOT. ll_diatide ) THEN
            tsltde_pot(:,:) = tsltde_pot(:,:) + amp_pot(:,:,jtde) * COS( zwt(jtde) + phi_pot(:,:,jtde) )
         ELSE   ! Store the potential contribution from each tidal constituent for diagnostic output
            pot_astro_comp(:,:,jtde) = amp_pot(:,:,jtde) * COS( zwt(jtde) + phi_pot(:,:,jtde) )
            tsltde_pot(:,:) = tsltde_pot(:,:) + pot_astro_comp(:,:,jtde)
         END IF
      END DO
      !
      ! Attenuation during the initial phase of the model run (if requested)
      IF( ln_tsltde_ramp ) THEN
         zt = REAL( kt - nit000, KIND=wp ) * rn_Dt + pdelta
         zramp = MIN( MAX( zt / ( rn_tsltde_ramp_dt * rday ) , 0.0_wp ) , 1.0_wp )
         tsltde_pot(:,:) = zramp * tsltde_pot(:,:)
         IF( ll_diatide ) pot_astro_comp(:,:,:) = zramp * pot_astro_comp(:,:,:)
      END IF
      !
      ! Output the tide-generating potential (incl. load potential)
      IF( ll_diatide ) THEN
         zscalar = 0.0_wp
         IF( PRESENT( psal_scalar ) ) zscalar = psal_scalar
         IF( iom_use( "tide_pot" ) ) CALL iom_put( "tide_pot", tsltde_pot(:,:) + zscalar * ssh(:,:,Kmm) )
         DO jtde = 1, ntsltde_harmo   ! Output tide-generating potential for each tidal constituent (incl. load potential)
            IF ( iom_use( "tide_pot_" // TRIM( stsltde_harmonics(jtde)%cname_tide ) ) ) THEN
               CALL iom_put( "tide_pot_" // TRIM( stsltde_harmonics(jtde)%cname_tide ), pot_astro_comp(:,:,jtde) )
            END IF
         END DO
      END IF
      !
   END SUBROUTINE tsl_tde_pot_upd

   !!======================================================================
END MODULE tsltde
