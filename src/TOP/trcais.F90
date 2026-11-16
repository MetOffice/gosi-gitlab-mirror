MODULE trcais
   !!======================================================================
   !!                         ***  MODULE trcais  ***
   !!  Module for passive tracers in Antarctic ice sheet
   !!  delivered by iceberg and ice shelf freshwater fluxes
   !!======================================================================
   !! History :  1.0  ! 2020    (R. Person, O. Aumont, C. Ethe) Original code
   !!            1.1  ! 2026    (P. Mathiot, R. Person) fix compatibility with isf and icb melt
   !!======================================================================
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP model
   !!----------------------------------------------------------------------
   !!   trc_ais       : external source of tracers from Antarctic ice sheet
   !!----------------------------------------------------------------------
   USE par_trc        !  passive tracers parameters
   USE oce_trc        !  shared variables between ocean and passive tracers
   USE trc            !  passive tracers common variables
   USE iom            !  I/O manager
   USE lib_mpp        !  MPP library
   USE sbc_oce        !
   USE isf_oce        ! ice shelf melting contribution
   USE sbcrnf         ! iceberg freshwater flux
   USE trcnam         ! Namelist read
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ais         ! called in trcstp_rk3.F90 or within TOP modules
   PUBLIC   trc_ais_ini     ! called in trcini.F90

   INTEGER  , SAVE, PUBLIC                              :: nb_trcais    ! number of tracers in AIS
   REAL(wp) , SAVE, PUBLIC                              :: rn_icbdep    ! mean underwater depth of iceberg (in meters)
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:,:) :: nk_icb_ais   ! mean underwater depth of iceberg (in level depth)
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)   :: n_trc_indais ! index of tracer with AIS freswater flux
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)   :: rf_trafac    ! multiplicative factor for AIS tracer values

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ais_ini
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ais_ini ***
      !!
      !! ** Purpose :   Initialization of passive tracers from the Antartic
      !!                Ice Sheet delivered by iceberg and ice shelf
      !!                freshwater flux
      !!
      !! ** Method  : - Read namtsd namelist
      !!              
      !!---------------------------------------------------------------------
      INTEGER            :: ji, jj, jl, jn, jk                     ! dummy loop indices
      INTEGER            :: ierr0, ierr1, ierr2, ierr3     ! temporary integers
      INTEGER            :: ios                            ! Local integer output status for namelist read
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trafac    ! multiplicative factor for tracer values
      !!
      NAMELIST/namtrc_ais/ rn_trafac, rn_icbdep
      !!----------------------------------------------------------------------
      !
      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_ais_ini : Antarctic ice sheet tracer initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      ! 
      IF( .NOT. ln_rnf_icb ) THEN
         CALL ctl_stop( 'trc_ais_ini: no iceberg freswater flux in runoff file' )   ;  RETURN
      ENDIF
      !
      ! Compute the number of tracers to be initialised in iceberg and ice
      ! shelf freshwater flux
      ALLOCATE( n_trc_indais(jptra), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_ais_ini: unable to allocate n_trc_indais' )   ;  RETURN
      ENDIF
      nb_trcais       = 0
      n_trc_indais(:) = 0
      rn_trafac(:)    = 0._wp
      !
      ! Read Antarctic Ice Sheet Namelist
      READ_NML_REF(numnat,namtrc_ais)
      READ_NML_CFG(numnat,namtrc_ais)
      IF(lwm) WRITE ( numont, namtrc_ais )
      ! 
      IF( lwp ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) '   Namelist : namtrc_ais'
         WRITE(numout,*) '   ~~~~~~~~~~~~~~~~~~~~~'
      ENDIF
      ! compose AIS data indexes
      DO jn = 1, jptra
         IF( ln_trc_ais(jn) ) THEN
             nb_trcais       = nb_trcais + 1   ;   n_trc_indais(jn) = nb_trcais
         ENDIF
      END DO

      ! Print summmary of Antarctic Ice Sheet tracers supply
      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,'(a,i3)') '   Total tracers to be initialized with AIS freshwater fluxes:', nb_trcais
      ENDIF
      !
      IF( nb_trcais > 0 ) THEN
         ALLOCATE( rf_trafac(nb_trcais), STAT=ierr0 )
         DO jn = 1, jptra
            IF( ln_trc_ais(jn) ) THEN
                jl = n_trc_indais(jn)
                rf_trafac(jl) = rn_trafac(jn)
                IF(lwp) WRITE(numout, 9001) jn, ctrcnm(jn), 'AIS', rn_trafac(jn), rf_trafac(jl)
            ENDIF
         END DO
      ENDIF
9001  FORMAT(2x,i5, 8x, a15, 3x, a3, 5x, e11.3, 5x, e11.3)

      ! 
      !  compute last level where depth less than rn_icbdep (120 m)
      ALLOCATE(nk_icb_ais(A2D(1)), STAT=ierr0 )
      DO_2D( 0, 0, 0, 0 )
         nk_icb_ais(ji,jj) = 1
         DO jk = jpkm1, 1, -1
            IF( gdept_1d(jk) > rn_icbdep ) nk_icb_ais(ji,jj) = MAX(jk - 1,mbkt(ji,jj))
         END DO
      END_2D
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' Level corresponding to iceberg depth ',  MAXVAL(nk_icb_ais),' ', gdept_1d(MAXVAL(nk_icb_ais)+1)
      !
   END SUBROUTINE trc_ais_ini

   SUBROUTINE trc_ais(kt, Kmm, ptr, Krhs)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_ais  ***
      !!
      !! ** Purpose :  Apply Antarctic Ice Sheet inputs to tracers
      !!
      !! ** Method  :  Read freswater flux from iceberg and ice shelf 
      !!               and update data 
      !!                
      !!----------------------------------------------------------------------
      !!
      INTEGER                                   , INTENT(in)           ::   kt ! ocean time-step index
      INTEGER                                   , INTENT(in)           ::   Kmm, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout)        ::   ptr ! passive tracers and RHS of tracer equation
      !!
      INTEGER  :: ji, jj, jk, jn, jl             ! Loop index
      INTEGER  :: ikt, ikb  ! top and bottom level of the tbl
      CHARACTER (len=22) :: charout
      REAL(wp) :: zfact, zcalv, zfrac, ztra
      REAL(wp), DIMENSION(T2D(1)) :: zhicb_ais
      REAL(wp), DIMENSION(A2D(1)) :: zone         ! Dummy array to use generic subroutine of icb and isf
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_ais')

      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN ! Do only for the first tile
         IF( kt == nit000 .AND. lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'trc_ais : passive tracers from Antarctic Ice Sheet'
            WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      ! source of bgc tracers from iceberg in Southern Ocean
      ! distributed along the water column until 120 m depth (Person et al., 2019)
      IF( ln_rnf_icb )  THEN
          ! compute hicb
          DO_2D( 0, 0, 0, 0 )
             zhicb_ais(ji,jj) = gdepw(ji,jj,nk_icb_ais(ji,jj)+1,Kmm)
             zone(ji,jj)      = 1.0_wp
          END_2D
          !
          ! add sources to ptr between surface and nk_icb_ais
          ! PM: Why not using nk_rnf and h_rnf to be consistent ?
          CALL trc_ais_mlt( Kmm, Krhs, ptr, mikt, nk_icb_ais, zhicb_ais, zone, fwficb )
      END IF
      !
      ! source of bgc tracers from ice shelf in the Southern Ocean 
      ! with tbl treated as in Mathiot et al. (2017)
      IF( ln_isf ) THEN
         IF( ln_isfpar_mlt ) CALL trc_ais_mlt( Kmm, Krhs, ptr, misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, fwfisf_par )
         IF( ln_isfcav_mlt ) CALL trc_ais_mlt( Kmm, Krhs, ptr, misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, fwfisf_cav )
      END IF
      !
      IF( ln_timing )   CALL timing_stop('trc_ais')
   !
   ! for debugging
!      IF( sn_cfctl%l_prttrc ) THEN ! print mean trends (used for debugging)
!         WRITE(charout, FMT="('ais ')")
!         CALL prt_ctl_trc_info(charout)
!         CALL prt_ctl_trc( tab4d=ptr(:,:,:,:,Krhs), mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
!      ENDIF
      !
   END SUBROUTINE trc_ais

   SUBROUTINE trc_ais_mlt( Kmm, Krhs, ptr, ktop, kbot, phtbl, pfrac, pfwf )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ais_mlt  ***
      !!
      !! ** Purpose :   apply the tracer trend of one ISF meltwater scheme
      !!                over its top boundary layer
      !!
      !! ** Global variables : e3t, jptra, ln_trc_ais, r1_rho0, n_trc_indais
      !!
      !!----------------------------------------------------------------------
      INTEGER                                    , INTENT(in   ) ::   Kmm, Krhs   ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt) , INTENT(inout) ::   ptr         ! passive tracers and RHS
      INTEGER , DIMENSION(A2D(1))                , INTENT(in   ) ::   ktop, kbot  ! top and bottom level of the tbl
      REAL(wp), DIMENSION(A2D(1))                , INTENT(in   ) ::   phtbl       ! tbl thickness [m]
      REAL(wp), DIMENSION(A2D(1))                , INTENT(in   ) ::   pfrac       ! fraction of the deepest level in the tbl
      REAL(wp), DIMENSION(jpi,jpj)               , INTENT(in   ) ::   pfwf        ! meltwater flux [kg/m2/s] ( > 0 from isf to oce )
      !
      INTEGER  ::   ji, jj, jk, jn, jl, ikt, ikb    ! local integers
      REAL(wp) ::   zcalv                           ! local scalars
      !!----------------------------------------------------------------------
      !
      DO jn = 1, jptra
         IF( ln_trc_ais(jn) ) THEN
            jl  = n_trc_indais(jn)
            !
            DO_2D( 0, 0, 0, 0 )
               zcalv = pfwf(ji,jj) * r1_rho0 / phtbl(ji,jj)
               ikt = ktop(ji,jj)
               ikb = kbot(ji,jj)
               ! level fully include in the ice shelf boundary layer
               DO jk = ikt, ikb - 1
                  ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + rf_trafac(jl) * e3t(ji,jj,jk,Kmm) * zcalv
               END DO
               ! level partially include in the ice shelf boundary layer
               ptr(ji,jj,ikb,jn,Krhs) = ptr(ji,jj,ikb,jn,Krhs) + rf_trafac(jl) * e3t(ji,jj,ikb,Kmm) * zcalv * pfrac(ji,jj)
            END_2D
            !
         END IF
      END DO
      !
   END SUBROUTINE trc_ais_mlt

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_ais_ini   ! Empty routine
   END SUBROUTINE trc_ais_ini
   SUBROUTINE trc_ais( kt, Kmm, Krhs )        ! Empty routine
      INTEGER, INTENT(in) :: kt, Kmm, Krhs ! time level indices
      WRITE(*,*) 'trc_ais: You should not have seen this print! error?', kt, Kmm, Krhs
   END SUBROUTINE trc_ais
#endif

   !!======================================================================
END MODULE trcais
