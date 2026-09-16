MODULE icbctl
   !!======================================================================
   !!                       ***  MODULE  icbctl  ***
   !! Ocean run control :  gross check of the ocean time stepping
   !!======================================================================
   !! History :  OPA  ! 1991-03  (G. Madec) Original code
   !!            6.0  ! 1992-06  (M. Imbard)
   !!            8.0  ! 1997-06  (A.M. Treguier)
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            2.0  ! 2009-07  (G. Madec)  Add statistic for time-spliting
   !!            3.7  ! 2016-09  (G. Madec)  Remove solver
   !!            4.0  ! 2017-04  (G. Madec)  regroup global communications
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   icb_ctl      : Control the run
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain variables 
   !  
   USE icb_oce         ! iceberg output variables
   USE icbdia          ! Standard run outputs       (dia_wri_state routine)
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! distributed memory computing
   USE timing          ! timing
   !
   USE netcdf          ! NetCDF library
   USE, INTRINSIC :: ieee_arithmetic, ONLY : ieee_is_nan

   IMPLICIT NONE
   PRIVATE

   PUBLIC icb_ctl           ! routine called by icbrk3.F90

   INTEGER, PARAMETER         ::   jpvar = 2
   INTEGER                    ::   nrunid   ! netcdf file id
   INTEGER, DIMENSION(jpvar+1)::   nvarid   ! netcdf variable id
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   PURE FUNCTION checksum(pfield) RESULT(ichecksum)
      !!-----------------------------------------------------------------------
      !!                  ***  checksum_contrib  ***
      !!
      !! ** Purpose : Compute 16-bit checksum contribution for a real array
      !!
      !! ** Method  : Reinterpret bits as i8, keep lower 16 bits, sum
      !!
      !! ** Input   : field - real(wp) array to checksum
      !!-----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(IN) :: pfield
      INTEGER(8) :: ichecksum
      INTEGER(8), PARAMETER :: idp = 1_i8
      INTEGER(8), PARAMETER :: ibitmask = 2_i8**16 - 1_i8
      !
      ichecksum = SUM(IAND(TRANSFER(REAL(pfield, KIND=dp), (/idp,idp/) ), ibitmask))

   END FUNCTION checksum

   SUBROUTINE icb_ctl( kt )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE icb_ctl  ***
      !!
      !! ** Purpose :   Control the run
      !!
      !! ** Method  :   Save the icb output checksum in numicb_run
      !!
      !! ** Actions :   "icb_time.step" file = last icb time-step
      !!                "icb_run.stat"  file = icb run statistics
      !!                 nstop indicator sheared among all local domain
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt       ! ocean time-step index
      !!
      INTEGER, PARAMETER              ::   jptst = 2
      INTEGER                         ::   ji                                    ! dummy loop indices
      INTEGER                         ::   idtime, istatus
      REAL(wp), DIMENSION(jpvar+1)    ::   zmax
      REAL(wp)                        ::   znan                                  ! number of NaN in the output variables
      INTEGER(i8)                     ::   ilsb_sum                              ! Test value: cumulative sum of the two
                                                                                 ! least-significant bytes of tested values
      REAL(wp), DIMENSION(jptst)      ::   zmaxlocal
      LOGICAL                         ::   ll_wrtstp, ll_colruns, ll_wrtruns
      LOGICAL                         ::   ll_lsb_sum                            ! Flag to indicate test-value computation
      LOGICAL, DIMENSION(jpi,jpj,jpk) ::   llmsk
      CHARACTER(len=20)               ::   clname
      !!----------------------------------------------------------------------
      IF( nstop > 0 .AND. ngrdstop > -1 )   RETURN   !   icbctl was already called by a child grid
      !
      IF( ln_timing )   CALL timing_start( 'icb_ctl' )
      !
      ll_wrtstp  = ( MOD( kt-nit000, sn_cfctl%ptimincr ) == 0 ) .OR. ( kt == nitend )
      ll_colruns = sn_cfctl%l_runstat .AND. ll_wrtstp .AND. jpnij > 1
      ll_wrtruns = sn_cfctl%l_runstat .AND. ll_wrtstp .AND. lwm
      ll_lsb_sum = sn_cfctl%l_lsb_sum   ! Local flag to indicate the computation of the test value
      ilsb_sum   = 0                    ! Initialisation of the test value (cumulative sum)
      !
      IF( kt == nit000 ) THEN
         !
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'icb_ctl : time-stepping control'
            WRITE(numout,*) '~~~~~~~'
         ENDIF
         !
         IF( ll_wrtruns ) THEN
            !                             ! open run.stat     ascii file, done only by 1st subdomain
            CALL ctl_opn( numicb_run, 'icb_run.stat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
            !                             ! open run.stat.nc netcdf file, done only by 1st subdomain
            clname = 'icb_run.stat.nc'
            istatus = NF90_CREATE( TRIM(clname), NF90_CLOBBER, nrunid )
            istatus = NF90_DEF_DIM( nrunid, 'time', NF90_UNLIMITED, idtime )
            istatus = NF90_DEF_VAR( nrunid, 'berg_melt_max', NF90_DOUBLE,  (/ idtime /), nvarid(1) )
            istatus = NF90_DEF_VAR( nrunid, 'berg_hflx_max', NF90_DOUBLE,  (/ idtime /), nvarid(2) )
            istatus = NF90_DEF_VAR( nrunid, 'checksum'     , NF90_INT, (/ idtime /), nvarid(3) )
            istatus = NF90_ENDDEF(nrunid)
         ENDIF
         !
      ENDIF
      !
      !                                   !==     test of local extrema and test sum     ==!
      !                                   !==  done by all processes at every time step  ==!
      !
      llmsk(     1:nn_hls,:,:) = .FALSE.                                          ! exclude halos from the checked region
      llmsk(Nie0+1:   jpi,:,:) = .FALSE.
      llmsk(:,     1:nn_hls,:) = .FALSE.
      llmsk(:,Nje0+1:   jpj,:) = .FALSE.
      !
      llmsk(Nis0:Nie0,Njs0:Nje0,1) = ssmask(Nis0:Nie0,Njs0:Nje0) == 1._wp                 ! define only the inner domain
      zmax(1) = MAXVAL( ABS( berg_grid%floating_melt(:,:) ), mask = llmsk(:,:,1) )        ! berg melt max
      znan = MERGE(1._wp, 0._wp, ANY(ieee_is_nan(berg_grid%floating_melt(A2D(0)))))       ! berg melt nan
      ilsb_sum = ilsb_sum + checksum( berg_grid%floating_melt(A2D(0)) * ssmask(A2D(0) ) ) ! berg melt contribution
      !
      llmsk(Nis0:Nie0,Njs0:Nje0,1) = ssmask(Nis0:Nie0,Njs0:Nje0) == 1._wp                 ! define only the inner domain
      zmax(2) = MAXVAL( ABS( berg_grid%calving_hflx(:,:)  ), mask = llmsk(:,:,1) )        ! berg hflx max
      znan = znan + MERGE(1._wp,0._wp,ANY(ieee_is_nan(berg_grid%calving_hflx(A2D(0)))))   ! berg hflx nan
      ilsb_sum = ilsb_sum + checksum( berg_grid%calving_hflx(A2D(0))  * ssmask(A2D(0) ) ) ! berg hflx contribution
      !
      zmax(jpvar+1) = REAL( nstop, wp )                                           ! stop indicator
      !
      !                                   !==               get global extrema             ==!
      !                                   !==  done by all processes if writting run.stat  ==!
      IF( ll_colruns ) THEN
         CALL mpp_max( "icbctl", zmax )          ! max over the global domain: ok even if l0oce_[T,U,V] = .true. 
         CALL mpp_sum( "icbctl", ilsb_sum )      ! Finalise the cumulative test value
         CALL mpp_sum( "icbctl", znan )
         nstop = NINT( zmax(jpvar+1) )           ! update nstop indicator (now shared among all local domains)
      ENDIF
      !
      !                                   !==              write "run.stat" files              ==!
      !                                   !==  done only by 1st subdomain at writting timestep  ==!
      IF( ll_wrtruns ) THEN
         WRITE(numicb_run,9501) kt, zmax(1), zmax(2), ilsb_sum
         IF( jpnij == 1 ) CALL FLUSH(numicb_run)
         DO ji = 1, jpvar
            istatus = NF90_PUT_VAR( nrunid, nvarid(ji), (/zmax(ji)/), (/kt/), (/1/) )
         END DO
         istatus = NF90_PUT_VAR( nrunid, nvarid(jpvar+1), (/ilsb_sum/), (/kt/), (/1/) )
         IF( kt == nitend )   istatus = NF90_CLOSE(nrunid)
      END IF
      !                                   !==               error handling               ==!
      !                                   !==  done by all processes at every time step  ==!
      !
      IF(  (znan > 0.5_wp) .OR.   &       ! NaN encounter in the tests
         & ABS( SUM(zmax(1:jptst)) ) > HUGE(1._wp)  ) THEN    ! Infinity encounter in the tests
         !
         WRITE(ctmp1,*) ' stp_ctl: Infinite or NaN encounter in the tests'
         WRITE(ctmp7,*) '      ===> output of last computed fields in icb_output.abort* files'
         !
         CALL icb_dia_wri_state( 'output.abort' )     ! create an output.abort file
         !
         IF( ll_colruns .OR. jpnij == 1 ) THEN   ! all processes synchronized -> use lwp to print in opened ocean.output files
            IF( .NOT. ll_colruns .AND. lwm )   istatus = NF90_CLOSE(nrunid)
            IF(lwp) THEN   ;   CALL ctl_stop( ctmp1, ' ', ctmp7 )
            ELSE           ;   nstop = MAX(1, nstop)   ! make sure nstop > 0 (automatically done when calling ctl_stop)
            ENDIF
         ELSE                                    ! only mpi subdomains with errors are here -> STOP now
            IF( lwm )   istatus = NF90_CLOSE(nrunid)
            CALL ctl_stop( 'STOP', ctmp1, ' ', ctmp7 )
         ENDIF
         !
      ENDIF
      !
      IF( nstop > 0 ) THEN                                                  ! an error was detected and we did not abort yet...
         IF( .NOT. ll_colruns .AND. jpnij > 1 )   CALL ctl_stop( 'STOP' )   ! we must abort here to avoid MPI deadlock
      ENDIF
      !
9501  FORMAT(' it :', i8, '    |berg_melt|_max: ', D23.16, ' |berg_hflx|_max: ', D23.16, ' lsb sum: ', z17.16 )
      !
      IF( ln_timing )   CALL timing_stop( 'icb_ctl' )
      !
   END SUBROUTINE icb_ctl

   !!======================================================================
END MODULE icbctl
