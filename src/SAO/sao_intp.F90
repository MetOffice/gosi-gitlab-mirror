MODULE sao_intp
   !!======================================================================
   !!                    ***  MODULE sao_intp ***
   !! ** Purpose : Run NEMO observation operator in offline mode
   !!======================================================================
   !! History :  3.6  ! 2015-12  (A. Ryan)  Original code
   !!----------------------------------------------------------------------
   !        ! NEMO modules
   USE in_out_manager
   USE dom_oce, ONLY : narea
   USE diaobs
   !        ! Stand Alone Observation operator modules
   USE sao_read
   USE sao_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC sao_interp

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sao_interp( Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE sao_interp ***
      !!
      !! ** Purpose : To interpolate the model as if it were running online.
      !!
      !! ** Method : 1. Populate model counterparts
      !!             2. Call dia_obs at appropriate time steps
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kmm     ! Time-level index
      INTEGER             ::   istp    ! Time-step index
      INTEGER             ::   ifile   ! File index
      !!----------------------------------------------------------------------
      istp = nit000 - 1
      ifile = 1
      CALL sao_rea_dri( Kmm, ifile )
      CALL mpp_max( 'sao_interp', nstop )   ! Error check across all processes
      !
      ! Open 'time.step' file
      IF( lwm ) CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
      !
      DO WHILE ( istp <= nitend .AND. nstop == 0 )
         IF (ifile <= n_files + 1) THEN
            IF ( MOD( istp, nn_sao_freq ) == MOD( nit000, nn_sao_freq ) ) THEN
               CALL sao_rea_dri( Kmm, ifile )
               ifile = ifile + 1
            ENDIF
            CALL dia_obs( istp, Kmm )
         ENDIF
         !
         IF( lwm ) THEN
            WRITE( numstp, '(1x,i8)' ) istp   ! Update 'time.step' counter
            REWIND( numstp )
         END IF
         !
         CALL mpp_max( 'sao_interp', nstop )   ! Error check across all processes
         istp = istp + 1
      END DO
      !
   END SUBROUTINE sao_interp

   !!======================================================================
END MODULE sao_intp
