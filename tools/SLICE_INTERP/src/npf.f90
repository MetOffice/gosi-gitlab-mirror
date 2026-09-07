MODULE npf
!
!
! Purpose
! =======
!
!    Mimic global grid boundaries update for cyclic or North Pole Folded NEMO grids
!
!    lbc_nfd_ext_byte and lbc_nfd_ext_double for the two types needed


  USE netcdf
  use declaration

  IMPLICIT NONE
  PUBLIC

  CONTAINS

   SUBROUTINE lbc_nfd_ext_byte( ptab, cd_nat, psgn, kextj, jpiglo, jpjglo, jpkglo, c_NFtype)
      !!----------------------------------------------------------------------
      INTEGER(kind=1), DIMENSION(:,:,:),INTENT(inout) ::   ptab
      CHARACTER(len=1), INTENT(in   ) ::   cd_nat      ! nature of array grid-points
      REAL(wp),  INTENT(in   ) ::   psgn        ! sign used across the north fold boundary
      INTEGER*4,          INTENT(in   ) ::   kextj       ! extra halo width at north fold
      INTEGER*4,          INTENT(in   ) ::   jpiglo       
      INTEGER*4,          INTENT(in   ) ::   jpjglo       
      INTEGER*4,          INTENT(in   ) ::   jpkglo       
      CHARACTER(len=1), INTENT(in   ) ::   c_NFtype      ! North fold pivot
      !
      INTEGER*4  ::    ji,  jj,  jk, jh   ! dummy loop indices
      INTEGER*4  ::   ipj
      INTEGER*4  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      !SELECT CASE ( jpni )
      !CASE ( 1 )     ;   ipj = jpj        ! 1 proc only  along the i-direction
      !CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      !END SELECT

      ipj = jpjglo - kextj
      !
      ipjm1 = ipj-1
      !
      IF( c_NFtype == 'T' ) THEN            ! *  North fold  T-point pivot
         !
         SELECT CASE ( cd_nat  )
         CASE ( 'T' , 'W' )                         ! T-, W-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj+jh,jk) = psgn * ptab(ijt,ipj-2-jh,jk)
               END DO
               ptab(1,ipj+jh,jk) = psgn * ptab(3,ipj-2-jh,jk)
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+2
               ptab(ji,ipjm1,jk) = psgn * ptab(ijt,ipjm1,jk)
            END DO
            END DO
         CASE ( 'U' )                               ! U-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj+jh,jk) = psgn * ptab(iju,ipj-2-jh,jk)
               END DO
               ptab(   1  ,ipj+jh,jk) = psgn * ptab(    2   ,ipj-2-jh,jk)
               ptab(jpiglo,ipj+jh,jk) = psgn * ptab(jpiglo-1,ipj-2-jh,jk) 
            END DO
            DO ji = jpiglo/2, jpiglo-1
               iju = jpiglo-ji+1
               ptab(ji,ipjm1,jk) = psgn * ptab(iju,ipjm1,jk)
            END DO
            END DO
         CASE ( 'V' )                               ! V-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj-1+jh,jk) = psgn * ptab(ijt,ipj-2-jh,jk)
                  ptab(ji,ipj+jh  ,jk) = psgn * ptab(ijt,ipj-3-jh,jk)
               END DO
               ptab(1,ipj+jh,jk) = psgn * ptab(3,ipj-3-jh,jk) 
            END DO
            END DO
         CASE ( 'F' )                               ! F-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj-1+jh,jk) = psgn * ptab(iju,ipj-2-jh,jk)
                  ptab(ji,ipj+jh  ,jk) = psgn * ptab(iju,ipj-3-jh,jk)
               END DO
            END DO
            DO jh = 0, kextj
               ptab(   1  ,ipj+jh,jk) = psgn * ptab(    2   ,ipj-3-jh,jk)
               ptab(jpiglo,ipj+jh,jk) = psgn * ptab(jpiglo-1,ipj-3-jh,jk)
            END DO
            END DO
         END SELECT
         !
      ENDIF   ! c_NFtype == 'T'
      !
      IF( c_NFtype == 'F' ) THEN            ! *  North fold  F-point pivot
         !
         SELECT CASE ( cd_nat  )
         CASE ( 'T' , 'W' )                         ! T-, W-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj+jh,jk) = psgn * ptab(ijt,ipj-1-jh,jk)
               END DO
            END DO
            END DO
         CASE ( 'U' )                               ! U-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj+jh,jk) = psgn * ptab(iju,ipj-1-jh,jk)
               END DO
               ptab(jpiglo,ipj+jh,jk) = psgn * ptab(jpiglo-2,ipj-1-jh,jk)
            END DO
            END DO
         CASE ( 'V' )                               ! V-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj+jh,jk) = psgn * ptab(ijt,ipj-2-jh,jk)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+1
               ptab(ji,ipjm1,jk) = psgn * ptab(ijt,ipjm1,jk)
            END DO
            END DO
         CASE ( 'F' )                               ! F-point
            DO jk = 1, jpkglo
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj+jh ,jk ) = psgn * ptab(iju,ipj-2-jh,jk)
               END DO
               ptab(jpiglo,ipj+jh,jk) = psgn * ptab(jpiglo-2,ipj-2-jh,jk)
            END DO
            DO ji = jpiglo/2+1, jpiglo-1
               iju = jpiglo-ji
               ptab(ji,ipjm1,jk) = psgn * ptab(iju,ipjm1,jk)
            END DO
            END DO
         END SELECT
         !
      ENDIF   ! c_NFtype == 'F'
      !
   END SUBROUTINE lbc_nfd_ext_byte


   SUBROUTINE lbc_nfd_ext_double( ptab, cd_nat, psgn, kextj, jpiglo, jpjglo, c_NFtype)
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:),INTENT(inout) ::   ptab
      CHARACTER(len=1), INTENT(in   ) ::   cd_nat      ! nature of array grid-points
      REAL(wp),  INTENT(in   ) ::   psgn        ! sign used across the north fold boundary
      INTEGER*4,          INTENT(in   ) ::   kextj       ! extra halo width at north fold
      INTEGER*4,          INTENT(in   ) ::   jpiglo       
      INTEGER*4,          INTENT(in   ) ::   jpjglo       
      CHARACTER(len=1), INTENT(in   ) ::   c_NFtype      ! North fold pivot
      !
      INTEGER*4  ::    ji,  jj, jh   ! dummy loop indices
      INTEGER*4  ::   ipj
      INTEGER*4  ::   ijt, iju, ipjm1
      !!----------------------------------------------------------------------
      !
      !SELECT CASE ( jpni )
      !CASE ( 1 )     ;   ipj = jpj        ! 1 proc only  along the i-direction
      !CASE DEFAULT   ;   ipj = 4          ! several proc along the i-direction
      !END SELECT

      ! Global array
      ipj = jpjglo - kextj
      !
      ipjm1 = ipj-1
      !
      IF( c_NFtype == 'T' ) THEN            ! *  North fold  T-point pivot
         !
         SELECT CASE ( cd_nat  )
         CASE ( 'T' , 'W' )                         ! T-, W-point
            DO jh = 0, kextj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj+jh) = psgn * ptab(ijt,ipj-2-jh)
               END DO
               ptab(1,ipj+jh) = psgn * ptab(3,ipj-2-jh)
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+2
               ptab(ji,ipjm1) = psgn * ptab(ijt,ipjm1)
            END DO
         CASE ( 'U' )                               ! U-point
            DO jh = 0, kextj
               DO ji = 2, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj+jh) = psgn * ptab(iju,ipj-2-jh)
               END DO
               ptab(   1  ,ipj+jh) = psgn * ptab(    2   ,ipj-2-jh)
               ptab(jpiglo,ipj+jh) = psgn * ptab(jpiglo-1,ipj-2-jh) 
            END DO
            DO ji = jpiglo/2, jpiglo-1
               iju = jpiglo-ji+1
               ptab(ji,ipjm1) = psgn * ptab(iju,ipjm1)
            END DO
         CASE ( 'V' )                               ! V-point
            DO jh = 0, kextj
               DO ji = 2, jpiglo
                  ijt = jpiglo-ji+2
                  ptab(ji,ipj-1+jh) = psgn * ptab(ijt,ipj-2-jh)
                  ptab(ji,ipj+jh  ) = psgn * ptab(ijt,ipj-3-jh)
               END DO
               ptab(1,ipj+jh) = psgn * ptab(3,ipj-3-jh) 
            END DO
         CASE ( 'F' )                               ! F-point
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji+1
                  ptab(ji,ipj-1+jh) = psgn * ptab(iju,ipj-2-jh)
                  ptab(ji,ipj+jh  ) = psgn * ptab(iju,ipj-3-jh)
               END DO
            END DO
            DO jh = 0, kextj
               ptab(   1  ,ipj+jh) = psgn * ptab(    2   ,ipj-3-jh)
               ptab(jpiglo,ipj+jh) = psgn * ptab(jpiglo-1,ipj-3-jh)
            END DO
         END SELECT
         !
      ENDIF   ! c_NFtype == 'T'
      !
      IF( c_NFtype == 'F' ) THEN            ! *  North fold  F-point pivot
         !
         SELECT CASE ( cd_nat  )
         CASE ( 'T' , 'W' )                         ! T-, W-point
            DO jh = 0, kextj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj+jh) = psgn * ptab(ijt,ipj-1-jh)
               END DO
            END DO
         CASE ( 'U' )                               ! U-point
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj+jh) = psgn * ptab(iju,ipj-1-jh)
               END DO
               ptab(jpiglo,ipj+jh) = psgn * ptab(jpiglo-2,ipj-1-jh)
            END DO
         CASE ( 'V' )                               ! V-point
            DO jh = 0, kextj
               DO ji = 1, jpiglo
                  ijt = jpiglo-ji+1
                  ptab(ji,ipj+jh) = psgn * ptab(ijt,ipj-2-jh)
               END DO
            END DO
            DO ji = jpiglo/2+1, jpiglo
               ijt = jpiglo-ji+1
               ptab(ji,ipjm1) = psgn * ptab(ijt,ipjm1)
            END DO
         CASE ( 'F' )                               ! F-point
            DO jh = 0, kextj
               DO ji = 1, jpiglo-1
                  iju = jpiglo-ji
                  ptab(ji,ipj+jh  ) = psgn * ptab(iju,ipj-2-jh)
               END DO
               ptab(jpiglo,ipj+jh) = psgn * ptab(jpiglo-2,ipj-2-jh)
            END DO
            DO ji = jpiglo/2+1, jpiglo-1
               iju = jpiglo-ji
               ptab(ji,ipjm1) = psgn * ptab(iju,ipjm1)
            END DO
         END SELECT
         !
      ENDIF   ! c_NFtype == 'F'
      !
   END SUBROUTINE lbc_nfd_ext_double
END MODULE

