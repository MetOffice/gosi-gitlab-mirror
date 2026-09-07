MODULE netcdferr
!
!
! Purpose
! =======
!
!    Generic netcdf error message and stop
!
  USE netcdf
  USE declaration

  IMPLICIT NONE
  PUBLIC

  CONTAINS

   SUBROUTINE stop_netcdf(kneterr,cmname,cmname2)

      INTEGER         ,           INTENT(in) :: kneterr
      CHARACTER(len=*),           INTENT(in) :: cmname
      CHARACTER(len=*), OPTIONAL, INTENT(in) :: cmname2

         WRITE (kout,*) TRIM(cmname)
         IF (PRESENT(cmname2)) WRITE (kout,*) TRIM(cmname)
         WRITE (kout,*) TRIM(NF90_STRERROR(kneterr))
         CALL FLUSH(kout)
         STOP

   END SUBROUTINE stop_netcdf
END MODULE

