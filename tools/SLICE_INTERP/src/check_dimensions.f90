MODULE checkdim
!
! Purpose
! =======
!
   ! Check that :
   !  - Source grid resolution lower than target
   !  - Target must have the exact required number of grid point regarding zoom factor
   !  - in both horizontal dimensions

   ! Remarks:
   ! - Independant of geographical position (this will appear during mask conformity check)
   ! - Made incredibly complex by periodic conditions (PC) and - suprise - North Pole Folding (NPF)

   !  Algorithm:
   !    - calculate the actual number of grid points per dimensions taking into account PC and NPF
   !    - check zero remainder of the euclidean division of this number by zoom factor
!

  USE netcdf
  use declaration

  IMPLICIT NONE
  PUBLIC

  CONTAINS
!
   SUBROUTINE check_dimensions
   !

   INTEGER :: id, isg, adim_s, adim_t, isouth, tmp_y, itmp

   ! Zoom factor must be provided
   IF ( nn_factor == 0 ) THEN
      write (kout,*) ' Zoom factor must be provided in nn_factor namelist parameter '
      STOP
   ENDIF

   ! loop over dimensions
   DO id = 1, 2

      ! Actual dimension
      adim_s = gsource%ndim(id)
      adim_t = gtarget%ndim(id)

      ! First: Simple check that Source grid resolution lower than target
      ! if no periodicity,
      IF ( ( id == 1 .AND. .NOT. gsource%iperio ) .OR. &
           ( id == 2 .AND. .NOT. gsource%jperio ) ) THEN
         ! if zoom, reduce the number of actual source grid points with zoom starting position
         IF ( ln_zoom ) THEN
            adim_s = adim_s - illid(id) + 1
         ELSE
            ! reduce the number of actual source grid points nb by three ( lateral mask 1 + 2 )
            adim_s = adim_s - 3
            adim_t = adim_t - 3
         ENDIF
      ENDIF

      ! Target grid points nb > ( source grid points * zoom factor ) 
      IF ( adim_s * nn_factor < adim_t ) THEN
          write (kout,*) ' Target grid must have higher or same resolution than source'
          write (kout,*) ' and target area must fit in source'
          STOP
      ENDIF

      ! if zoom, no additional check possible
      ! to be modified when ICB Arctic zoom
      IF ( ln_zoom ) CYCLE

      ! Additional dimension check
      IF ( id == 1 .OR. gsource%pivot == '-' ) THEN

         ! Exact match of dimensions required
         IF ( adim_s*nn_factor /= adim_t .OR. INT(adim_t/nn_factor)*nn_factor /= adim_t ) THEN
            write (kout,*) ' Coarsening factor and grids X dimensions incompatible '
            STOP
         ENDIF

      ! Additional check for Y dimension and NPF
      ELSE
 
         ! Cancel check if identity transform
         IF ( nn_factor == 1 ) CYCLE
 
         IF ( gtarget%pivot == 'T' ) THEN

            IF ( INT(nn_factor/2)*2 == nn_factor ) THEN
               write (kout,*) ' With target grid T-pivot, coarsening factor even value is not allowed '
               STOP
            ENDIF

            ! From the target input grid # of lines, 
            !  - we substract 1 line (masked South) + 
            isouth = illid(2) - 1
            !  - we duplicate as many half line as necessary to build the pole line
            !  - we divide this total by the CRSF
            !  - we add 1 line (masked South)
            tmp_y = (adim_t-isouth + (nn_factor-1) / 2 )/nn_factor + isouth

            itmp = (tmp_y-isouth)*nn_factor - (nn_factor-1)/2 + isouth

            IF ( itmp /= adim_t .OR. tmp_y /= gsource%ndim(2) ) THEN
               write (kout,*) ' Coarsening factor and grids Y dimensions incompatible '
               STOP
            ENDIF

            IF ( gsource%pivot /= 'T' ) THEN
               write (kout,*) ' If fine grid has a T-pivot, coarse grid must have a T-pivot '
               STOP
            ENDIF

         ELSEIF ( gtarget%pivot == 'F' ) THEN

            ! Even coarsening factor case
            IF ( INT(nn_factor/2)*2 == nn_factor ) THEN

               IF ( gsource%pivot /= 'T' ) THEN
                  write (kout,*) ' If fine grid has F-pivot and even coarsening factor,'
                  write (kout,*) ' coarse grid must have a T-pivot '
                  STOP
               ENDIF

               ! From the target input grid # of lines, 
               !  - we substract 1 line (masked South)
               isouth = illid(2) - 1
               !  - we duplicate as many half line as necessary to build the pole line
               !  - we divide this total by the CRSF
               !  - we add 1 line (masked South)
               tmp_y = (adim_t-isouth + nn_factor/2 )/nn_factor + isouth

               itmp = (tmp_y-isouth)*nn_factor - nn_factor/2 + isouth

               IF ( itmp /= adim_t .OR. tmp_y /= gsource%ndim(2) ) THEN
                  write (kout,*) ' Coarsening factor and grids Y dimensions incompatible '
                  STOP
               ENDIF

            ! Odd coarsening factor
            ELSE

               IF ( gsource%pivot /= 'F' ) THEN
                  write (kout,*) ' If fine grid has F-pivot and odd coarsening factor,'
                  write (kout,*) ' coarse grid must have too '
                  STOP
               ENDIF

               ! From the target input grid # of lines, 
               !  - we substract 1 line (masked South)
               isouth = illid(2) - 1
               !  - we duplicate as many half line as necessary to build the pole line
               !  - we divide this total by the CRSF
               !  - we add 1 line (masked South)
               tmp_y = (adim_t-isouth)/nn_factor + isouth

               itmp = (tmp_y-isouth)*nn_factor + isouth

               IF ( itmp /= adim_t .OR. tmp_y /= gsource%ndim(2) ) THEN
                  write (kout,*) ' Coarsening factor and grids Y dimensions incompatible '
                  STOP
               ENDIF

            ENDIF

         ENDIF

      ENDIF


   ENDDO

   ! Shorter synonyms
   xs = gsource%ndim(1); ys = gsource%ndim(2)
   xt = gtarget%ndim(1); yt = gtarget%ndim(2)
   crsf = nn_factor
!

   END SUBROUTINE check_dimensions

END MODULE
