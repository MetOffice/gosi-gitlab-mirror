PROGRAM slice_interp
!
!
! Purpose
! =======
!
!    Compute and save in OASIS file format mapping weight and address
!    for slice interpolation (pseudo 3D)
!
!
! Further Details
! ===============
!
!   Usage :
!
!           ./slice_interp 
!
!   Input namelist : namelist_cfg
!
!-----------------------------------------------------------------------
!&nam_def    !   Definition of transformations 
!-----------------------------------------------------------------------
!!
!  Source grid (parent)
!     - must be have the lowest resolution 
!
!    sn_msk_fname_s   ! source meshmask file name
!
!  Target grid (child)
!     - must be have the highest resolution
!
!    sn_msk_fname_t   ! target meshmask file name
!
!  Zoom (T) or same grid transform (F) 
!     - in the first case, the target grid have a smaller extend than the parent
!     - in the second case, source and target grid have the same extend 
!
!    ln_zoom        default  = .FALSE.
!
!  Lower-left indexes of lower-left child grid point on parent grid
!   - e.g. AGRIF_FixedGrids.in parameter
!
!    nn_lli         default  = 1
!    nn_llj         default  = 1
!
!  Factor of coarsening or Zoom
!   - can be = 1
!
!    nn_factor      default  = 0
!
!  U,V grid transformation for transport (T) or current (F)
!   - if transport, coarsening only involves the corresponding "nn_factor" side grid points
!
!   ln_transport    default  = .true.
!
!  For 3D variable coupling, number of coupled levels
!
!    nn_lvlcut      default  = -1    ! whole column coupling
!
! ================================================================================================
!
use declaration
use read_grid
use checkdim
use process_masks
use process_weights
!
!
! STRONG TYPING IMPOSED
! =====================
    implicit none
!
!
! SPECIFICATIONS FOR VARIABLES
! ============================
!
    NAMELIST/nam_def/ nn_factor, nn_lli, nn_llj, sn_msk_fname_s, sn_msk_fname_t, ln_transport, ln_zoom, nn_lvlcut
    INTEGER :: jg
!
! EXECUTABLE STATEMENTS
! =====================
!
    ! ================================ !
    !      Namelist informations       !
    ! ================================ !
 
    OPEN( UNIT=numnam_ref, FILE='namelist_cfg', STATUS='OLD', FORM='FORMATTED', ACCESS='SEQUENTIAL' , IOSTAT=iost )

    IF( iost /= 0 ) THEN
       WRITE(kout,*) ' ===>>>> : bad opening file namelist_cfg '
       WRITE(kout,*) '           we stop. verify the file '
       STOP
    ENDIF

    ! Default values
    nn_factor    =   0
    ln_zoom      =   .FALSE.
    nn_lli       =   1
    nn_llj       =   1
    ln_transport =   .TRUE.
    nn_lvlcut    =   -1

    REWIND( numnam_ref ) 
    READ  ( numnam_ref, nam_def, IOSTAT = iost )
!
    IF( iost /= 0 ) THEN
       WRITE(kout,*) ' ===>>>> : not able to read nam_def '
       WRITE(kout,*) '           we stop. verify the namelist '
       STOP
    ENDIF

    write (kout,*) ' Source Grid '
    write (kout,*) ' ----------- '
    write (kout,*) ' '
    write (kout,*) ' configuration file name ', TRIM(sn_msk_fname_s)
    write (kout,*) ' '
    write (kout,*) ' Target Grid '
    write (kout,*) ' ----------- '
    write (kout,*) ' '
    write (kout,*) ' configuration file name ', TRIM(sn_msk_fname_t)
    write (kout,*) ' '
    write (kout,*) ' Transformation parameters '
    write (kout,*) ' ----------- '
    write (kout,*) ' '
    write (kout,*) ' Zoom (T) or whole grid coarsening (F) : ', ln_zoom
    write (kout,*) ' '
    IF ( ln_zoom ) THEN
       write (kout,*) ' Lower-left indexes of child : ', nn_lli, nn_llj
       write (kout,*) ' '
       write (kout,*) ' Zoom factor : ', nn_factor
       write (kout,*) ' '
       write (kout,*) ' LBC filling : ', ln_transport
       write (kout,*) ' '
    ELSE
       write (kout,*) ' Coarsening factor : ', nn_factor
       write (kout,*) ' '
       write (kout,*) ' U,V grid transformation for transport (T) or current (F) : ', ln_transport
       write (kout,*) ' '
    ENDIF
    IF ( nn_lvlcut >= 1 ) THEN
       write (kout,*) ' Vertical levels coupling limited to : ', nn_lvlcut
       write (kout,*) ' '
    ELSE
       write (kout,*) ' Vertical levels coupling for all meshmask levels '
       write (kout,*) ' '
    ENDIF

    ! Read source and target grid info
    call read_grid_info

    call check_dimensions

    call allocate_arrays

    ! Loop over Arakawa mesh vertices
    DO jg = 1, 5

       ! Read mask and areas
       call get_masks(jg)

       ! Allocate local arrays with appropriate dimensions
       call dimension_weights(jg)

       call compute_weights(jg)

       call save_weights(jg)

    ENDDO

    call deallocate_arrays

    write (kout,*) ' '
    write (kout,*) ' Slice interpolation W&A computed and saved successfully '
!
! END OF PROGRAM slice_interp
! =============================
!
end program slice_interp
