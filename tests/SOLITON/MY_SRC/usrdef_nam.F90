MODULE usrdef_nam
   !!======================================================================
   !!                       ***  MODULE  usrdef_nam  ***
   !!
   !!                      ===  SOLITON configuration  ===
   !!
   !! User defined : set the domain characteristics of a user configuration
   !!======================================================================
   !! History :  NEMO ! 2026-06  (J. Chanut)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_nam   : read user defined namelist and set global domain size
   !!   usr_def_hgr   : initialize the horizontal mesh 
   !!----------------------------------------------------------------------
   USE dom_oce
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE timing         ! Timing
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_nam   ! called by nemogcm.F90

   !                              !!* namusr_def namelist *!!
   REAL(wp), PUBLIC ::   rn_dx      ! resolution in meters defining the horizontal domain size
   REAL(wp), PUBLIC ::   rn_dy      ! resolution in meters defining the horizontal domain size

   !! * Substitutions
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usr_def_nam( cd_cfg, kk_cfg, kpi, kpj, kpk, ldIperio, ldJperio, ldNFold, cdNFtype )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_nam  ***
      !!                    
      !! ** Purpose :   read user defined namelist and define the domain size
      !!
      !! ** Method  :   read in namusr_def containing all the user specific namelist parameter
      !!
      !!                Here SOLITON configuration
      !!
      !! ** input   : - namusr_def namelist found in namelist_cfg
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(out) ::   cd_cfg               ! configuration name
      INTEGER         , INTENT(out) ::   kk_cfg               ! configuration resolution
      INTEGER         , INTENT(out) ::   kpi, kpj, kpk        ! global domain sizes
      LOGICAL         , INTENT(out) ::   ldIperio, ldJperio   ! i- and j- periodicity
      LOGICAL         , INTENT(out) ::   ldNFold              ! North pole folding
      CHARACTER(len=1), INTENT(out) ::   cdNFtype             ! Folding type: T or F
      !
      INTEGER ::   ios          ! Local integer
      INTEGER :: ighost_n, ighost_s, ighost_w, ighost_e
      !!
      NAMELIST/namusr_def/  rn_dx, rn_dy
      !!----------------------------------------------------------------------
      !
      READ_NML_(numnam_cfg,cfg,namusr_def,.TRUE.)
      !
      !
#if defined key_agrif 
      ! Domain parameters are taken from parent:
      IF( .NOT. Agrif_Root() ) THEN
         rn_dx = Agrif_Parent(rn_dx)/Agrif_Rhox()
         rn_dy = Agrif_Parent(rn_dy)/Agrif_Rhoy()
      ENDIF
#endif
      !
      IF(lwm)   WRITE( numond, namusr_def )
      !
      cd_cfg = 'SOLITON'            ! name & resolution (not used)
      kk_cfg = nINT( rn_dx )
      !
#if defined key_agrif 
      IF( Agrif_Root() ) THEN  
#endif
!! E-W cyclic setup        kpi = NINT( 48  / rn_dx )
         kpi = NINT( 24  / rn_dx )
         kpj = NINT( 16  / rn_dy ) + 2 
#if defined key_agrif 
      ELSE                          ! Global Domain size: add nbghostcells + 1 "land" point on each side
         ! At this stage, child ghosts have not been set
         ighost_w = nbghostcells
         ighost_e = nbghostcells
         ighost_s = nbghostcells
         ighost_n = nbghostcells

         ! In case one sets zoom boundaries over domain edges: 
         IF  ( Agrif_Ix() == 2 - Agrif_Parent(nbghostcells_x_w) ) ighost_w = 1
         IF  ( Agrif_Ix() + nbcellsx/AGRIF_Irhox() == Agrif_Parent(Ni0glo) - Agrif_Parent(nbghostcells_x_w) ) ighost_e = 1 
         IF  ( Agrif_Iy() == 2 - Agrif_Parent(nbghostcells_y_s) ) ighost_s = 1
         IF  ( Agrif_Iy() + nbcellsy/AGRIF_Irhoy() == Agrif_Parent(Nj0glo) - Agrif_Parent(nbghostcells_y_s) ) ighost_n = 1 
!         kpi  = nbcellsx + 2 * ( nbghostcells + 1 )
!         kpj  = nbcellsy + 2 * ( nbghostcells + 1 )
         kpi  = nbcellsx + ighost_w + ighost_e 
         kpj  = nbcellsy + ighost_s + ighost_n
      ENDIF
#endif
      kpk = 1
      !
      !                             ! Set the lateral boundary condition of the global domain
      ldIperio = .FALSE.   ;   ldJperio = .FALSE. 
      ldNFold  = .FALSE.   ;   cdNFtype = '-'
      !
      !                             ! control print
      IF(lwp) THEN
         WRITE(numout,*) '   '
         WRITE(numout,*) 'usr_def_nam  : read the user defined namelist (namusr_def) in namelist_cfg'
         WRITE(numout,*) '~~~~~~~~~~~ '
         WRITE(numout,*) '   Namelist namusr_def : SOLITON test case'
         WRITE(numout,*) '      horizontal resolution             rn_dx  = ', rn_dx, ' ad'
         WRITE(numout,*) '      horizontal resolution             rn_dy  = ', rn_dy, ' ad'
         WRITE(numout,*) '   '
      ENDIF
      !
   END SUBROUTINE usr_def_nam

   !!======================================================================
END MODULE usrdef_nam
