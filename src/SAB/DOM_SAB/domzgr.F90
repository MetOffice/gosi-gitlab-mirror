MODULE domzgr
   !!==============================================================================
   !!                       ***  MODULE domzgr   ***
   !! Ocean domain : definition of the vertical coordinate system
   !!==============================================================================
   !! History :  OPA  ! 1995-12  (G. Madec)  Original code : s vertical coordinate
   !!                 ! 1997-07  (G. Madec)  lbc_lnk call
   !!                 ! 1997-04  (J.-O. Beismann) 
   !!            8.5  ! 2002-09  (A. Bozec, G. Madec)  F90: Free form and module
   !!             -   ! 2002-09  (A. de Miranda)  rigid-lid + islands
   !!  NEMO      1.0  ! 2003-08  (G. Madec)  F90: Free form and module
   !!             -   ! 2005-10  (A. Beckmann)  modifications for hybrid s-ccordinates & new stretching function
   !!            2.0  ! 2006-04  (R. Benshila, G. Madec)  add zgr_zco
   !!            3.0  ! 2008-06  (G. Madec)  insertion of domzgr_zps.h90 & conding style
   !!            3.2  ! 2009-07  (R. Benshila) Suppression of rigid-lid option
   !!            3.3  ! 2010-11  (G. Madec) add mbk. arrays associated to the deepest ocean level
   !!            3.4  ! 2012-08  (J. Siddorn) added Siddorn and Furner stretching function
   !!            3.4  ! 2012-12  (R. Bourdalle-Badie and G. Reffray)  modify C1D case  
   !!            3.6  ! 2014-11  (P. Mathiot and C. Harris) add ice shelf capabilitye  
   !!            3.?  ! 2015-11  (H. Liu) Modifications for Wetting/Drying
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_zgr       : read or set the ocean vertical coordinate system
   !!   zgr_top_bot   : ocean top and bottom level for t-, u, and v-points with 1 as minimum value
   !!---------------------------------------------------------------------
   USE dom_oce        ! ocean domain
   USE depth_e3       ! depth <=> e3
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! distributed memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_zgr        ! called by dom_init.F90

  !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS       

   SUBROUTINE dom_zgr( k_top, k_bot )
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE dom_zgr  ***
      !!                   
      !! ** Purpose :   set the depth of model levels and the resulting 
      !!              vertical scale factors.
      !!
      !! ** Method  : - reference 1D vertical coordinate (gdep._1d, e3._1d)
      !!              - read/set ocean depth and ocean levels (bathy, mbathy)
      !!              - vertical coordinate (gdep., e3.) depending on the 
      !!                coordinate chosen :
      !!                   l_zco=T   z-coordinate   
      !!                   l_zps=T   z-coordinate with partial steps
      !!                   l_zco=T   s-coordinate 
      !!
      !! ** Action  :   define gdep., e3., mbathy and bathy
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(:,:), INTENT(out) ::   k_top, k_bot   ! ocean first and last level indices
      !
      INTEGER  ::   ji,jj,jk            ! dummy loop index
      INTEGER  ::   ikt, ikb            ! top/bot index
      INTEGER  ::   ioptio, inum, iatt  ! local integer
      INTEGER  ::   is_mbkuvf           ! ==0 if mbku, mbkv, mbkf to be computed
      !REAL(wp) ::   zrefdep             ! depth of the reference level (~10m)
      REAL(WP) ::   z_zco, z_zps, z_sco, z_cav
      CHARACTER(len=7) ::   catt        ! 'zco', 'zps, 'sco' or 'UNKNOWN'
      REAL(wp), DIMENSION(jpi,jpj)   ::   zmsk, z2d
      REAL(wp), DIMENSION(jpi,jpj,2) ::   ztopbot
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dom_zgr : vertical coordinate'
         WRITE(numout,*) '~~~~~~~'
      ENDIF
      !                             !==============================!
      IF( ln_read_cfg ) THEN        !==  read in domcfg.nc file  ==!
         !                          !==============================!
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   ==>>>   Read vertical mesh in ', TRIM( cn_domcfg ), ' file'
         is_mbkuvf = 0         
         !
         CALL iom_open( cn_domcfg, inum )    ! open domcfg file
         !
         CALL iom_get( inum, jpdom_unknown, 'e3t_1d'  , e3t_1d  )   ! 1D reference coordinate
         CALL iom_get( inum, jpdom_unknown, 'e3w_1d'  , e3w_1d  )
         CALL e3_to_depth( e3t_1d, e3w_1d, gdept_1d, gdepw_1d )     ! 1D reference depth deduced from e3
         !                                   !- ocean top and bottom k-indices
         CALL iom_get( inum, jpdom_global, 'top_level'    , z2d(:,:)   )   ! 1st wet T-points (ISF)
         k_top(:,:) = NINT( z2d(:,:) )
         CALL iom_get( inum, jpdom_global, 'bottom_level' , z2d(:,:)   )   ! last wet T-points
         k_bot(:,:) = NINT( z2d(:,:) )
             !
         CALL iom_get( inum, jpdom_global, 'e3t_0'  ,  e3t_3d(:,:,:), cd_type = 'T', psgn = 1._wp, kfill = jpfillcopy )
         CALL lbc_lnk( 'dom_zgr', e3t_3d, 'T', 1._wp, kfillmode = jpfillcopy,ldfull = .TRUE.)
         !
         CALL iom_close( inum )        ! close domcfg file
         !
         ztopbot(:,:,1) = REAL(k_top, wp)
         ztopbot(:,:,2) = REAL(k_bot, wp)
         CALL lbc_lnk( 'dom_zgr', ztopbot, 'T', 1._wp, kfillmode = jpfillcopy )   ! do not put 0 over closed boundaries
         k_top(:,:) = NINT(ztopbot(:,:,1))
         k_bot(:,:) = NINT(ztopbot(:,:,2))
         !
      ENDIF
      !
      ! the following is mandatory
      ! make sure that closed boundaries are correctly defined in k_top that will be used to compute all mask arrays
      !
      zmsk(:,:) = 1._wp                                       ! default: no closed boundaries
      IF( .NOT. l_Iperio ) THEN                                    ! E-W closed:
         zmsk(  mi0(     1+nn_hls,nn_hls):mi1(     1+nn_hls,nn_hls),:) = 0._wp   ! first column of inner global domain at 0
         zmsk(  mi0(jpiglo-nn_hls,nn_hls):mi1(jpiglo-nn_hls,nn_hls),:) = 0._wp   ! last  column of inner global domain at 0 
      ENDIF
      IF( .NOT. l_Jperio ) THEN                                    ! S closed:
         zmsk(:,mj0(     1+nn_hls,nn_hls):mj1(     1+nn_hls,nn_hls)  ) = 0._wp   ! first   line of inner global domain at 0
      ENDIF
      IF( .NOT. ( l_Jperio .OR. l_NFold ) ) THEN                   ! N closed:
         zmsk(:,mj0(jpjglo-nn_hls,nn_hls):mj1(jpjglo-nn_hls,nn_hls)  ) = 0._wp   ! last    line of inner global domain at 0
      ENDIF
      !CALL lbc_lnk( 'usrdef_zgr', zmsk, 'T', 1._wp )             ! set halos
      k_top(:,:) = k_top(:,:) * NINT( zmsk(:,:) )
      !
      ioptio = 0                       ! Check Vertical coordinate options
      !
      mbkt(:,:) = MAX( k_bot(:,:) , 1 )    ! bottom ocean k-index of T-level (=1 over land)
      !
      IF( lwp )   THEN
         WRITE(numout,*) ' MIN val k_top   ', MINVAL(   k_top(:,:) ), ' MAX ', MAXVAL( k_top(:,:) )
         WRITE(numout,*) ' MIN val k_bot   ', MINVAL(   k_bot(:,:) ), ' MAX ', MAXVAL( k_bot(:,:) )
         WRITE(numout,*) ' MIN val e3t     ', MINVAL(   e3t_3d(:,:,:) ), ' MAX ', MAXVAL(e3t_3d(:,:,:))
      !
      ENDIF
      !
   END SUBROUTINE dom_zgr

   !!======================================================================
END MODULE domzgr
