PROGRAM nemo
   !!======================================================================
   !!                     ***  PROGRAM sab  ***
   !!
   !! ** Purpose :   encapsulate sab_gcm so that it can also be called
   !!              together with the linear tangent and adjoint models
   !!======================================================================
   !! History : to be written later on  
   !!----------------------------------------------------------------------
   USE sabgcm   ! SAB system   (sab_gcm routine)
   !!----------------------------------------------------------------------
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
   !
   CALL sab_gcm           ! SAB direct code
   ! 
   !!======================================================================
END PROGRAM nemo
