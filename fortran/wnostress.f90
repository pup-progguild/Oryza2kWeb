!----------------------------------------------------------------------!
!  SUBROUTINE WNOSTRESS                                                !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date  : December 2001                                               !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Invoked when production environment is POTENTIAL.          !
!           Sets actual transpiration of a crop at zero, and sets      !
!           effects of water stress on growth and development of rice  !
!           at unity.                                                  !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! NL      I4  Number of soil layers (-)                             I  !
! TRW     R4  Actual transpiration rate (mm d-1)                    O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for accelerating leaf death (-)         O  !
! LESTRS  R4  Stress factor for reducing expansion of leaves (-)    O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WNOSTRESS (NL, TRW, TRWL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)

      IMPLICIT NONE
!-----Formal parameters
      INTEGER NL
      REAL    TRW, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW
      REAL    TRWL(NL)
!-----Local variables
      INTEGER I
      SAVE

      TRW    = 0.
      LRSTRS = 1.
      LDSTRS = 1.
      LESTRS = 1.
      CPEW   = 1.
      PCEW   = 1.
      DO I=1,NL
        TRWL(I) = 0.
      END DO

      RETURN
      END

