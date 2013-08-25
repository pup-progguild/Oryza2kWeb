!----------------------------------------------------------------------!
!  SUBROUTINE WSTRESS                                                  !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date   : December 2001                                              !
!  Author : B.A.M. Bouman                                              !
!  Version: 1.0 (Based on earlier versions of DSTRES)                  !
!  Version: Redistribution of transpiration uptake per soil layer.     !
!           Avarage stress factors over all soil layers                !
!           Calculations based on Wopereis et al (1996a)               !
!                                                                      !
!  Purpose: Calculate actual transpiration of a crop, and the  effects !
!          of water stress on growth and development of rice.          !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! DELT    R4  Time step of integration (d)                          T  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with input model data (-)                I  !
! TRC     R4  Potential transpiration rate (mm d-1)                 I  !
! ZRT     R4  Rooting depth (m)                                     I  !
! TKL     R4  Array of thicknesses soil layers (m)                  I  !
! NL      I4  Number of soil layers (-)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! WCLQT   R4  Array of actual soil water contents/layer (m3 m-3)    I  !
! WCWP    R4  Array of water content at wilting point/layer (m3 m-3)I  !
! WCAD    R4  Array of water content air dry/ layer (m3 m-3)        I  !
! MSKPA   R4  Array with soil water potential/layer (KPa)           I  !
! TRW     R4  Actual transpiration rate (mm)                        O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for dead leaves (-)                     O  !
! LESTRS  R4  Stress factor for expansion of leaves (-)             O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
!                                                                      !
! Files included: -                                                    !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WSTRESS (ITASK,  DELT,   OUTPUT, IUNITD, IUNITL, FILEI1, &
                          FILEIT, TRC,    ZRT,    TKL,    NL,    CROPSTA, &
                          WCLQT,  WCWP,   MSKPA, &
                          TRW,    TRWL,   LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)


      USE CHART
      IMPLICIT NONE

!-----Formal parameters
      INTEGER ITASK, IUNITD, IUNITL, NL, CROPSTA
      REAL    DELT, TRC, ZRT 
      REAL    TRW, LRSTRS, LDSTRS,PCEW, CPEW, LESTRS
      REAL    TKL(NL),WCLQT(NL),WCWP(NL)
      REAL    MSKPA(NL)
      LOGICAL OUTPUT, RDINQR
      CHARACTER (*) FILEI1, FILEIT 

!-----Local variables
!     The following parameters must be declared with INL=10 because
!     they are used/defined in COMMON blocks! (see PADDY subroutines)
      INTEGER    I,INL, J
      PARAMETER (INL=10)
      REAL       TRWL(INL), LR(INL), LE(INL), LD(INL), TRR(INL),WLA(INL)
      REAL       TRRM, ZRTL, ZLL , LRAV, LEAV, LDAV
      REAL       ULLS, LLLS, ULDL, LLDL, LLLE, ULLE, LLRT, ULRT
      CHARACTER(18) THEESTAB        !ADDED BY TAOLI, 27 JULY, 2010
      INTEGER    IESTAB             !IT IS 1 FOR DIRECT-SEED AND 2 FOR TRANSPLANT
      REAL LIMIT
      REAL TINY
      PARAMETER (TINY=0.0000000001)

      REAL NOTNUL

      CHARACTER (10) SWIRTR
      REAL SWIRTRF     !ADDED BY TAOLI, 1 JUNE 2010, FOR WATER STRESS QUANAFICATION

      SAVE
 
      IF (NL.GT.INL) CALL FATALERR ('STRESS','too many soil layers')

!====================================================================!
! INITIALIZATION (ITASK=1)                                           !
!====================================================================!

      IF (ITASK.EQ.1) THEN
!        Open crop input file
         CALL RDINIT(IUNITD,IUNITL,FILEI1)
 
!        Read initial states (Note: values in kPa)
         CALL RDSREA ('ULLS', ULLS)
         CALL RDSREA ('LLLS', LLLS)
         CALL RDSREA ('ULDL', ULDL)
         CALL RDSREA ('LLDL', LLDL)
         CALL RDSREA ('LLLE', LLLE)
         CALL RDSREA ('ULLE', ULLE)
         CALL RDSREA ('LLRT', LLRT)
         CALL RDSREA ('ULRT', ULRT)
         CALL RDSCHA ('SWIRTR', SWIRTR)
         CALL UPPERC (SWIRTR)
         IF (SWIRTR .NE. 'DATA' .AND. SWIRTR .NE. 'FUNCTION') THEN
            CALL FATALERR ('Crop data file','Unknown name for SWIRTR')
         END IF
         IF(SWIRTR.EQ.'FUNCTION') THEN
              IF(RDINQR('SWIRTRF')) THEN
                  CALL RDSREA('SWIRTRF',SWIRTRF)
              ELSE
                  SWIRTRF = 0.003297
              ENDIF
         ENDIF
         CLOSE (IUNITD)
         CALL RDINIT(IUNITD,IUNITL,FILEIT)
         CALL RDSCHA('ESTAB',THEESTAB)
         CLOSE (IUNITD)
         IF(THEESTAB.EQ.'TRANSPLANT') THEN
             IESTAB=2
         ELSE
             IESTAB=1
         ENDIF
         LRAV   = 1.
         LDAV   = 1.
         LEAV   = 1.
         LESTRS = 1.
         PCEW   = 1.
         CPEW   = 1.
         LRSTRS = 1.
         LDSTRS = 1.
         DO I=1,NL
           TRWL(I) = 0.
         END DO
         TRW = 0.

!====================================================================!
! RATE CALCULATION SECTION  (ITASK=2)                                !
!====================================================================!

      ELSE IF (ITASK.EQ.2) THEN

!--- Bas, 8 Sept, 2006. We calculate stress also in seedbed!
!-------Only stress in main field after day of emergence
        IF (CROPSTA .GT. IESTAB) THEN
        !IF (CROPSTA .GT. 2) THEN
           TRRM = TRC/(ZRT+1.0E-10)

           TRW  = 0.
           ZLL  = 0.
           LRAV = 0.
           LEAV = 0.
           LDAV = 0.

           DO I = 1,NL

!-------------Root length in each soil layer
              ZRTL  = MIN(TKL(I),MAX((ZRT-ZLL),0.0))

!-------------Leaf-rolling factor
              LR(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLLS)) &
                            /(LOG10(ULLS)-LOG10(LLLS))
              LR(I) = LIMIT(0.,1.,LR(I))
              LRAV  = LRAV+(ZRTL/(ZRT+TINY))*LR(I)

!-------------Relative leaf expansion rate factor
              LE(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLLE)) &
                            /(LOG10(ULLE)-LOG10(LLLE))
              LE(I) = LIMIT(0.,1.,LE(I))
              LEAV  = LEAV+(ZRTL/(ZRT+TINY))*LE(I)

!-------------Relative death rate factor
              LD(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLDL)) &
                            /(LOG10(ULDL)-LOG10(LLDL))
              LD(I) = LIMIT(0.,1.,LD(I))
              LDAV  = LDAV+(ZRTL/(ZRT+TINY))*LD(I)

!-------------Relative transpiration ratio (actual/potential)
              IF (MSKPA(I) .GE. 10000.) THEN
                 TRR(I) = 0.
              ELSE
                 IF (SWIRTR .EQ. 'DATA') THEN
                    TRR(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLRT)) &
                                  /(LOG10(ULRT)-LOG10(LLRT))
                    TRR(I) = LIMIT(0.,1.,TRR(I))
                 ELSE
                    TRR(I)  = 2./(1.+EXP(SWIRTRF*MSKPA(I)))
                 END IF
              END IF
              TRR(I)  = LIMIT(0.,1.,TRR(I))
              WLA(I)  = MAX(0.0,(WCLQT(I)-WCWP(I))*ZRTL*1000.)
              TRWL(I) = MIN(TRR(I)*ZRTL*TRRM,WLA(I)/DELT)
              TRW     = TRW + TRWL(I)
              ZLL     = ZLL+TKL(I)

           END DO

!-----Compensation of water extraction from soil layers if drought stress occurs
!     Take water from soil layer that has a surplus, starting from top.
           DO I = 1,NL
              IF (TRW .LT. TRC) THEN
                 IF (TRR(I).GE.1 .AND. TRWL(I).LT.WLA(I)/DELT) THEN
                    TRWL(I) = MIN(WLA(I)/DELT,(TRWL(I)+(TRC-TRW)/DELT))
                 END IF
                 TRW = 0.
                 DO J = 1,NL
                   TRW = TRW + TRWL(J)
                 END DO
              END IF
           END DO

           PCEW   = NOTNUL(TRW/TRC)

!-----Set stress factors as average over all layers: DEFAULT
           LRSTRS = LRAV
           LDSTRS = LDAV
           LESTRS = LEAV

!-------If crop is not in the main field, set all stres factors at 1.
        ELSE
          PCEW   = 1.
          LRSTRS = 1.
          LDSTRS = 1.
          LESTRS = 1.
        END IF

        CPEW = LESTRS

!------Output writing 
      IF (OUTPUT) THEN
         IF (CROPSTA .GE.1) THEN
            CALL OUTDAT (2, 0, 'TRW' , TRW)
            CALL ChartOutputRealScalar('TRW', TRW)
         END IF
      END IF

      END IF

      RETURN

      END
