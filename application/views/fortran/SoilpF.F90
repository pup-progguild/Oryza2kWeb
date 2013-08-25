!----------------------------------------------------------------------!
!  SUBROUTINE SOILPF                                                   !
!                                                                      !
!  Version: 1.0                                                        !
!  Date   : May 2004                                                   !
!  Authors: B.A.M. Bouman                                              !
!                                                                      !
!  Purpose: Calculate osmotic potential in kPa from salt concentration !
!                                                                      !
!  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!  name   type meaning                                    units  class *
!  ----   ---- -------                                    -----  ----- *
! ITASK   I4  Task that subroutine should perform            -   C,IN  *
! IUNITD  I4  Unit that can be used for input files          -   C,IN  *
! IUNITO  I4  Unit used for output file                      -   C,IN  *
! FILEI2  C*  Name of input file no. 2                       -   C,IN  *
! OUTPUT  L4  Flag to indicate if output should be done      -   C,IN  *
! DOY     R4  Day number (January 1 = 1) (d)                        I  *
! DVS     R   Development stage                              -      I  *
! NL      I4  Number of soil layers (-)                             O  *
! TKL     R4  Array of layer thicknesses (m)                        O  *
! TKLT    R4  Depth of simulated soil (m)                           O  *
! ZRTMS   R4  Maximum rooting depth of soil profile (m)             O  *
! WCWP    R4  Array of water content at wilting point/layer (m3 m-3)O  *
! WCFC    R4  Array of water content field capacity/layer (m3 m-3)  O  *
! WCST    R4  Array of water content saturation/layer (m3 m-3)      O  *
! WCLQT   R4  Array of actual water content/layer (m3 m-3)          O  *
! WL0     R4  Depth of ponded water (mm)                            O  *
! MSKPA   R4  Array of soil water tension/soil layer (kPa)          O  *
!                                                                      *
!  Subroutines called: From library TTUTIL: OUTCOM,                    *
!                     RDSREA, RDSINT, RDSCHA, OUTDAT                   *
!  Data files needed: soil definition file FILEI2 (as specified in     *
!                     in the file CONTROL.DAT)                         *
!----------------------------------------------------------------------*

      SUBROUTINE SOILPF(ITASK, IUNITD, IUNITO, FILEI2, OUTPUT, & 
           DOY, DVS, NL, TKL, TKLT, ZRTMS, &
           WCWP, WCFC, WCST, WCLQT, WL0, MSKPA)

      USE CHART

      IMPLICIT NONE
!---- Formal parameters
      INTEGER       ITASK, IUNITD, IUNITO, NL
      LOGICAL       OUTPUT
      CHARACTER (*) FILEI2
      REAL          DOY,ZRTMS, TKLT, WL0, DVS
      CHARACTER (10) SCODE, STYPE

      INTEGER      I, MNL
      PARAMETER   (MNL=10)
      REAL         TKL(MNL), MSKPA(MNL)
      REAL         WCAD(MNL),WCWP(MNL),WCFC(MNL),WCST(MNL),WCLQT(MNL)

      INTEGER IMX, IPF, IPFC
      PARAMETER (IMX=500)
      REAL PFDVS(IMX), PFDAY(IMX), PFCURVE(IMX)

!-----Functions 
      REAL LINT2
      SAVE

!============================================================*
!------Initialization section
!============================================================*
      IF (ITASK.EQ.1) THEN

!-- Read data from soil input data file
       CALL RDINIT (IUNITD, IUNITO, FILEI2)
         CALL RDSCHA('SCODE',SCODE)
         IF (SCODE .NE. 'SOILPF') THEN
            CALL FATALERR ('SOILPF','Wrong soil input file for SOILPF water balance')
         END IF
!-- Read soil water tension data 
         CALL RDSCHA('STYPE', STYPE)
         IF (STYPE .EQ.'SDVS') THEN
             CALL RDAREA('PFDVS ',PFDVS,IMX,IPF)
         ELSE IF (STYPE .EQ.'SDAY') THEN
             CALL RDAREA('PFDAY ',PFDAY,IMX,IPF)
         ELSE
            CALL FATALERR ('STYPE','Wrong name for STYPE in SOILPF data file')
         END IF
!-- Read soil water retention characteristics
         CALL RDAREA('PFCURVE ',PFCURVE,IMX,IPFC)
       CLOSE (IUNITD)

!-- Fill a soil of 1 m depth with 10 layers of 10 cm (dummy)
!-- Set soil moisture potentials in all soil layers to zero
!-- Interpolate soil water content at critical points
       NL = MNL
       TKLT = 0.
       DO I=1,NL
         TKL(I) = 0.10
         TKLT    = TKLT+TKL(I)
         WCLQT(I) = 0.3
         MSKPA(I) = 0.
         WCST(I)  = LINT2('PFCURVE',PFCURVE,IPFC,0.)
         WCFC(I)  = LINT2('PFCURVE',PFCURVE,IPFC,10.)
         WCWP(I)  = LINT2('PFCURVE',PFCURVE,IPFC,100.)
         WCAD(I)  = LINT2('PFCURVE',PFCURVE,IPFC,1500.)
       END DO

! Initialize water tension and water content values
!---- Read water tension values for each soil layer
         IF (STYPE .EQ.'SDVS') THEN
            DO I =1,NL
               MSKPA(I) = LINT2('PFDVS',PFDVS,IPF,DVS)
            END DO
         ELSE IF (STYPE.EQ.'SDAY') THEN
            DO I =1,NL
               MSKPA(I) = LINT2('PFDAY',PFDAY,IPF,DOY)
            END DO
         END IF

!---- Interpolate soil moisture content values for each soil layer
            DO I =1,NL
               WCLQT(I) = LINT2('PFCURVE',PFCURVE,IPFC,MSKPA(I))
            END DO

!--Set maximum rooting depth to 1 m
!--Set ponded water dpeth to 0.
       ZRTMS = 1.
       WL0=0.

!============================================================*
!-----Rate calculation section
!============================================================*
      ELSE IF (ITASK.EQ.2) THEN

! No calculations, only output writing

!--------Write output
         IF (OUTPUT) THEN
            CALL OUTDAT(2,0,'MSKPA',MSKPA(1))
            CALL ChartOutputRealScalar('MSKPA',MSKPA(1))
            CALL OUTDAT(2,0,'WCLQT',WCLQT(1))
            CALL ChartOutputRealScalar('WCLQT',WCLQT(1))
         END IF

!============================================================*
!-----Integration section
!============================================================*
      ELSE IF (ITASK .EQ. 3) THEN

!---- Read soil water tension values for each soil layer
         IF (STYPE .EQ.'SDVS') THEN
            DO I =1,NL
               MSKPA(I) = LINT2('PFDVS',PFDVS,IPF,DVS)
            END DO
         ELSE IF (STYPE.EQ.'SDAY') THEN
            DO I =1,NL
               MSKPA(I) = LINT2('PFDAY',PFDAY,IPF,DOY)
            END DO
         END IF

!---- Interpolate soil moisture content values for each soil layer
            DO I =1,NL
               WCLQT(I) = LINT2('PFCURVE',PFCURVE,IPFC,MSKPA(I))
            END DO

!============================================================*
!------Terminal section
!============================================================*
       ELSE IF (ITASK .EQ. 4) THEN

! No calculations

      END IF

      RETURN
      END
