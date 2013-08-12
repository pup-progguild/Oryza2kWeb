!----------------------------------------------------------------------!
!  SUBROUTINE ET2                                                      !
!  Used in ORYZA model version 4.0                                     !
!  Date  : December 2001; modified May 30, 2006                        !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Calculates potential evaporation of soil/water layer and   !
!           potential transpiration of a crop. Calculations done with: !
!           Penman, Priestley-Taylor or Makkink subroutines.           !
!           All calculations pertain to the main field, and not to     !
!           the seedbed.                                              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! ANGA    R4  Angstrom parameter A                                  I  !
! ANGB    R4  Angstrom parameter B                                  I  !
! RDD     R4  Daily shortwave radiation (J.m-2.d-1)                 I  !
! TMDA    R4  Daily average temperature (degrees C)                 I  !
! VP      R4  Early morning vapour pressure (kPa)                   I  !
! WN      R4  Average wind speed (m.s-1)                            I  !
! LAT     R4  Latitude of site (dec.degr.)                          I  !
! IDOY    I4  Day number within year of simulation (d)              I  !
! ETMOD   C*  Name of subroutine to calculate E and T (-)           I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! NL      I4  Number of soil layers (-)                             I  !
! FAOF    R4  Correction factor for E and T (FAO factor) (-)        I  !
! WL0     R4  Depth f ponded water layer (mm)                       I  !
! WCLQT   R4  Array of actual soil water contents/layer (m3 m-3)    I  !
! WCST    R4  Array of water content saturation / layer (m3 m-3)    I  !
! LAI     R4  Leaf Area Index (-)                                   I  !
! EVSC    R4  Potential soil evaporation (mm d-1)                   O  !
! ETD     R4  Reference evapotranspiration (mm d-1)                 O  !
! TRC     R4  Potential transpiration of crop at given LAI (mm d-1) O  !
!                                                                      !
! SUBROUTINES called: SETPMD, SETMKD, SETPTD                           !
!                                                                      !
! Files included: -                                                    !
!----------------------------------------------------------------------!
      SUBROUTINE ET2(ITASK, OUTPUT, ANGA , ANGB   , RDD , TMDA, VP  , WN , LAT, &
                    IDOY , DELT, ETMOD, CROPSTA, ESTAB, NL  , FAOF, WL0,      &
                    WCLQT, WCST , LAI    , EVSC, ETD , TRC)

          USE CHART

      IMPLICIT NONE
!     Formal parameters
      INTEGER       ITASK, IDOY, CROPSTA, NL 
      CHARACTER (*) ETMOD
      REAL          ANGA, ANGB, RDD , TMDA, VP , WN, LAT, FAOF
      REAL          WL0 , LAI ,       EVSC, ETD, TRC, DELT
      REAL          WCLQT(NL) , WCST(NL)
!     Local variables
      INTEGER       ISURF
      REAL          ALB, DT, ETAE, ETRD
      REAL          RF , RFS

      LOGICAL   OUTPUT

      CHARACTER (*) ESTAB
      REAL ETDCUM1, EVSCCUM1, TRCCUM1
      REAL ETDCUM2, EVSCCUM2, TRCCUM2
      REAL ETDCUM3, EVSCCUM3, TRCCUM3

      REAL INTGRL

      SAVE
!============================================================*
!------Initialization section
!============================================================*
      IF (ITASK .EQ. 1) THEN

      ETD = 0.
      EVSC = 0.
      TRC = 0.

      ETDCUM1  = 0.
      EVSCCUM1 = 0.
      TRCCUM1  = 0.

      ETDCUM2  = 0.
      EVSCCUM2 = 0.
      TRCCUM2  = 0.

      ETDCUM3  = 0.
      EVSCCUM3 = 0.
      TRCCUM3  = 0.

!============================================================*
!-----Rate calculation section
!============================================================*

      ELSE IF (ITASK .EQ. 2) THEN

!---- Set value for reflection coefficient of soil or water background
!     If there is standing water:
      IF (WL0 .GT. 5.) THEN
         ALB = 0.05
         RFS = ALB
!     If there is moist or dry soil
      ELSE
         ALB = 0.25
         RFS = ALB*(1.-0.5*WCLQT(1)/WCST(1))
      END IF

!---- The soil or water background is shielded by the crop
      RF  = RFS*EXP(-0.5*LAI)+0.25*(1.-EXP(-0.5*LAI))

!-----Penman evapotranspiration
      IF (ETMOD.EQ.'PENMAN') THEN

!-----Set ISURF value (soil or water background) for wind function in main field
!     Before transplanting: ISURF equals 1=open water, or 2 = bare soil)
!     After transplanting : ISURF equals 3
         IF (CROPSTA .LT. 3) THEN
            IF (WL0 .GT. 5.) THEN
               ISURF = 1
            ELSE
               ISURF = 2
            END IF
         ELSE
            ISURF = 3
         END IF

         CALL SETPMD (IDOY,LAT,ISURF,RF,ANGA,ANGB,0.,RDD,TMDA,WN,VP, &
                      ETD,ETRD,ETAE,DT)

!-----Makkink evapotranspiration
      ELSE IF (ETMOD.EQ.'MAKKINK') THEN
         CALL SETMKD (RDD, TMDA, ETD)

!        Estimate radiation-driven and wind- and humidity-driven part
         ETRD = 0.75*ETD
         ETAE = ETD-ETRD

!-----Priestley-Taylor evapotranspiration
      ELSE IF (ETMOD.EQ.'PRIESTLEY TAYLOR') THEN
         CALL SETPTD (IDOY,LAT,RF,RDD,TMDA,ETD)

!        Estimate radiation-driven and wind- and humidity-driven part
         ETRD = 0.75*ETD
         ETAE = ETD-ETRD
      END IF

!-----Multiplied by a factor according to FAO (1998)
      ETD  = ETD  * FAOF
      ETRD = ETRD * FAOF
      ETAE = ETAE * FAOF

!---- Calculate potential soil evaporation taking into account the standing crop   
      EVSC = EXP (-0.5*LAI)*(ETRD+ETAE)
      EVSC = MAX (EVSC, 0.)

! Bas, June 2006
!---- Calculate potential transpiration of rice crop (not anymore only in main field)
      TRC = ETRD*(1.-EXP(-0.5*LAI))+ETAE*MIN(2.0,LAI)
   
!         IF (OUTPUT) THEN
!            CALL OUTDAT (2, 0, 'ETDCUM1', ETDCUM1)
!            CALL ChartOutputRealScalar('ETDCUM1', ETDCUM1)
!           CALL OUTDAT (2, 0, 'ETDCUM2', ETDCUM2)
!            CALL ChartOutputRealScalar('ETDCUM2', ETDCUM2)
!           CALL OUTDAT (2, 0, 'ETDCUM3', ETDCUM3)
!            CALL ChartOutputRealScalar('ETDCUM3', ETDCUM3)
!         END IF

!============================================================*
!-----Integration section
!============================================================*

      ELSE IF (ITASK .EQ. 3) THEN

!---- Cumulative amounts
! Bas, Sept 8 2006: a whole new set of cumulative water balance components
!----- 1. From STTIME onwards
         ETDCUM1   = INTGRL (ETDCUM1, ETD   , DELT)
         EVSCCUM1  = INTGRL (EVSCCUM1, EVSC  , DELT)
         TRCCUM1   = INTGRL (TRCCUM1 , TRC   , DELT)
!------ 2. From emergence onward, both in direct-seeded and in transplanted systems
         IF (CROPSTA.GE.1) THEN
            ETDCUM2   = INTGRL (ETDCUM2, ETD   , DELT)
            EVSCCUM2  = INTGRL (EVSCCUM2, EVSC  , DELT)
            TRCCUM2   = INTGRL (TRCCUM2 , TRC   , DELT)
         END IF
!------ 2. From transplanting onward, only in transplanted systems
         IF (ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) THEN
            ETDCUM3   = INTGRL (ETDCUM3, ETD   , DELT)
            EVSCCUM3  = INTGRL (EVSCCUM3, EVSC  , DELT)
            TRCCUM3   = INTGRL (TRCCUM3 , TRC   , DELT)
         END IF

!============================================================*
!------Terminal section
!============================================================*

       ELSE IF (ITASK .EQ. 4) THEN

!         CALL OPSTOR ('ETDCUM1'  , ETDCUM1  )
!         CALL OPSTOR ('EVSCCUM1' , EVSCCUM1 )
!         CALL OPSTOR ('TRCCUM1' , TRCCUM1 )

         IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL OPSTOR ('ETDCUM2'  , ETDCUM2  )
            CALL OPSTOR ('EVSCCUM2' , EVSCCUM2 )
            CALL OPSTOR ('TRCCUM2' , TRCCUM2 )
         END IF

         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL OPSTOR ('ETDCUM3'  , ETDCUM3  )
            CALL OPSTOR ('EVSCCUM3' , EVSCCUM3 )
            CALL OPSTOR ('TRCCUM3' , TRCCUM3 )
         END IF

      END IF

      RETURN
      END
