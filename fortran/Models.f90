!----------------------------------------------------------------------!
! SUBROUTINE MODELS                                                    !
! Author(s): Daniel van Kraalingen  (original)                         !
! Date     : 5-Jul-1993, Version: 1.1                                  !
!          Version august, 2003                                        !
!                                                                      !
! This version used in ORYZA2000 FSEWin model;                         !
! Date     : November 2002; Adapted by B.A.M. Bouman                   !
! Purpose  : This subroutine is the interface routine between the FSE  !
!            driver and the simulation models. This routine is called  !
!            by the FSE driver at each new task at each time step. It  !
!            can be used by the user to specify calls to the different !
!            models that have to be simulated.                         !
!                                                                      !
! FORMAL PARAMETERS:  I=input, O=output                                !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit number that is used for input files (-)          I  !
! IUNITO  I4  Unit number that is used for output file (-)          I  !
! IUNITL  I4  Unit number that is used for log file (-)             I  !
! FILEIT  C*  Name of timer file (-)                                I  !
! FILEI1  C*  Name of input file no. 1 (-)                          I  !
! FILEI2  C*  Name of input file no. 2 (-)                          I  !
! FILEI3  C*  Name of input file no. 3 (-)                          I  !
! FILEI4  C*  Name of input file no. 4 (-)                          I  !
! FILEI5  C*  Name of input file no. 5 (-)                          I  !
! OUTPUT  L4  Flag to indicate if output should be done (-)         I  !
! TERMNL  L4  Flag to indicate if simulation is to stop (-)        I/O !
! DOY     R4  Day number since 1 January (day of year) (d)          I  !
! IDOY    I4  Day number within year of simulation (d)              I  !
! YEAR    R4  Year of simulation (y)                                I  !
! IYEAR   I4  Year of simulation (y)                                I  !
! STTIME  R4  Start day of simulation (d)                           I  !
! TIME    R4  Time of simulation (d)                                I  !
! DELT    R4  Time interval of integration (d)                      I  !
! LAT     R4  Latitude of site (dec.degr.)                          I  !
! WSTAT   C*  Status code from weather system (-)                   I  !
! WTRTER  L4  Flag whether weather can be used by model (-)         O  !
! RDD     R4  Daily shortwave radiation (kJ.m-2.d-1)                I  !
! TMMN    R4  Daily minimum temperature (degrees C)                 I  !
! TMMX    R4  Daily maximum temperature (degrees C)                 I  !
! VP      R4  Early morning vapour pressure (kPa)                   I  !
! WN      R4  Average wind speed (m.s-1)                            I  !
! RAIN    R4  Daily amount of rainfall (mm.d-1)                     I  !
!                                                                      !
! Subroutines called: ET, WSTRESS, WNOSTRESS, IRRIG, PADDY             !
!                     NSOIL, NNOSTRESS, NCROP, ORYZA1                  !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE MODELS (ITASK , IUNITD, IUNITO, IUNITL,                 &
                         FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                         OUTPUT, TERMNL,                                 &
                         DOY   , IDOY  , YEAR  , IYEAR , STTIME,         &
                         TIME  , DELT ,  LAT   , WSTAT , WTRTER,         &
                         RDD   , TMMN  , TMMX  , VP    , WN    , RAIN)

          USE CHART

        IMPLICIT NONE

!-----Formal parameters
      INTEGER   ITASK, IUNITD, IUNITO, IUNITL, IDOY, IYEAR

      REAL      DOY  , YEAR  , TIME  , DELT  , LAT , STTIME
      REAL      RDD  , TMMN  , TMMX  , VP    , WN  , RAIN
      CHARACTER (*) FILEIT, FILEI1, FILEI2, FILEI3, &
                    FILEI4, FILEI5
      LOGICAL   OUTPUT, TERMNL, WTRTER
      CHARACTER (*) WSTAT

!-----Local variables
      INTEGER        I, I1, NL
      CHARACTER (6)  WUSED
      CHARACTER (80) ESTAB  , ETMOD   , PRODENV, RUNMODE, NITROENV
      CHARACTER (80) WATBAL , RICETYPE
      INTEGER        CROPSTA, DTFSECMP, EMYR   , EMD    , IDATE , SBDUR
      REAL           ANGA   , ANGB    , DAE    , DVS    , ETD   , EVSC
      REAL           FAOF   , IR      , LAI    , LDSTRS , LESTRS, LRSTRS
      REAL           PCEW   , RAINCU  , SWR    , TKLT   , TMDA  , TRC  
      REAL           TRW    , WL0     , ZRT    , ZRTMS
      REAL           LAIROL , CPEW    ,SLA

      INTEGER      NLXM
      PARAMETER   (NLXM=10)
      REAL         MSKPA(NLXM), TKL(NLXM) , TRWL(NLXM)
      REAL         WCAD(NLXM) , WCWP(NLXM)  , WCFC(NLXM), WCST(NLXM)
      REAL         WCLQT(NLXM)
      LOGICAL      GIVEN

      REAL LLV, DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, PLTR
      REAL TNSOIL, NACR, NSLLV, NFLV, RNSTRS

      SAVE

      DATA WUSED /'------'/, GIVEN /.FALSE./

!     avoid compiler warnings for not using these variables
      FILEI4 = ' '
      FILEI5 = ' '

!==============================================================!
! Initialization section: ITASK = 1                            !
!==============================================================!
      IF (ITASK.EQ.1) THEN

!--------Read data from the experimental data file
         CALL RDINIT (IUNITD, IUNITL, FILEIT)
         CALL RDSCHA ('RICETYPE' , RICETYPE)
         CALL RDSCHA ('RUNMODE' , RUNMODE)
         CALL RDSCHA ('ESTAB'   , ESTAB  )
         CALL RDSCHA ('ETMOD'   , ETMOD  )
         CALL RDSCHA ('PRODENV' , PRODENV)
         CALL RDSCHA ('NITROENV' , NITROENV)
         IF (PRODENV.EQ.'WATER BALANCE') THEN
            CALL RDSCHA ('WATBAL' , WATBAL)
         END IF
         CALL UPPERC (RICETYPE)
         CALL UPPERC (RUNMODE)
         CALL UPPERC (ESTAB  )
         CALL UPPERC (ETMOD  )
         CALL UPPERC (PRODENV)
         CALL UPPERC (NITROENV)
         CALL UPPERC (WATBAL)
         CALL RDSREA ('ANGA' , ANGA)
         CALL RDSREA ('ANGB' , ANGB)
         IF (RUNMODE.EQ.'EXPERIMENT') THEN
              CALL RDSINT('EMYR' , EMYR)
              CALL RDSINT('EMD'  , EMD )
         ELSE IF (RUNMODE.EQ.'EXPLORATION') THEN
              SWR = IYEAR
              EMD = NINT(STTIME)
         ELSE
            CALL FATALERR  &
            ('MODELS','unknown name for RUNMODE')
         END IF
         CALL RDSINT('SBDUR  ',SBDUR)
         CALL RDSREA('FAOF'   ,FAOF )
         CLOSE (IUNITD)

!--------Initialize variables
         CROPSTA  = 0
         WL0      = 0.
         RAINCU   = 0.
         DO I=1,NLXM
            WCLQT(I) = 0.3
            WCST(I)  = 0.3
         END DO

!--------Choose rice type to be modelled
         IF (RICETYPE.EQ.'LOWLAND') THEN
            WRITE (IUNITO,'(A,T7,A)') '*','Lowland rice modelled'
         ELSE IF (RICETYPE.EQ.'AEROBIC') THEN
            WRITE (IUNITO,'(A,T7,A)') '*','Upland or aerobic rice modelled'
         ELSE
            CALL FATALERR ('MODELS','unknown name for RICETYPE')
         END IF

!--------Write and check water production situation setting
         IF (PRODENV.EQ.'POTENTIAL') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','Rice grown in potential water production situation'
         ELSE IF (PRODENV.EQ.'WATER BALANCE') THEN
!           Select water balance model
            IF (WATBAL.EQ.'PADDY') THEN
               WRITE (IUNITO,'(A,T7,A)')'*','Water balance PADDY used'
            ELSE IF (WATBAL.EQ.'SAWAH') THEN
               WRITE (IUNITO,'(A,T7,A)')'*','Water balance SAWAH used'
            ELSE IF (WATBAL.EQ.'SAHEL') THEN
               WRITE (IUNITO,'(A,T7,A)')'*','Water balance SAHEL used'
            ELSE IF (WATBAL.EQ.'LOWBAL') THEN
               WRITE (IUNITO,'(A,T7,A)')'*','Water balance LOWBAL used'
            ELSE IF (WATBAL.EQ.'SOILPF') THEN
               WRITE (IUNITO,'(A,T7,A)')'*','Water balance SOILPF used'
            ELSE
               CALL FATALERR  &
                  ('MODELS','unknown name for soil water balance')
            END IF
            WUSED(6:6) = 'U'
         ELSE
            CALL FATALERR  &
               ('MODELS','unknown name for production situation')
         END IF

!--------Choose and check info on nitrogen production situation setting
         IF (NITROENV.EQ.'POTENTIAL') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','Rice grown in potential N production situation'
         ELSE IF (NITROENV.EQ.'NITROGEN BALANCE') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','Crop and soil nitrogen balance used'
         ELSE
            CALL FATALERR ('MODELS','unknown name for NITROENV')
         END IF

!--------Send warning if water and nitrogen limitations are combined
         IF (NITROENV.EQ.'NITROGEN BALANCE' .AND. PRODENV.EQ.'WATER BALANCE') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','****************************************************************'
            WRITE (IUNITO,'(A,T7,A)') &
              '*','WARNING: Combined water and nitrogen limitations not validated!!'
            WRITE (IUNITO,'(A,T7,A)') &
              '*','****************************************************************'
         END IF

!--------Write information about RUNMODE to output file
         IF (RUNMODE.EQ.'EXPERIMENT') THEN
             WRITE (IUNITO,'(A,T7,A)') &
             '*','ORYZA model runs to simulate an experiment'
         ELSE IF (RUNMODE.EQ.'EXPLORATION') THEN
             WRITE (IUNITO,'(A,T7,A)') &
              '*','ORYZA model runs for exploration'
         ELSE
            CALL FATALERR  &
               ('MODELS','unknown name for RUNMODE')
         END IF

!--------Choose and check establishment setting
         IF (ESTAB.EQ.'TRANSPLANT') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','Rice crop is transplanted'
         ELSE IF (ESTAB.EQ.'DIRECT-SEED') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','Rice crop is direct-seeded'
!BB: set SBDUR to 0. if direct seeded:
            SBDUR = 0.
         ELSE
            CALL FATALERR &
               ('MODELS','unknown name for establishment')
         END IF

!--------Choose and check evapotranspiration modules
         IF (ETMOD.EQ.'PENMAN') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','SETPMD: Penman evapotranspiration'
            WUSED(1:5) = 'UUUUU'
         ELSE IF (ETMOD.EQ.'MAKKINK') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','SETMKD: Makkink evapotranspiration'
            WUSED(1:3) = 'UUU'
         ELSE IF (ETMOD.EQ.'PRIESTLEY TAYLOR') THEN
            WRITE (IUNITO,'(A,T7,A)') &
              '*','SETPTD: Priestley Taylor evapotranspiration'
            WUSED(1:3) = 'UUU'
         ELSE
            CALL FATALERR &
               ('MODELS','unknown module name for evapotranspiration')
         END IF

!--------Check weather data for ORYZA crop model
         WUSED(1:3) = 'UUU'


!--------Write log messages to output file
         WRITE (IUNITO,'(A,76A1)') '*',('=',I1=1,76)
         WRITE (IUNITO,'(A)') '*'
         WRITE (IUNITO,'(A)') '* FSE driver info:'
         WRITE (IUNITO,'(A,T7,A,I5,A,I4,A)') &
           '*','Year:',IYEAR,', day:',IDOY,', System start'

      END IF

!==============================================================!
! Here ended initialization section ITASK = 1                  !
!==============================================================!

!-----Check weather data availability
      IF (ITASK.EQ.1.OR.ITASK.EQ.2.OR.ITASK.EQ.4) THEN
         IF (WSTAT(6:6).EQ.'4') THEN
            RAIN       = 0.
            WSTAT(6:6) = '1'
            IF (.NOT.GIVEN) THEN
               WRITE (IUNITL,'(2A)') ' Rain not available,', &
                 ' value set to zero, (patch DvK, Jan 1995)'
               GIVEN = .TRUE.
            END IF
         END IF
!--------Check whether there is an error in the I1-th weather variable
         DO I1=1,6
            IF (WUSED(I1:I1).EQ.'U' .AND. &
                WSTAT(I1:I1).EQ.'4') THEN
               WTRTER = .TRUE.
               TERMNL = .TRUE.
               RETURN
            END IF
         END DO
      END IF

!-----Calculate average temperature
      TMDA = (TMMX+TMMN)/2.

!-----Calculate potential soil evaporation and transpiration
      CALL ET2(ITASK,OUTPUT, ANGA,  ANGB,  RDD,   TMDA,    VP,  WN,   LAT, &
                    IDOY,  DELT, ETMOD, CROPSTA, ESTAB, NL,  FAOF, WL0, &
                    WCLQT, WCST,  LAI,   EVSC,    ETD, TRC)

!-----Calculate drought stress factors
      IF (PRODENV.EQ.'WATER BALANCE') THEN
!         Lowland rice drought stress routine
          IF (RICETYPE.EQ.'LOWLAND') THEN
             CALL WSTRESS (ITASK,  DELT,   OUTPUT, IUNITD, IUNITL, FILEI1,&
                       FILEIT, TRC,    ZRT,    TKL,    NL,    CROPSTA, &
                       WCLQT,  WCWP,   MSKPA,                 &
                       TRW,    TRWL,   LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
!          ELSE IF (RICETYPE.EQ. 'AEROBIC') THEN
!         Upland/aerobic rice drought stress routine
!              CALL WSTRESSAEROBIC (ITASK,  DELT,   OUTPUT, IUNITD, IUNITL, &
!                          FILEI1, TRC,    ZRT,   TKL,    NL,    CROPSTA, &
!                          WCLQT,  WCWP,  WCFC,   WCST, &
!                          TRW,    TRWL,  LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
          END IF
      ELSE IF (PRODENV.EQ.'POTENTIAL') THEN
         CALL WNOSTRESS(NL,TRW, TRWL,LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
      END IF

!-----Call the crop growth module
       CALL ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEIT, &
                        OUTPUT, TERMNL, IDOY  , DOY, &
                        TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, &
                        NFLV,   NSLLV,  RNSTRS,                 &
                        ESTAB,  TKLT,   ZRTMS,  CROPSTA, &
                        LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, &
                        DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, &
                        PLTR)
!-----Call the nitrogen crop demand and soil supply modules
      IF (NITROENV.EQ.'NITROGEN BALANCE') THEN
!         CALL NCROP(ITASK, IUNITD, IUNITL, FILEI1, DELT, TIME, OUTPUT, &
!                    TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV,&
!                    PLTR, LAI, CROPSTA, TNSOIL, NACR, NFLV, NSLLV,RNSTRS)
         CALL NCROP2(ITASK, IUNITD, IUNITL, FILEI1, FILEIT, DELT, TIME, OUTPUT, &
                    TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV,&
                    PLTR, LAI, SLA, CROPSTA, TNSOIL, NACR, NFLV, NSLLV,RNSTRS)
         CALL NSOIL(ITASK, IUNITD, IUNITL, FILEIT, OUTPUT, DELT, DAE, &
                    DVS, NACR, TNSOIL)
      ELSE IF (NITROENV.EQ.'POTENTIAL') THEN
!         CALL NNOSTRESS(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
!                           CROPSTA, DVS, NFLV, NSLLV, RNSTRS)
          CALL NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, RNSTRS)

       END IF

!-----Call the water balance module
      IF (PRODENV.EQ.'WATER BALANCE') THEN
!--      First, the irrigation subroutine
         CALL IRRIG (ITASK, IUNITD, IUNITL,  FILEIT, OUTPUT, &
                        DOY,   DELT,   CROPSTA, WL0, &
                        DVS, NL,    WCLQT,  MSKPA,   IR)
!        Then the soil water balans module
         IF (WATBAL.EQ.'PADDY') THEN
            CALL PADDY (ITASK, IUNITD, IUNITL, FILEI2, OUTPUT, &
                      DOY,    DELT,   TIME,   CROPSTA,  ESTAB, &
                      RAIN,   EVSC,   TRWL,   TRW,      IR, &
                      NL,     ZRTMS,  TKL,    TKLT, &
                      WCAD,   WCWP,   WCFC,   WCST,     WCLQT, &
                      WL0,    MSKPA)
         ELSE IF (WATBAL.EQ.'SAWAH') THEN
            CALL SAWAH  (ITASK, IUNITD, IUNITL, FILEI2, OUTPUT, &
                       ESTAB,  CROPSTA, &
                       DOY,   DELT,   TIME,   RAIN,   IR,  EVSC, &
                       TRC,    TRWL, NL,    ZRTMS,  TKL,    TKLT, &
                       WCAD,   WCWP,   WCFC,   WCST, WCLQT, &
                       WL0,    MSKPA)
         ELSE IF (WATBAL.EQ.'SAHEL') THEN
            CALL DRSAHE (ITASK , IUNITD, IUNITL, FILEI2 ,       &
                        DELT  , OUTPUT, TERMNL, ESTAB, CROPSTA,       &         
                        NL    , EVSC  , RAIN  , IR, ZRT,   &
                        TRWL  , TKL   , TKLT  , ZRTMS , WL0 ,  &
                        WCAD  , WCWP  , WCFC  , WCST ,         &
                        WCLQT , MSKPA)
         ELSE IF (WATBAL.EQ.'LOWBAL') THEN
            CALL LOWBAL (ITASK, IUNITD, IUNITO, &
                         FILEI2, OUTPUT, DELT, TIME, ESTAB, CROPSTA,IR,&
                         TRWL, EVSC, RAIN, NL, TKL, TKLT, ZRTMS,    &
                         WCWP, WCFC, WCST, WCLQT, WL0, MSKPA) 
         ELSE IF (WATBAL.EQ.'SOILPF') THEN
           CALL SOILPF(ITASK, IUNITD, IUNITO, FILEI2, OUTPUT, & 
                       DOY, DVS, NL, TKL, TKLT, ZRTMS, &
                       WCWP, WCFC, WCST, WCLQT, WL0, MSKPA)
         END IF
!---  No water balance in potential situation
      ELSE IF (PRODENV.EQ.'POTENTIAL') THEN
         TKLT = 100.
         ZRTMS = 100.
         WL0=0.
         NL = NLXM
         DO I=1,NL
           WCLQT(I) = 0.3
           WCST(I)  = 0.3
         END DO
      END IF


!==============================================================!
! Output writing only at ITASK = 2                             !
!==============================================================!
      IF (ITASK.EQ.2) THEN
         IF (OUTPUT) THEN
            CALL OUTDAT (2, 0, 'YEAR', YEAR)
            CALL ChartOutputRealScalar('YEAR', YEAR)
            CALL OUTDAT (2, 0, 'DOY' , DOY)
            CALL ChartOutputRealScalar('DOY', DOY)
            CALL OUTDAT (2, 0, 'CROPSTA ', REAL (CROPSTA))
            CALL ChartOutputRealScalar('CROPSTA ', REAL (CROPSTA))
            CALL OUTDAT (2, 0, 'ETD' , ETD)
            CALL ChartOutputRealScalar('ETD', ETD)
            CALL OUTDAT (2, 0, 'TRC' , TRC)
            CALL ChartOutputRealScalar('TRC', TRC)
            CALL OUTDAT (2, 0, 'EVSC', EVSC)
            CALL ChartOutputRealScalar('EVSC', EVSC)
            CALL OUTDAT (2, 0, 'RAIN', RAIN)
            CALL ChartOutputRealScalar('RAIN', RAIN)
            CALL OUTDAT (2, 0, 'RAINCU',   RAINCU)
            CALL ChartOutputRealScalar('RAINCU', RAINCU)
         END IF
      END IF

!-----Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period
      IF (ITASK.EQ.1 .OR. ITASK.EQ.3) THEN
         IF (CROPSTA .EQ. 3) CROPSTA = 4
         IF (CROPSTA .EQ. 2) THEN
            IF (DAE .EQ. REAL(SBDUR)) CROPSTA = 3
         END IF
         IF (CROPSTA .EQ. 1) THEN
            IF (ESTAB.EQ.'TRANSPLANT') THEN
               CROPSTA = 2
            ELSE IF (ESTAB.EQ.'DIRECT-SEED') THEN
               CROPSTA = 4
            END IF
        END IF
        IF (CROPSTA .EQ. 0) THEN
           IDATE = DTFSECMP(EMYR, EMD, IYEAR, IDOY)
           IF (IDATE .EQ. 0) THEN
              CROPSTA = 1
           ELSE IF (IDATE .EQ. 1) THEN
              CALL FATALERR ('MODELS',  &
                'Time past supplied sowing date or year')
           END IF
        END IF

      END IF

!============================================================*
!-----Integration section
!============================================================*

      IF (ITASK .EQ. 3) THEN


!-------Summation of some state variables
        RAINCU = RAINCU + RAIN

      END IF

!==============================================================!
! Terminal calculations at ITASK = 4                           !
!==============================================================!
      IF (ITASK.EQ.4) THEN
!         WRITE (IUNITO,'(A)') '*'
!         WRITE (IUNITO,'(A)') '* FSE driver info:'
         WRITE (IUNITO,'(A,T7,A,I5,A,I4,A)') &
           '*','Year:',IYEAR,', day:',IDOY,', System end'
!---     Terminal output
         CALL OPSTOR ('EMD', 1.0*EMD)
         CALL OPSTOR ('DAE', DAE)
      END IF
!==============================================================!
! End of section ITASK = 4                                     !
!==============================================================!

      RETURN
      END





