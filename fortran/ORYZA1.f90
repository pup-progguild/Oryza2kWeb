!----------------------------------------------------------------------*
! SUBROUTINE ORYZA1                                                    *
! Rice crop growth module of ORYZA2000 model                           *
!                                                                      *
! Date      : November 2002                                            *
!          Version august, 2003                                        *
! History   : Adapted from ORYZA1 (1995), and ORYZA_W (1996) models    *
!             This version for release under FSEWin                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! IUNITD  I4  Unit that can be used for input files (-)             I  *
! IUNITL  I4  Unit number for log file messages (-)                 I  *
! FILEI1  C*  Name of file with input model data (-)                I  *
! FILEIT  C*  Name of experimental data file (-)                    I  *
! OUTPUT  L4  Flag to indicate if output should be done (-)         I  *
! TERMNL  L4  Flag to indicate if simulation is to stop (-)        I/O *
! IDOY    I4  Day number within year of simulation (d)              I  *
! DOY     R4  Day number (January 1 = 1) (d)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! DELT    R4  Time step of integration (d)                          I  *
! LAT     R4  Latitude of site (dec.degr.)                          I  *
! RDD     R4  Daily shortwave radiation (J m-2 d-1)                 I  *
! TMMN    R4  Daily minimum temperature (degrees C)                 I  *
! TMMX    R4  Daily maximum temperature (degrees C)                 I  *
! NFLV    R4  N fraction in the leaves (g N m-2)                    I  *
! NSLLV   R4  N stress factor that accelerates leaf death (-)       I  *
! RNSTRS  R4  N stress reduction factor for RGRL (-)                I  *
! ESTAB   C*  Mode of establishment (-)                             I  *
! TKLT    R4  Thickness of combined soil layers (m)                 I  *
! ZRTMS   R4  Maximum rooting depth of soil (m)                     I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! LDSTRS  R4  Leaf death stress factor (-)                          I  *
! LESTRS  R4  Leaf expansion stress factor (-)                      I  *
! PCEW    R4  Reduction in potential transpiration rate (-)         I  *
! DAE     R4  Days after emergence (d)                              O  *
! LAIROL  R4  Leaf area index rolled (ha ha-1)                      O  *
! ZRT     R4  Rooting depth (m)                                     O  *
! DVS     R4  Development stage of the crop (-)                     O  *
! LLV     R4  Loss rate of leaves (kg ha-1 d-1)                     O  *
! DLDR    R4  Death rate of leaves caused by drought (kg ha-1 d-1)  O  *
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  O  *
! WST     R4  Dry weight of stems (kg ha-1)                         O  *
! WSO     R4  Dry weight of storage organs (kg ha-1)                O  *
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           O  *
! GGR     R4  Rate of increase in grain weight (kg ha-1 d-1)        O  *
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    O  * 
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   O  *
! PLTR    R4  Intermediate variable for planting density (-)        O  *
!                                                                      *
! SUBROUTINES called: PHENOL, SUBLAI3, SUBDD, SUBCD, SUBCBC, SUBGRN    *
!                     GPPARSET, SGPCDT                                 *                  
!                                                                      *
! Files included:      -                                               *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEIT, &
                        OUTPUT, TERMNL, IDOY  , DOY, &
                        TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, &
                        NFLV,   NSLLV,  RNSTRS,                 &
                        ESTAB,  TKLT,   ZRTMS,  CROPSTA, &
                        LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, &
                        DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, &
                        PLTR)

!===================================================================*
!     DECLARATION SECTION                                           *
!===================================================================*
      USE CHART

      IMPLICIT NONE
 
!-----Formal parameters
      INTEGER       ITASK , IUNITD, IUNITL, CROPSTA, IDOY
      LOGICAL       OUTPUT, TERMNL
      CHARACTER (*) FILEI1, FILEIT
      CHARACTER (*) ESTAB
      REAL          DOY , TIME, DELT  , LAT  , RDD
      REAL          TMMN, TMMX, TMAXC, TMINC, TKLT  , ZRTMS, LRSTRS, LDSTRS, LESTRS
      REAL          PCEW, CPEW, DAE , LAIROL, ZRT  , DVS, NSLLV, RNSTRS

!-----Local variables
      LOGICAL DLEAF, DROUT, INQOBS  , GRAINS

      INTEGER IMX 
      PARAMETER (IMX=40)
      INTEGER ILDRLV, ILEFFT, ILFLVT, ILFSHT, ILFSOT, ILFSTT
      INTEGER ILKDFT, ILKNFT, ILSLAT
      INTEGER ILREDF, ILSSGA, ILTMCT
      REAL    DRLVT(IMX) , EFFTB(IMX), SLATB(IMX) 
      REAL    FLVTB(IMX) , FSHTB(IMX), FSOTB(IMX) , FSTTB(IMX)
      REAL    KDFTB(IMX) , KNFTB(IMX) 
      REAL    REDFTT(IMX), SSGATB(IMX), TMCTB(IMX)
      REAL    ALAI  , AMAX
      REAL    CBCHK , CKCIN , CKCFL
      REAL    CO2   , CO2EFF, CO2LV, CO2ST, CO2STR, CO2SO , CO2REF, CO2RT
      REAL    CRGCR , CRGLV , CRGRT, CRGSO, CRGST , CRGSTR, CTRANS 
      REAL    DAYL  , DAYLP , DLDR , DLDRT, DPAR  , DPARI , DTGA  , DTR
      REAL    DVEW  , DVR   , DVRI , DVRJ , DVRP  , DVRR  , DVSI 
      REAL    EFF   , FCLV  , FCRT , FCSO , FCST  , FCSTR , FLV   , FSH
      REAL    FRPAR , FSO   , FRT  , FST  , FSTR
      REAL    GCR   , GGR   , GLAI , GLV  , GNGR  , GNSP  , GRT   , GRT1
      REAL    GSO   , GST   , GST1 , GSTR , GZRT
      REAL    HU    , HULV 
      REAL    KEEP  , KDF   , KNF     , LAI
      REAL    LAPE  , LLV   , LRSTR   , LSTR
      REAL    MAINLV, MAINRT, MAINSO  , MAINST, MNDVS , MOPP
      REAL    NCOLD , NFLV  , NGCR  , NGR   , NGRM2
      REAL    NH    , NPLDS , NPLH    , NPLSB , NSP   , NSPM2
      REAL    PARCM1, PARCUM, PARI1   , PLTR  , PPSE  , PWRR  , Q10
      REAL    RAPCDT, RDAE  , REDFT , RGCR    , RGRL  , RMCR  , RTNASS, RWLVG 
      REAL    RWLVG1, RWSTR , RWSTR1
      REAL    SAI   , SCP   , SF1     , SF2   , SHCKD , SHCKL , SLA  
      REAL    SPFERT, SPGF  , SSGA 
      REAL    TAV   , TAVD  , TBD     , TBLV  , TCLSTR, TCOR  , TDRW
      REAL    TEFF  , TMAX  , TMIN    , TMPCOV, TMPSB , TMD   , TNASS 
      REAL    TOD   , TS    , TSHCKD  , TSHCKL, TSLV  , TREF  
      REAL    WAG   , WAGT  , WGRMX   , WLV   , WLVG  , WLVGI , WLVGIT
      REAL    WLVD  , WRR   , WRR14   , WRT   , WRTI  , WST   , WSTI  
      REAL    WSO   , WSOI  , WSTS    , WSTR   
      REAL    ZRTI  , ZRTM  , ZRTTR   , ZRTMCW, ZRTMCD, RGRLMX, RGRLMN
      REAL    ASLA  , BSLA  , CSLA    , DSLA  , SLAMAX
      REAL    COLDMIN,COLDEAD
      CHARACTER (10) SWISLA

      REAL CHECKTB, TSTCHK 

!     Used functions
      REAL    LINT2, INSW, NOTNUL, GETOBS, INTGRL, INTGR2
      SAVE
 
!===================================================================*
!     INITIALIZATION SECTION                                        *
!===================================================================*
      IF (ITASK.EQ.1) THEN

      WRITE (*,*) 'SUCCESS'

!--------Open experimental data input file
         CALL RDINIT (IUNITD, IUNITL, FILEIT)
!        Read initial states
         CALL RDSREA('LAPE  ',LAPE )
         CALL RDSREA('DVSI  ',DVSI )
         CALL RDSREA('WLVGI ',WLVGI)
         CALL RDSREA('WRTI  ',WRTI )
         CALL RDSREA('WSOI  ',WSOI )
         CALL RDSREA('WSTI  ',WSTI )
         CALL RDSREA('ZRTI  ',ZRTI )
         CALL RDSREA('ZRTTR ',ZRTTR)
!        Read management parameters
         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL RDSREA('NH    ',NH   )
            CALL RDSREA('NPLH  ',NPLH )
            CALL RDSREA('NPLSB ',NPLSB)
         ELSE IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL RDSREA('NPLDS ',NPLDS)
         END IF
         CALL RDSREA('TMPSB ',TMPSB)
         CALL RDAREA('TMCTB ',TMCTB,IMX,ILTMCT)
!--------Close experimental data input file
         CLOSE (IUNITD)

!--------Open crop input file
         CALL RDINIT(IUNITD,IUNITL,FILEI1)
!        Read model parameters
         CALL RDSREA('FRPAR ',FRPAR )
         CALL RDSREA('CO2   ',CO2   )
         CALL RDSREA('CO2REF',CO2REF)
         CALL RDSREA('CRGLV ',CRGLV )
         CALL RDSREA('CRGRT ',CRGRT )
         CALL RDSREA('CRGSO ',CRGSO )
         CALL RDSREA('CRGST ',CRGST )
         CALL RDSREA('CRGSTR',CRGSTR)
         CALL RDSREA('DVRI  ',DVRI  )
         CALL RDSREA('DVRJ  ',DVRJ  )
         CALL RDSREA('DVRP  ',DVRP  )
         CALL RDSREA('DVRR  ',DVRR  )
         CALL RDSREA('FCLV  ',FCLV  )
         CALL RDSREA('FCRT  ',FCRT  )
         CALL RDSREA('FCSO  ',FCSO  )
         CALL RDSREA('FCST  ',FCST  )
         CALL RDSREA('FCSTR ',FCSTR )
         CALL RDSREA('FSTR  ',FSTR  )
         CALL RDSREA('LRSTR ',LRSTR )
         CALL RDSREA('MAINLV',MAINLV)
         CALL RDSREA('MAINRT',MAINRT)
         CALL RDSREA('MAINSO',MAINSO)
         CALL RDSREA('MAINST',MAINST)
         CALL RDSREA('MOPP  ',MOPP  )
         CALL RDSREA('PPSE  ',PPSE  )
         CALL RDSREA('Q10   ',Q10   )
         CALL RDSREA('RGRLMX',RGRLMX)
         CALL RDSREA('RGRLMN',RGRLMN)
         CALL RDSREA('SCP   ',SCP   )
         CALL RDSREA('SHCKD ',SHCKD )
         CALL RDSREA('SHCKL ',SHCKL )
         CALL RDSREA('SPGF  ',SPGF  )
         CALL RDSREA('TBD   ',TBD   )
         CALL RDSREA('TBLV  ',TBLV  )
         CALL RDSREA('TCLSTR',TCLSTR)
         CALL RDSREA('TMD   ',TMD   )
         CALL RDSREA('TOD   ',TOD   )
         CALL RDSREA('COLDMIN',COLDMIN)
         CALL RDSREA('COLDEAD',COLDEAD)
         CALL RDSREA('TREF  ',TREF  )
         CALL RDSREA('WGRMX ',WGRMX )
         CALL RDSREA('ZRTMCW',ZRTMCW)
         CALL RDSREA('ZRTMCD',ZRTMCD)
         CALL RDSREA('GZRT  ',GZRT  )
!        Read tables
         CALL RDAREA('REDFTT',REDFTT,IMX,ILREDF)
         CALL RDAREA('EFFTB ',EFFTB ,IMX,ILEFFT)
         CALL RDAREA('KDFTB ',KDFTB ,IMX,ILKDFT)
         CALL RDAREA('KNFTB ',KNFTB ,IMX,ILKNFT)
         CALL RDAREA('FSHTB ',FSHTB ,IMX,ILFSHT)
         CALL RDAREA('FLVTB ',FLVTB ,IMX,ILFLVT)
         CALL RDAREA('FSTTB ',FSTTB ,IMX,ILFSTT)
         CALL RDAREA('FSOTB ',FSOTB ,IMX,ILFSOT)
         CALL RDAREA('DRLVT ',DRLVT ,IMX,ILDRLV)
         CALL RDAREA('SSGATB',SSGATB,IMX,ILSSGA)
         CALL RDSCHA('SWISLA',SWISLA)
         CALL UPPERC (SWISLA)
         IF (SWISLA .EQ. 'TABLE') THEN
             CALL RDAREA('SLATB ',SLATB ,IMX,ILSLAT)
         ELSE IF (SWISLA .EQ. 'FUNCTION') THEN
            CALL RDSREA('ASLA',ASLA)
            CALL RDSREA('BSLA',BSLA)
            CALL RDSREA('CSLA',CSLA)
            CALL RDSREA('DSLA',DSLA)
            CALL RDSREA('SLAMAX',SLAMAX)
         ELSE 
            CALL FATALERR ('Crop data file','Unknown name for SWISLA')
         END IF

! (BB: MAY 2003): Check on input data validity
         CHECKTB = 0. 
         DO TSTCHK = 0.,2.,0.1
            CHECKTB = LINT2('FLVTB',FLVTB,ILFLVT,TSTCHK) + &
                      LINT2('FSTTB',FSTTB,ILFSTT,TSTCHK) + &
                      LINT2('FSOTB',FSOTB,ILFSOT,TSTCHK)
            IF (CHECKTB .GT. 1.01 .OR. CHECKTB .LE. 0.99) THEN
               CALL FATALERR ('Crop data file','FLV, FST and FSO do not add up to 1.')
            END IF
         END DO

!--------Close crop data input file
         CLOSE (IUNITD)

!--------Initialize state variables
         DVS    = 0.
         PARCUM = 0.
         PARCM1 = 0.
         WLVG   = 0.01
         WLVD   = 0.
         WSTS   = 0.01
         WSTR   = 0.
         WSO    = 0.
         WRT    = 0.
         WST    = WSTS + WSTR
         WLV    = WLVG + WLVD
         WAG    = WLVG + WST  + WSO
         WAGT   = WLV  + WST  + WSO
         TDRW   = WLV  + WST  + WSO + WRT
         WRR    = 0.
         WRR14  = 0.
         PWRR   = 0.
         NGR    = 0.
         NSP    = 0.
         DAE    = 0.
         TS     = 0.
         TMAXC  = 0.
         TMINC  = 0.
         TSLV   = 0.
         TNASS  = 0.
         WLVGIT = 0.
         LAI    = 0.
         LAIROL = 0.
         DVR    = 0.
         TSHCKD = 0.
         TSHCKL = 0.
         NSPM2  = 0.
         NGRM2  = 0.
         HU     = 0.
         HULV   = 0.
         NCOLD  = 0.
         ZRT    = 0.
         DLDRT  = 0.
         DLDR   = 0.
         KEEP   = 0.
         DLEAF    = .FALSE.
         DROUT    = .FALSE.
         GRAINS   = .FALSE.

!===================================================================*
!     RATE CALCULATION SECTION                                      *
!===================================================================*
      ELSE IF (ITASK.EQ.2) THEN

!--------Re-initialize weights and LAI at day of emergence
         IF (CROPSTA .EQ. 1) THEN
            DVS  = DVSI
            WLVG = WLVGI
            WLVD = 0.
            WSTS = WSTI
            WSTR = 0.
            WST  = WSTS+WSTR
            WSO  = WSOI
            WRT  = WRTI
            ZRT  = ZRTI
            IF (ESTAB.EQ.'TRANSPLANT' ) LAI= LAPE * NPLSB
            IF (ESTAB.EQ.'DIRECT-SEED') LAI= LAPE * NPLDS
         END IF

!--------Re-initialize rooting depth at day of transplanting
         IF (CROPSTA .EQ. 3) THEN
            ZRT = ZRTTR
         END IF

!=======SKIP ALL RATE CALCULATIONS BEFORE EMERGENCE
        IF (CROPSTA .GE. 1) THEN
!----------Set DROUT when leaf expansion is reduced in the
!          vegetative growth phase (=> extra root growth)
           IF ((DVS.LT. 1).AND.(LESTRS.LT.1.)) THEN
              DROUT = .TRUE.
           ELSE
              DROUT = .FALSE.
           END IF

!----------Computation of weather variables
           IF (CROPSTA .LE. 2) THEN
              TMPCOV = TMPSB
           ELSE
              TMPCOV = 0.
           END IF
           TCOR = LINT2('TMCTB',TMCTB,ILTMCT,DOY)
           TMAX = TMMX+TCOR+TMPCOV
           TMIN = TMMN+TCOR
           TAV  = (TMIN+TMAX)/2.
           TAVD = (TMAX+TAV )/2.
           DTR  = RDD

!----------Counter for days after emergence
           RDAE = 1.
 
!----------Phenological development 
           CALL SUBDD (TMAX,TMIN,TBD,TOD,TMD,HU)
           CALL SUBCD2 (COLDMIN,CROPSTA,TAV,TIME,NCOLD)
           CALL PHENOL(ESTAB,DVS,DVRJ,DVRI,DVRP,DVRR,HU,DAYL,MOPP,PPSE, &
                        TS,SHCKD,CROPSTA,DVR,TSHCKD)

!----------Effect of drought stress on development rate
           IF (DVS.LT.1.0) THEN
! BB: REMOVE THIS; IT CAN TAKE MORE THAN 1 YEAR TO COMPLETE A CROP CYCLE!!
!              DVEW = LESTRS + (DVS*(1.-LESTRS))
              DVEW = 1.
           ELSE IF (DVS.GE.1.) THEN
              DVEW = 1.
           END IF
           DVR = DVR*DVEW

!----------CO2 concentration
           CO2EFF = (1.-EXP(-0.00305*CO2   -0.222))  &
                   /(1.-EXP(-0.00305*CO2REF-0.222))
           EFF = LINT2('EFFTB',EFFTB,ILEFFT,TAVD)*CO2EFF
 
!----------Leaf rolling under drought stress (only for photosynthesis)
           LAIROL = LAI*(0.5*LRSTRS+0.5)

!--------- Add specific stem area to leaf area
           SSGA = LINT2('SSGATB',SSGATB,ILSSGA,DVS)
           SAI  = SSGA*WST
           ALAI   = LAIROL+0.5*SAI

!----------Intercepted solar radiation
           KDF   = LINT2('KDFTB' ,KDFTB,ILKDFT,DVS)
           REDFT = LINT2('REDFTT',REDFTT,ILREDF,TAVD)
           KNF   = LINT2('KNFTB' ,KNFTB,ILKNFT,DVS)

!----------Daily gross canopy CO2 assimilation (DTGA)
           CALL GPPARSET (CO2, KNF, NFLV, REDFT)
!----------The value 2 in next argument list: accuracy for Gauss integration over canopy. 
!          If value=1 => 3-points Gauss over canopy (as in TOTASP); of value = 2 => 
!          enhanced accuracy if required as detrmined within the subroutine TRY!
           CALL SGPCDT   (1, IDOY , LAT   , DTR  , FRPAR, &
                          SCP, AMAX , EFF   , KDF, ALAI , &
                          DAYL, DAYLP, DTGA, RAPCDT)
           PARI1 = RAPCDT/1.E6
           DPARI = RAPCDT/1.E6
           DPAR  = FRPAR*DTR/1.E6

!----------Unrolling of ALAI again 
           ALAI  = LAI+0.5*SAI

!----------Effect of drought stress on DTGA
           DTGA  = DTGA*PCEW

!----------Relative growth rates of shoots and roots
!          Effect of drought stress on shoot-root partitioning 
!BB: Changed according to SUCROS2
           FSH = LINT2('FSHTB',FSHTB,ILFSHT,DVS)
           IF (DVS.LT.1.) THEN
              FSH  = (FSH*CPEW)/NOTNUL((1.+(CPEW-1.)*FSH))
           END IF
           FRT = 1.-FSH

!           IF (DVS.LT.1.) THEN
!              FSH = FSH * LESTRS
!              FRT = 1.-FSH
!           END IF

!----------Relative growth rates of shoot organs
           FLV = LINT2('FLVTB',FLVTB,ILFLVT,DVS)
           FST = LINT2('FSTTB',FSTTB,ILFSTT,DVS)
           FSO = LINT2('FSOTB',FSOTB,ILFSOT,DVS)
!----------Check sink limitation based on yesterday's growth rates
!          and adapt partitioning stem-storage organ accordingly
           IF (GRAINS) THEN
              IF (GGR.GE.(PWRR-WRR)) THEN
                 FSO = MAX(0.,(PWRR-WRR)/NOTNUL((GCR*FSH)))
                 FST = 1.-FSO-FLV
              END IF
           END IF

!----------Loss rates of green leaves and stem reserves
           LLV  = NSLLV*WLVG*LINT2('DRLVT',DRLVT,ILDRLV,DVS)
           LSTR = INSW(DVS-1.,0.,WSTR/TCLSTR)
 
!----------Maintenance requirements
           TEFF = Q10**((TAV-TREF)/10.)
           MNDVS = WLVG/NOTNUL(WLVG+WLVD)
           RMCR  = (WLVG*MAINLV+WST*MAINST+WSO*MAINSO+WRT*MAINRT) &
                   *TEFF*MNDVS

!----------Carbohydrate requirement for dry matter production (growth respiration)
           CRGCR = FSH*(CRGLV*FLV+CRGST*FST*(1.-FSTR)+CRGSTR*FSTR*FST+ &
                       CRGSO*FSO)+CRGRT*FRT 

!----------Gross and net growth rate of crop (GCR, NGCR)
           GCR   =((DTGA*30./44.)-RMCR+(LSTR*LRSTR*FCSTR*30./12.))/NOTNUL(CRGCR)
           NGCR  = MAX(0.,GCR-LSTR*LRSTR*FCSTR*30./12.)
 
!----------Set transplanting effect
           IF (CROPSTA .EQ. 3) THEN
             PLTR = NPLH*NH/NPLSB
           ELSE
             PLTR = 1.
           END IF

!----------Growth rates of crop organs at transplanting
           RWLVG1 = (WLVG*(1.-PLTR))/DELT
           GST1   = (WSTS*(1.-PLTR))/DELT
           RWSTR1 = (WSTR*(1.-PLTR))/DELT
           GRT1   = (WRT *(1.-PLTR))/DELT
 
!----------Growth rates of crop organs
           GRT    = GCR*FRT-GRT1
           GLV    = GCR*FSH*FLV-RWLVG1
           RWLVG  = GLV-LLV
           GST    = GCR*FSH*FST*(1.-FSTR)-GST1
           GSTR   = GCR*FSH*FST*FSTR-RWSTR1
           RWSTR  = GSTR-LSTR
           GSO    = GCR*FSH*FSO
           IF (DVS.GT.0.95) THEN
              GGR = GSO
           ELSE 
              GGR = 0.
           END IF

!----------Growth rate of number of spikelets and grains
           CALL SUBGRN (GCR,CROPSTA,LRSTRS,DVS,SF2,SF1,SPGF,TAV,TMAX, &
                        NSP,GNSP,GNGR,SPFERT,GRAINS)

!--------- Leaf area growth (after calculation on leaf growth and loss rates!)

!----------Temperature sum for leaf development
           CALL SUBDD (TMAX,TMIN,TBLV,30.,42.,HULV)

!----------Specific leaf area
           IF (SWISLA .EQ. 'TABLE') THEN
              SLA  = LINT2('SLATB',SLATB,ILSLAT,DVS)
           ELSE
           SLA = ASLA + BSLA*EXP(CSLA*(DVS-DSLA))
           SLA = MIN(SLAMAX, SLA)
           END IF

! BB: NEW LAI ROUTINE
!----------Leaf area index growth
           CALL SUBLAI3(CROPSTA,RGRLMX,RGRLMN,TSLV,HULV, &
                          SHCKL,LESTRS,RNSTRS,SLA,NH,NPLH,NPLSB,DVS,LAI, &
                          ESTAB,RWLVG,DLDR,WLVG,GLAI,RGRL)

!----------Leaf death as caused by drought stress
           DLDR = 0.
           IF (LDSTRS.EQ.1.) THEN
               DLEAF = .FALSE.
               DLDRT = 0.
           END IF
           IF ((LDSTRS.LT.1.).AND.(.NOT.DLEAF)) THEN
              WLVGIT = WLVG
              DLEAF  = .TRUE.
              KEEP   = LDSTRS
           END IF
           IF (DLEAF) THEN
              IF (LDSTRS.LE.KEEP) THEN
                 DLDR  = (WLVGIT/DELT)*(1.-LDSTRS)-DLDRT/DELT
                 KEEP  = LDSTRS
                 DLDRT = DLDR*DELT+DLDRT
              END IF
           END IF

!----------Growth respiration of the crop (RGCR)
           CO2RT  = 44./12.*(CRGRT *12./30.-FCRT )
           CO2LV  = 44./12.*(CRGLV *12./30.-FCLV )
           CO2ST  = 44./12.*(CRGST *12./30.-FCST )
           CO2STR = 44./12.*(CRGSTR*12./30.-FCSTR)
           CO2SO  = 44./12.*(CRGSO *12./30.-FCSO )
 
           RGCR = (GRT+GRT1)*CO2RT + (GLV+RWLVG1)*CO2LV +  &
                  (GST+GST1)*CO2ST + GSO*CO2SO+(GSTR+RWSTR1)*CO2STR+ &
                  (1.-LRSTR)*LSTR*FCSTR*44./12.
 
           CTRANS = RWLVG1*FCLV+GST1*FCST+RWSTR1*FCSTR+GRT1*FCRT
           RTNASS = ((DTGA*30./44.-RMCR)*44./30.)-RGCR-(CTRANS*44./12.)

!----------Carbon balance check
           CKCIN  = (WLVG+WLVD-WLVGI)*FCLV+(WSTS-WSTI)*FCST+WSTR*FCSTR &
                          +(WRT-WRTI)*FCRT+WSO*FCSO
           CKCFL  = TNASS*(12./44.)
 
           CALL SUBCBC(CKCIN,CKCFL,TIME,CBCHK,TERMNL)
 
!----------Output section
           IF (OUTPUT) THEN
            CALL OUTDAT(2,0,'DVS   ',DVS)
            CALL ChartOutputRealScalar('DVS', DVS)
            CALL OUTDAT(2,0,'RDD   ',RDD)
            CALL ChartOutputRealScalar('RDD', RDD)
            CALL OUTDAT(2,0,'TMIN   ',TMIN)
            CALL ChartOutputRealScalar('TMIN', TMIN)
            CALL OUTDAT(2,0,'TMAX   ',TMAX)
            CALL ChartOutputRealScalar('TMAX', TMAX)
!NEW
            CALL OUTDAT(2,0,'DTR   ',DTR)
            CALL ChartOutputRealScalar('DTR', DTR)
            CALL OUTDAT(2,0,'RAPCDT   ',RAPCDT)
            CALL ChartOutputRealScalar('RAPCDT', RAPCDT)
            CALL OUTDAT(2,0,'PARCUM   ',PARCUM)
            CALL ChartOutputRealScalar('PARCUM', PARCUM)
!            CALL OUTDAT(2,0,'PAR1M   ',PAR1M)
!            CALL ChartOutputRealScalar('PAR1M', PAR1M)
! END NEW
            CALL OUTDAT(2,0,'NFLV  ',NFLV)
            CALL ChartOutputRealScalar('NFLV', NFLV)
            CALL OUTDAT(2,0,'SLA   ',SLA)
            CALL ChartOutputRealScalar('SLA', SLA)
            CALL OUTDAT(2,0,'SLASIM',LAI/NOTNUL(WLVG))
            CALL ChartOutputRealScalar('SLASIM', LAI/NOTNUL(WLVG))
            CALL OUTDAT(2,0,'LESTRS ',LESTRS)
            CALL ChartOutputRealScalar('LESTRS', LESTRS)
            CALL OUTDAT(2,0,'LRSTRS ',LRSTRS)
            CALL ChartOutputRealScalar('LRSTRS', LRSTRS)
            CALL OUTDAT(2,0,'PCEW  ',PCEW)
            CALL ChartOutputRealScalar('PCEW', PCEW)
            CALL OUTDAT(2,0,'NSP  ',NSP)
            CALL ChartOutputRealScalar('NSP', NSP)
            CALL OUTDAT(2,0,'LAI   ',LAI  )
            CALL ChartOutputRealScalar('LAI', LAI)
            CALL OUTDAT(2,0,'WAGT  ',WAGT  )
            CALL ChartOutputRealScalar('WAGT', WAGT)
            CALL OUTDAT(2,0,'WST   ',WST)
            CALL ChartOutputRealScalar('WST', WST)
            CALL OUTDAT(2,0,'WLVG  ',WLVG) 
            CALL ChartOutputRealScalar('WLVG', WLVG)
            CALL OUTDAT(2,0,'WLVD  ',WLVD)
            CALL ChartOutputRealScalar('WLVD', WLVD)
            CALL OUTDAT(2,0,'WLV   ',WLV)
            CALL ChartOutputRealScalar('WLV', WLV)
            CALL OUTDAT(2,0,'WSO   ',WSO)
            CALL ChartOutputRealScalar('WSO', WSO)
            CALL OUTDAT (2,0,'WRR14 ',WRR14 )
            CALL ChartOutputRealScalar('WRR14', WRR14)
            CALL OUTDAT(2,0,'ZRT',ZRT)
            CALL ChartOutputRealScalar('ZRT', ZRT)

            IF (INQOBS (FILEIT,'NFLV')) THEN
                    CALL OUTDAT (2, 0, 'NFLV_OBS',GETOBS(FILEIT,'NFLV'))
                    CALL ChartOutputRealScalar('NFLV_OBS',GETOBS(FILEIT,'NFLV'))
            ENDIF
            IF (INQOBS (FILEIT,'FNLV')) THEN
                      CALL OUTDAT (2, 0, 'FNLV_OBS',GETOBS(FILEIT,'FNLV'))
                    CALL ChartOutputRealScalar('FNLV_OBS',GETOBS(FILEIT,'FNLV'))
            ENDIF
            IF (INQOBS (FILEIT,'FNST')) THEN
                    CALL OUTDAT (2, 0, 'FNST_OBS',GETOBS(FILEIT,'FNST'))
                    CALL ChartOutputRealScalar('FNST_OBS',GETOBS(FILEIT,'FNST'))
            ENDIF
            IF (INQOBS (FILEIT,'FNSO')) THEN
                    CALL OUTDAT (2, 0, 'FNSO_OBS',GETOBS(FILEIT,'FNSO'))
                    CALL ChartOutputRealScalar('FNSO_OBS',GETOBS(FILEIT,'FNSO'))
            ENDIF
            IF (INQOBS (FILEIT,'LAI')) THEN
                    CALL OUTDAT (2, 0, 'LAI_OBS',GETOBS(FILEIT,'LAI'))
                    CALL ChartOutputRealScalar('LAI_OBS',GETOBS(FILEIT,'LAI'))
            ENDIF
            IF (INQOBS (FILEIT,'WLVG')) THEN
                    CALL OUTDAT (2, 0, 'WLVG_OBS',GETOBS(FILEIT,'WLVG'))
                    CALL ChartOutputRealScalar('WLVG_OBS',GETOBS(FILEIT,'WLVG'))
            ENDIF
            IF (INQOBS (FILEIT,'WLVD')) THEN
                    CALL OUTDAT (2, 0, 'WLVD_OBS',GETOBS(FILEIT,'WLVD'))
                    CALL ChartOutputRealScalar('WLVD_OBS',GETOBS(FILEIT,'WLVD'))
            ENDIF
            IF (INQOBS (FILEIT,'WLV')) THEN
                    CALL OUTDAT (2, 0, 'WLV_OBS',GETOBS(FILEIT,'WLV'))
                    CALL ChartOutputRealScalar('WLV_OBS',GETOBS(FILEIT,'WLV'))
            ENDIF
            IF (INQOBS (FILEIT,'WST')) THEN
                    CALL OUTDAT (2, 0, 'WST_OBS',GETOBS(FILEIT,'WST'))
                    CALL ChartOutputRealScalar('WST_OBS',GETOBS(FILEIT,'WST'))
            ENDIF
            IF (INQOBS (FILEIT,'WSO')) THEN
                    CALL OUTDAT (2, 0, 'WSO_OBS',GETOBS(FILEIT,'WSO'))
                    CALL ChartOutputRealScalar('WSO_OBS',GETOBS(FILEIT,'WSO'))
            ENDIF
            IF (INQOBS (FILEIT,'WAGT')) THEN
                    CALL OUTDAT (2, 0, 'WAGT_OBS',GETOBS(FILEIT,'WAGT'))
                    CALL ChartOutputRealScalar('WAGT_OBS',GETOBS(FILEIT,'WAGT'))
            END IF

           END IF

!=======SET EXPORTED VARIABLES FOR SOIL BALANCE AT 0 BEFORE EMERGENCE
        ELSE IF (CROPSTA .EQ. 0) THEN
           LAI    = 0.
           ALAI   = 0.
           LAIROL = 0
        END IF
!=======END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE

!===========Checks on simulation run
!-----------If biomass is negative: set at 0 and abort simulation
            IF (WSO.LT.-5..OR.WLVG.LT.-5..OR.WST.LT.-5..OR.WRR.LT.-5) THEN
               WRITE (*,*) 'Negative biomass=> simulation stopped'
               CALL OUTCOM('Negative biomass => simulation stopped')
! BAS: removed, Spet 2006  IF (WSO.LT.0.) WSO = 0.
!               IF (WRR.LT.0.) WRR = 0.
!               IF (WST.LT.0.) WST = 0.
!               IF (WLVG.LT.0.) WLVG = 0.
               TERMNL = .TRUE.
            END IF

!-----------If LAI is negative: set at 0 and abort simulation
            IF (LAI.LT.-0.01) THEN
               WRITE (*,*) 'Negative LAI=> simulation stopped'
               CALL OUTCOM('Negative LAI => simulation stopped')
!               IF (LAI.LT.0.) LAI = 0.
               TERMNL = .TRUE.
            END IF

!-----------The following only in main field
            IF (CROPSTA .GE. 4) THEN
!           Check if lower limit dead leaves is reached
               IF (LDSTRS.LE.0.) THEN
                  WRITE (*,*) 'Soil dryer than lower limit dead leaves'
                  WRITE (*,*) 'LDSTRS = 0 => Simulation stopped'
                  CALL OUTCOM('LDSTRS = 0 => simulation stopped')
                  TERMNL = .TRUE.
               END IF
!-----------End if only in main field
            END IF
 
!===================================================================*
!     INTEGRATION SECTION                                           *
!===================================================================*
      ELSE IF (ITASK.EQ.3) THEN

!=======SKIP WHOLE STATE UPDATE BEFORE EMERGENCE
         IF (CROPSTA .GE. 1) THEN

!-----------Integrate rate variables
            PARCUM = INTGRL(PARCUM,DPARI,DELT)
            PARCM1 = INTGRL(PARCM1,PARI1,DELT)
            TS     = INTGRL(TS    ,HU   ,DELT)
            TSLV   = INTGRL(TSLV  ,HULV ,DELT)
            TMAXC  = INTGRL(TMAXC ,TMAX ,DELT)
            TMINC  = INTGRL(TMINC ,TMIN ,DELT)
            DVS    = INTGRL(DVS   ,DVR  ,DELT)
            WLVG   = INTGRL(WLVG  ,RWLVG-DLDR,DELT)
            WLVD   = INTGRL(WLVD  ,LLV+DLDR  ,DELT)
            WSTS   = INTGRL(WSTS  ,GST  ,DELT)
            WSTR   = INTGRL(WSTR  ,RWSTR,DELT)
            WSO    = INTGRL(WSO   ,GSO  ,DELT)
            WRT    = INTGRL(WRT,GRT,DELT)
            WRR    = INTGRL(WRR,GGR,DELT)
            NGR    = INTGRL(NGR,GNGR,DELT)
            NSP    = INTGRL(NSP,GNSP,DELT)
            DAE    = INTGRL(DAE,RDAE,DELT)
            TNASS  = INTGRL(TNASS,RTNASS,DELT)

!-----------Calculate sums of states
            WST    = WSTS + WSTR
            WLV    = WLVG + WLVD
            WAG    = WLVG + WST  + WSO
            WAGT   = WLV  + WST  + WSO
            TDRW   = WLV  + WST  + WSO + WRT

            PWRR   = NGR*WGRMX
            NGRM2  = NGR/10000.
            NSPM2  = NSP/10000.

!-----------Weight rough rice with 14% moisture
            WRR14  = (WRR/0.86)

!-----------Leaf area index and total area index (leaves + stems)
            LAI    = INTGR2(LAI, GLAI, DELT, FILEIT, 'LAI')
            ALAI   = LAI+0.5*SAI

!-----------Root length
            IF ((.NOT.DROUT).AND.(ZRT.LE.ZRTMCW)) THEN
               ZRTM = MIN(ZRTMCW,ZRTMS,TKLT)
            ELSE IF ((.NOT.DROUT).AND.(ZRT.GT.ZRTMCW)) THEN
               ZRTM = MIN(ZRT,ZRTMS,TKLT)
            ELSE IF (DROUT) THEN
               ZRTM = MIN(ZRTMCD,ZRTMS,TKLT)
            END IF
            ZRT    = INTGRL(ZRT,GZRT,DELT)
            ZRT    = MIN(ZRT,ZRTM)

!-----------Terminate simulation settings
            IF (DVS.GT.2.0) THEN
               TERMNL = .TRUE.
               WRITE (*,*) 'Crop reached maturity'
             END IF
! BB 2006: Crop growth stops below certain lower threshold T days
            IF (NCOLD.GT.COLDEAD) THEN
               TERMNL = .TRUE.
               WRITE (*,*) 'Crop died because of low temperature'
             END IF

!========END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE
         END IF

!===================================================================*
!     TERMINAL SECTION                                              *
!===================================================================*
      ELSE IF (ITASK.EQ.4) THEN
!        Terminal calculations
!        Terminal output
         CALL OPSTOR ('WRR14', WRR14)
         CALL OPSTOR ('WSO', WSO)
         CALL OPSTOR ('WAGT', WAGT)
         CALL OPSTOR ('PARCUM', PARCUM)
         CALL OPSTOR ('TS', TS)
         CALL OPSTOR ('TMAXC', TMAXC)
         CALL OPSTOR ('TMINC', TMINC)
         CALL OPSTOR ('TAVERC', (TMAXC+TMINC)/2.)
      END IF

      RETURN
      END
