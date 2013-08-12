*----------------------------------------------------------------------*
*  SUBROUTINE LOWBAL                                                   *
*                                                                      *
*  Version: 3.0                                                        *
*  Date   : June 2003                                                  *
*                                                                      *
*  Purpose: simple water balance for puddled rice soils in irrigated   *
!  and rainfed lowland environments.                                   *
!                                                                      *
!                                                                      *
!  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!  name   type meaning                                    units  class *
!  ----   ---- -------                                    -----  ----- *
! ITASK   I4  Task that subroutine should perform            -   C,IN  *
! IUNITD  I4  Unit that can be used for input files          -   C,IN  *
! IUNITO  I4  Unit used for output file                      -   C,IN  *
! IUNITL  I4  Unit used for log  file                        -   C,IN  *
! FILEI2  C*  Name of input file no. 2                       -   C,IN  *
! OUTPUT  L4  Flag to indicate if output should be done      -   C,IN  *
! TERMNL  L4  Flag to indicate if simulation is to stop      -     I/O *
! DELT    R   Time step of integration                       d      T  *
! SWIWLP  I   Switch for production environment              -      C  *
! ITIM    I   Time of simulation                             d      T  *
! ITRT    I   Time of transplanting                          d      T  *
! DVS     R   Development stage                              -      I  *
! TRWL    R   Array of actual transpiration per layer       mm/d    I  *
! EVSC    R   Potential soil evaporation rate               mm/d    I  *
! RAIN    R   Rainfall rate                                 mm/d    I  *
! RAINN   R   Rainfall rate next day                        mm/d    I  *
! NL      I   Number of soil layers                          -      O  *
! TKL     R   Array of layer thicknesses                     m      O  *
! TKLT    R   Total soil thickness                           m      O  *
! ZRTMS   R   Maximum rooting set by puddled layer           m      O  *
! WCWP    R   Array of water content wilting point/layer  cm3/cm3   O  *
! WCFC    R   Array of water content field capacity/layer cm3/cm3   O  *
! WCLQT   R   Array of actual water content/layer         cm3/cm3   O  *
! WL0     R   Amount of ponded water                         mm     O  *
!                                                                      *
!  Subroutines called: From library TTUTIL: OUTCOM, CHKTSK,            *
!                     RDSREA, RDSINT, RDSCHA, OUTDAT, OPSTOR           *
!  Data files needed: soil definition file FILEI2 (as specified in     *
!                     in the file CONTROL.DAT)                         *
!----------------------------------------------------------------------*
      SUBROUTINE LOWBAL (ITASK, IUNITD, IUNITO,
     &    FILEI2, OUTPUT, DELT, TIME, ESTAB, CROPSTA, IR,
     &    TRWL, EVSC, RAIN, NL, TKL, TKLT, ZRTMS, 
     &    WCWP, WCFC, WCST, WCLQT, WL0, MSKPA)

      USE CHART

      IMPLICIT NONE

      INTEGER  ITASK, IUNITD, IUNITO
      INTEGER  NL, CROPSTA
      REAL     DELT, EVSC, RAIN, TKLT, ZRTMS, IR
      REAL     WL0, WL0D 
      REAL     RAINCUM1,IRCUM1,RUNOFCUM1,EVSWCUM1,TRWCUM1,PERCCUM1
      REAL     SSOUTCUM1,SSINCUM1   
      REAL     RAINCUM2,IRCUM2,RUNOFCUM2,EVSWCUM2,TRWCUM2,PERCCUM2
      REAL     SSOUTCUM2,SSINCUM2
      REAL     RAINCUM3,IRCUM3,RUNOFCUM3,EVSWCUM3,TRWCUM3,PERCCUM3
      REAL     SSOUTCUM3,SSINCUM3
      REAL     EVSH, EVSW, EVSD, RUNOF, FIXPERC  
      REAL     TRWP, PERC, PSOIL, WL0MX
      REAL     DSLR, RDSLR, WL0MXI, TKLPI, DDR, WL0I, SHRINK, WCCRAC
      REAL     WCSTP, WCFCP, WCWPP, WCADP, TKLP, TKLPM, WCLP, WLP, WLPI
      REAL     WCAD(1), WCWP(1), WCFC(1), WCST(1), WCLQT(1)
      REAL     TKL(1), TRWL(1), MSKPA(1), WCL(1), MSUC(1)
      REAL     INTGRL, INSW, FACT
      REAL     CHECK, WL0CHK, WLPCHK

! Bouman: to separate seepage from percolation
! SSOILIN = fixed seepage inflow; SSOILOUT = fixed seepage outflow
! SS = dynamic seepage outflow (only if standing water)
      REAL  SSOILIN, SSOILOUT, SS 

! Bouman: Percolation rate can vary in time
      INTEGER PSWITCH, IMX, IPTABLE
      PARAMETER (IMX=20)
      REAL TIME
      REAL PTABLE(IMX)

      LOGICAL  OUTPUT, CRACK
      CHARACTER*(*)  FILEI2
      CHARACTER (6) SCODE
      CHARACTER (*) ESTAB
      LOGICAL INQOBS
      REAL GETOBS, INTGR2, LINT2

*-----Standard local variables

      SAVE

*============================================================*
*------Initialization section
*============================================================*

      IF (ITASK .EQ. 1) THEN

       CRACK = .FALSE.

       CALL RDINIT (IUNITD, IUNITO, FILEI2)
       CALL RDSCHA ('SCODE', SCODE)
       CALL RDSREA ('WL0MXI', WL0MXI)
       CALL RDSREA ('TKLPI', TKLPI)
       CALL RDSINT ('PSWITCH', PSWITCH)

       IF (PSWITCH .EQ. 1) THEN
          CALL RDSREA ('FIXPERC', FIXPERC)
       ELSE IF (PSWITCH .EQ. 2) THEN
         CALL RDAREA('PTABLE',PTABLE,IMX,IPTABLE)
       ELSE
         CALL FATALERR ('Soil data file','Wrong value for PSWITCH')
       END IF

       CALL RDSREA ('SSOILIN', SSOILIN)
       CALL RDSREA ('SSOILOUT', SSOILOUT)
       CALL RDSREA ('DDR', DDR)
       CALL RDSREA ('WL0I', WL0I)
       CALL RDSREA ('SHRINK', SHRINK)
       CALL RDSREA ('WCCRAC', WCCRAC)
       CALL RDSREA ('WCSTP', WCSTP)
       CALL RDSREA ('WCFCP', WCFCP)
       CALL RDSREA ('WCWPP', WCWPP)
       CALL RDSREA ('WCADP', WCADP)
       CLOSE (IUNITD, STATUS='DELETE')

!------Initializing state variables
       WL0 = WL0I
       WL0MX = WL0MXI
       TKLP = TKLPI
       TKLPM = TKLPI*SHRINK
       WLPI = (WCSTP*TKLPM)+(TKLPI-TKLPM)
       WLP = WLPI
       WCLP = WLP/TKLP
       WL0CHK = WL0
       WLPCHK = WLP
       DSLR = 1.
       RUNOF = 0.
!       PERC = FIXPERC
       RAINCUM1  = 0.
       IRCUM1   = 0.
       RUNOFCUM1 = 0.
       EVSWCUM1  = 0.
       TRWCUM1   = 0.
       SSOUTCUM1 = 0.
       SSINCUM1 = 0.
       PERCCUM1   = 0.

       RAINCUM2  = 0.
       IRCUM2   = 0.
       RUNOFCUM2 = 0.
       EVSWCUM2  = 0.
       TRWCUM2   = 0.
       SSOUTCUM2 = 0.
       SSINCUM2 = 0.
       PERCCUM2   = 0.

       RAINCUM3  = 0.
       IRCUM3   = 0.
       RUNOFCUM3 = 0.
       EVSWCUM3  = 0.
       TRWCUM3   = 0.
       SSOUTCUM3 = 0.
       SSINCUM3 = 0.
       PERCCUM3   = 0.
 
       CHECK  = 0.

!------For communication with ORYZA subroutine
       NL = 1
       ZRTMS = TKLPI/1000.
       WCWP(1) = WCWPP
       WCFC(1) = WCFCP
       WCST(1) = WCSTP
       WCAD(1) = WCADP
       TKL(1) = TKLPI/1000.
       TKLT = TKLPI/1000.
       WCLQT(1) = WCLP

!============================================================*
!-----Rate calculation section
!============================================================*

      ELSE IF (ITASK .EQ. 2) THEN

      IF (PSWITCH .EQ. 1) THEN
        PSOIL = FIXPERC
      ELSE IF (PSWITCH .EQ. 2) THEN
        PSOIL = LINT2('PTABLE',PTABLE,IPTABLE,TIME)
      END IF

      TRWP = TRWL(1)
      WL0CHK = WL0
      WLPCHK = WLP

!------Uncracked situation
       IF (.NOT. CRACK) THEN
!--------Percolation only when ponded water is present
         IF (WL0 .LE. 0) THEN
            PERC = 0.
            SS = 0.            
         ELSE 
           IF (WL0 .GE. (PSOIL+SSOILOUT)) THEN
             PERC = PSOIL
             SS = SSOILOUT
           ELSE
             SS = MIN (SSOILOUT, WL0)
             PERC = WL0 - SS
           END IF
         END IF
       END IF

!------Reduction of EVSC when no more ponded water
       IF (WL0 .LE. 0) THEN
          EVSH = MIN(EVSC, (WLP-WCADP*TKLP)/DELT+RAIN+IR+SSOILIN)
          EVSD = MIN(EVSC, 
     $           0.6*EVSC*(SQRT(DSLR+1.)-SQRT(DSLR))+RAIN+IR+SSOILIN)
          EVSW = INSW(DSLR-1.1, EVSH, EVSD)
          RDSLR = 1.
      ELSE
          EVSW = EVSC
          RDSLR = -(DSLR-1.)/DELT
       END IF

!------Cracked situation
       IF (CRACK) THEN
         IF ((RAIN+IR+SSOILIN+WL0) .LE. DDR) THEN
            IF (WL0 .LE. 0. .AND. (RAIN+IR+SSOILIN) .EQ. 0.) THEN
              PERC = 0.
              SS = 0.            
            ELSE IF (WL0.LE.0. .AND. (RAIN+IR+SSOILIN) .GT.0.) THEN
              PERC = MAX(RAIN+IR+SSOILIN-EVSW, 0.)
              SS = 0.
             ELSE
              PERC = WL0 + RAIN+IR+SSOILIN
              SS = 0.
            END IF
         ELSE
            PERC = DDR
            SS = MIN (RAIN+IR+SSOILIN-PERC, SSOILOUT)
         END IF
       END IF

         IF (OUTPUT) THEN
            CALL OUTDAT(2,0,'CHECK',CHECK)
            CALL ChartOutputRealScalar('CHECK',CHECK)
            CALL OUTDAT(2,0,'PERC',PERC)
            CALL ChartOutputRealScalar('PERC',PERC)
            CALL OUTDAT(2,0,'RUNOF',RUNOF)
            CALL ChartOutputRealScalar('RUNOF',RUNOF)
            CALL OUTDAT(2,0,'SS',SS)
            CALL ChartOutputRealScalar('SS',SS)
            CALL OUTDAT(2,0,'IR',IR)
            CALL ChartOutputRealScalar('IR',IR)
            CALL OUTDAT(2,0,'IRCUM3',IRCUM3)
            CALL ChartOutputRealScalar('IRCUM3',IRCUM3)
            CALL OUTDAT(2,0,'RAINCUM3',RAINCUM3)
            CALL ChartOutputRealScalar('RAINCUM3',RAINCUM3)
            CALL OUTDAT(2,0,'TKLP',TKLP)
            CALL ChartOutputRealScalar('TKLP',TKLP)
            CALL OUTDAT(2,0,'WL0',WL0)
            CALL ChartOutputRealScalar('WL0',WL0)
            CALL OUTDAT(2,0,'EVSW',EVSW)
            CALL ChartOutputRealScalar('EVSW',EVSW)
            CALL OUTDAT(2,0,'EVSWCUM3',EVSWCUM3)
            CALL ChartOutputRealScalar('EVSWCUM3',EVSWCUM3)
            CALL OUTDAT(2,0,'TRWCUM3',TRWCUM3)
            CALL ChartOutputRealScalar('TRWCUM3',TRWCUM3)
            CALL OUTDAT(2,0,'MSKPA1',MSKPA(1))
            CALL ChartOutputRealScalar('MSKPA1',MSKPA(1))
            CALL OUTDAT(2,0,'WCL1',WCL(1))
            CALL ChartOutputRealScalar('WCL1',WCLP)

            IF (INQOBS (FILEI2,'WL0')) THEN
        CALL OUTDAT (2, 0, 'WL0_OBS',GETOBS(FILEI2,'WL0'))
        CALL ChartOutputRealScalar('WL0_OBS',GETOBS(FILEI2,'WL0'))
            ENDIF
            IF (INQOBS (FILEI2,'WCL1')) THEN
        CALL OUTDAT (2, 0, 'WCL1_OBS',GETOBS(FILEI2,'WCL1'))
        CALL ChartOutputRealScalar('WCL1_OBS',GETOBS(FILEI2,'WCL1'))
            ENDIF
            IF (INQOBS (FILEI2,'MSKPA1')) THEN
        CALL OUTDAT (2, 0, 'MSKPA1_OBS',GETOBS(FILEI2,'MSKPA1'))
        CALL ChartOutputRealScalar('MSKPA1_OBS',GETOBS(FILEI2,'MSKPA1'))
            ENDIF
       END IF

!============================================================*
!-----Integration section
!============================================================*

      ELSE IF (ITASK .EQ. 3) THEN

!-----Surface draingae is standard zero
      RUNOF =0.

!-----1. Situation with ponded water
       IF (WL0 .GT. 0) THEN
         WL0 = INTGRL(WL0, (RAIN+SSOILIN+IR-EVSW-TRWP-PERC-SS), DELT)
!--------1.1 bund overflow
         IF (WL0 .GT. WL0MX) THEN
           RUNOF = WL0 - WL0MX
           WL0 = WL0MX
!-------1.2 no more ponded water; soil not yet completely shrunken
         ELSE IF (WL0.LT.0 .AND. TKLP.GT.TKLPM) THEN
!---------1.2.1 further shrinkage of puddled layer
          IF (WL0 .GE. (TKLPM-TKLP)) THEN 
            WL0MX = WL0MX - WL0
            TKLP = TKLP + WL0
            WLP = (WCSTP*TKLPM)+(TKLP-TKLPM)
            WCLP = WLP/TKLP
            WL0 = 0.
!---------1.2.2 complete shrinkage of puddled layer
          ELSE IF (WL0 .LT. (TKLPM-TKLP)) THEN
            WL0MX = (WL0MXI+TKLPI)-TKLPM
            TKLP = TKLPM
            WLP = (WCSTP*TKLP)+(WL0-(TKLPM-TKLP))
            WCLP = WLP/TKLP
            WL0 = 0.
          END IF
!--------1.3 no more ponded water; soil already completely shrunken
         ELSE IF (WL0.LT.0. .AND. TKLP.EQ.TKLPM) THEN
          WLP = WLP + WL0
          WCLP = WLP/TKLP
          WL0 = 0.
         END IF

!------2. Situation with no ponded water
       ELSE IF (WL0 .LE. 0) THEN
         WLP = INTGRL (WLP, (RAIN+SSOILIN+IR-EVSW-TRWP-PERC-SS), DELT)
!--------2.1 completely shrunken puddled layer
         IF (WLP .LE. (TKLPM*WCSTP)) THEN
           TKLP = TKLPM
           WL0MX = (WL0MXI+TKLPI)-TKLP
           WCLP = WLP/TKLP
           WL0 = 0.
!--------2.2 more water than maximum in completely shrunken layer
         ELSE IF (WLP .GT. (TKLPM*WCSTP)) THEN
!----------2.2.1 formation of ponded water layer
           IF (WLP .GE. ((TKLPM*WCSTP)+(TKLP-TKLPM))) THEN
             WL0D = WLP - ((TKLPM*WCSTP)+(TKLP-TKLPM))
             IF (WL0D .GT. WL0MX) THEN
               WL0 = WL0MX
               RUNOF = WL0D - WL0MX
             ELSE
               WL0 = WL0D
             END IF
             WLP = (TKLPM*WCSTP)+(TKLP-TKLPM)
             WCLP = WLP/TKLP
!----------2.2.2 further shrinkage of puddled layer
           ELSE IF (WLP .LT. ((TKLPM*WCSTP)+(TKLP-TKLPM))) THEN 
             TKLP = WLP-(TKLPM*WCSTP)+TKLPM
             WL0MX = (WL0MXI+TKLPI)-TKLP
             WCLP = WLP/TKLP
             WL0 = 0.
           END IF
         END IF

       END IF

! Force observed water content values per layer, if available and
! selected; else integrate simulated values
         WCLP = INTGR2 (WCLP,0,DELT,FILEI2,'WCL1')

!------- Calculate moisture suction in KPa (FROM PADY)
        IF (WCLP.GE.WCFC(1)) THEN
           FACT    = MAX(0., 
     $                MIN(1.,(WCST(1)-WCLP)/(WCST(1)-WCFC(1))))
           MSUC(1) = 10.**(FACT*2.0)
           IF (WCLP.GE.WCST(1)) MSUC(1) = 0.
        ELSE IF (WCLP.GE.WCWP(1).AND.WCLP.LT.WCFC(1)) THEN 
           FACT    = MAX(0., 
     $                MIN(1.,(WCLP-WCWP(1))/(WCFC(1)-WCWP(1))))
           MSUC(1) = 10.**(4.2-FACT*2.2)
        ELSE IF (WCLP.LT.WCWP(1)) THEN
           FACT    = MAX(0., 
     $                MIN(1.,(WCLP-WCAD(1))/(WCWP(1)-WCAD(1))))
           MSUC(1) = 10.**(7.0-FACT*2.8)
        END IF
!           Note: MSKPA(1) is matrix moisture suction in kPa!
        MSKPA(1) = (MSUC(1)/10.)

! BAS, 8 SEPTEMBER 2006: 3 CUMULATIVE AMOUNTS
!---- Cumulative amounts From STTIME onwards
         IRCUM1    = INTGRL (IRCUM1, IR  , DELT)
         RAINCUM1  = INTGRL (RAINCUM1, RAIN  , DELT)
         RUNOFCUM1 = INTGRL (RUNOFCUM1, RUNOF , DELT)
         EVSWCUM1  = INTGRL (EVSWCUM1, EVSW  , DELT)
         TRWCUM1   = INTGRL (TRWCUM1 , TRWP   , DELT)
         PERCCUM1    = INTGRL (PERCCUM1, PERC, DELT)
         SSOUTCUM1 = INTGRL (SSOUTCUM1, SS, DELT)
         SSINCUM1  = INTGRL (SSINCUM1, SSOILIN, DELT)
         DSLR    = INTGRL (DSLR  , RDSLR , DELT)

!------ from emergence onward, both in direct-seeded and in transplanted systems
         IF (CROPSTA.GE.1) THEN
            IRCUM2    = INTGRL (IRCUM2, IR  , DELT)
            RAINCUM2  = INTGRL (RAINCUM2, RAIN  , DELT)
            RUNOFCUM2 = INTGRL (RUNOFCUM2, RUNOF , DELT)
            EVSWCUM2  = INTGRL (EVSWCUM2, EVSW  , DELT)
            TRWCUM2   = INTGRL (TRWCUM2 , TRWP   , DELT)
            PERCCUM2    = INTGRL (PERCCUM2, PERC, DELT)
            SSOUTCUM2    = INTGRL (SSOUTCUM2, SS, DELT)
            SSINCUM2    = INTGRL (SSINCUM2, SSOILIN, DELT)
          END IF
                
!------ From transplanting onward, only in transplanted systems
         IF (ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) THEN
            IRCUM3    = INTGRL (IRCUM3, IR  , DELT)
            RAINCUM3  = INTGRL (RAINCUM3, RAIN  , DELT)
            RUNOFCUM3 = INTGRL (RUNOFCUM3, RUNOF , DELT)
            EVSWCUM3  = INTGRL (EVSWCUM3, EVSW  , DELT)
            TRWCUM3   = INTGRL (TRWCUM3 , TRWP   , DELT)
            PERCCUM3    = INTGRL (PERCCUM3, PERC, DELT)
            SSOUTCUM3 = INTGRL (SSOUTCUM3, SS, DELT)
            SSINCUM3  = INTGRL (SSINCUM3, SSOILIN, DELT)
         END IF

!------For communication with ORYZA subroutine
       TKL(1) = TKLP/1000.
       TKLT = TKLP/1000.
       WCLQT(1) = WCLP

       IF (WCLP .LT. WCCRAC) CRACK = .TRUE.

! Water balance check: inflows-outflows - change in storage should be less than 10 mm.
       CHECK = (RAINCUM1+IRCUM1+SSINCUM1)
     $           - (EVSWCUM1+TRWCUM1+PERCCUM1+SSOUTCUM1+RUNOFCUM1)
     $           + (WL0I - WL0) + (WLPI - WLP)
         IF (ABS (CHECK).GT.10) THEN
            CALL FATALERR ('LOWBAL','error in water balance')
         END IF

!============================================================*
!------Terminal section
!============================================================*

       ELSE IF (ITASK .EQ. 4) THEN

          CLOSE (15)
          CLOSE (16)
!---------Store end-of-year data to a special file.
!         CALL OPSTOR ('RAINCUM1' , RAINCUM1 )
!         CALL OPSTOR ('IRCUM1', IRCUM1)
!         CALL OPSTOR ('RUNOFCUM1', RUNOFCUM1)
!         CALL OPSTOR ('TRWCUM1'  , TRWCUM1  )
!         CALL OPSTOR ('EVSWCUM1' , EVSWCUM1 )
!         CALL OPSTOR ('PERCCUM1', PERCCUM1)

         IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL OPSTOR ('RAINCUM2' , RAINCUM2 )
            CALL OPSTOR ('IRCUM2', IRCUM2)
            CALL OPSTOR ('RUNOFCUM2', RUNOFCUM2)
            CALL OPSTOR ('TRWCUM2'  , TRWCUM2  )
            CALL OPSTOR ('EVSWCUM2' , EVSWCUM2 )
            CALL OPSTOR ('PERCCUM2', PERCCUM2)
         END IF

         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL OPSTOR ('RAINCUM3' , RAINCUM3 )
            CALL OPSTOR ('IRCUM3', IRCUM3)
            CALL OPSTOR ('RUNOFCUM3', RUNOFCUM3)
            CALL OPSTOR ('TRWCUM3'  , TRWCUM3  )
            CALL OPSTOR ('EVSWCUM3' , EVSWCUM3 )
            CALL OPSTOR ('PERCCUM3', PERCCUM3)
!           CALL OPSTOR ('SSOUTCUM3', SSOUTCUM3)
!           CALL OPSTOR ('SSINCUM3', SSINCUM3)
          END IF

       END IF

       RETURN

       END

