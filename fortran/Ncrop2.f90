!----------------------------------------------------------------------!
! SUBROUTINE NCROP                                                     !
! Authors:                                                             !
!          Version august, 2003                                        !
! Date   : December 2001, Version: 1                                   !
! Purpose: This subroutine calculates the nitrogen dynamics in a rice  !
!          crop and the effects on growth and development              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning                                     units  class !
! ----   ---- -------                                     -----  ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with crop data (-)                       I  !
! DELT    R4  Time step of integration (d)                          I  !
! TIME    R4  Time of simulation (d)                                I  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! TERMNL  L4  Flag to indicate if simulation is to stop (-)         I  !
! DVS     R4  Development stage of the crop (-)                     I  !
! LLV     R4  Loss rate of leaves caused by senescence (kg ha-1 d-1)I  !
! DLDR    R4  Loss rate of leaves caused by drought (kg ha-1 d-1)   I  !
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  I  !
! WST     R4  Dry weight of stems (kg ha-1)                         I  !
! WSO     R4  Dry weight of storage organs (kg ha-1)                I  !
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           I  !
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    I  ! 
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   I  !
! PLTR    R4  Intermediate variable for planting density (-)        I  !
! LAI     R4  Leaf area index (ha ha-1)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! TNSOIL  R4  Soil-N available for crop uptake (kg N ha-1 d-1)      I  !
! NACR    R4  Actual N uptake rate by the crop (kg ha-1 d-1)        O  !
! NFLV    R4  Nitrogen fraction in the leaves (g N m-2 leaf)        O  !
! NSLLV   R4  Stress factor for leaf death caused by N stress  (-)  O  !
! RNSTRS  R4  Decrease factor for RGRL caused by N stress (-)       O  !
!                                                                      !
! Subroutine called: SUBNBC                                            !
!                                                                      !
!----------------------------------------------------------------------*
      SUBROUTINE NCROP2 (ITASK, IUNITD, IUNITL, FILEI1, FILEIT, DELT, TIME, OUTPUT, &
                       TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV, &
                       PLTR, LAI, SLA, CROPSTA, TNSOIL, NACR, NFLV, NSLLV,RNSTRS)

      USE CHART

        IMPLICIT NONE

!     Formal parameters
!     Input parameters
      INTEGER IUNITD, IUNITL, ITASK
      CHARACTER (*) FILEI1, FILEIT

      LOGICAL OUTPUT, TERMNL

      REAL DELT, TIME
      REAL DVS, LLV, DLDR, WLVG, WST, WSO, PLTR, LAI, TNSOIL
      INTEGER CROPSTA

!     Ouput parameters
      REAL NACR, NFLV, NSLLV, RNSTRS, NFLV1

!     Local parameters
      INTEGER IMX
      PARAMETER (IMX=40)
      INTEGER ILNMAX, ILNMIN, ILNMNS, INSLLV, ILNFLV
      REAL RFNLV, RFNST, TCNTRF, ATNLV, ATNST, ATNRT, NUPP, NMAXUP
      REAL FNLV, FNST, FNSO, ANST, ANSO, ANCR, ANLV, ANCRPT
      REAL ANSTA, ANLVA, ANCRF, ANLD
      REAL NMAXL, NMINL, NMAXSO, NLDLV, NSHKLV, NSHKST, FNTRT
      REAL NALV, NAST, NASO, NALVS, NASTS, NASOS, NACRS, NTRTS
      REAL NDEMSX, NDEMC, NDEMS, NDEML, NDEMSN
      REAL ATN, NSO, NTSO, NTLV, NTST, NTRT, NLV, NSTAN, NST, NLVAN
      REAL NMINSO, GSO, GST, GLV, NFLVI, FNLVI, NFLVP
      REAL NMAXLT(IMX), NMINLT(IMX), NMINSOT(IMX), NSLLVT(IMX), NFLVTB(IMX)
      REAL NOTNUL, NCHCK, NBCHK, NSTRES, SLA

!     Functions
      REAL LINT2, INTGRL, LIMIT, INTGR2

      SAVE

!-----Initialization
      IF (ITASK.EQ.1) THEN
!       Initialize variables
        NUPP  = 0.
        ANLV   = 0.
        ANSO   = 0.
        ANST   = 0.
        ANLD   = 0.
        ANCR   = 0.
        ANLVA  = 0.
        ANSTA  = 0.
        ANCRF  = 0.
        NALVS  = 0.
        NASTS  = 0.
        NASOS  = 0.
        NACRS  = 0.
        NTRTS  = 0.
        NALV   = 0.
        NAST   = 0.
        NASO   = 0.
        NACR   = 0.
        NLV    = 0.
        NST    = 0.
        NSO    = 0.
        NLDLV  = 0.
        NLVAN  = 0.
        NSTAN  = 0.
        NTRT   = 0.
        ANCRPT = 0.
        NBCHK  = 0.
        NCHCK  = 0.

        FNLV   = FNLVI
        FNST   = 0.5*FNLVI
        FNSO   = 0.
        NFLV   = NFLVI

        NSLLV  = 1.
        RNSTRS = 1.

!------ Reading input parameters (from crop  file)
        CALL RDINIT(IUNITD, IUNITL, FILEI1)
        CALL RDAREA('NMAXLT ',NMAXLT ,IMX,ILNMAX)
        CALL RDAREA('NMINLT ',NMINLT ,IMX,ILNMIN)
        CALL RDAREA('NMINSOT',NMINSOT,IMX, ILNMNS)
        CALL RDAREA('NSLLVT ',NSLLVT ,IMX,INSLLV)
        CALL RDAREA('NFLVTB',NFLVTB,IMX,ILNFLV)
        CALL RDSREA('NMAXSO', NMAXSO)
        CALL RDSREA('NMAXUP', NMAXUP)
        CALL RDSREA ('NFLVI', NFLVI)
        CALL RDSREA ('FNLVI', FNLVI)
        CALL RDSREA('RFNLV', RFNLV)
        CALL RDSREA('RFNST', RFNST)
        CALL RDSREA('TCNTRF', TCNTRF)
        CALL RDSREA('FNTRT', FNTRT)
        CLOSE (IUNITD)

        NMINSO  = LINT2('NMINSOT',NMINSOT,ILNMNS,ANCRF)
        NMAXL   = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
        NMINL   = LINT2('NMINLT',NMINLT,ILNMIN,DVS)
        FNLV    = FNLVI
        NFLV    = NFLVI

!=====Rate calculations
      ELSE IF (ITASK.EQ.2) THEN

!------ Linear interpolation of parameter values
        NMINSO  = LINT2('NMINSOT',NMINSOT,ILNMNS,ANCRF)
        NMAXL   = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
        NMINL   = LINT2('NMINLT',NMINLT,ILNMIN,DVS)

!====== Only calculations after sowing
        IF (CROPSTA .GE. 4) THEN

!       Potential leaf N content (on LAI basis)
        NFLVP = LINT2('NFLVTB',NFLVTB,ILNFLV,DVS)

!========== Calculate (potential) N demand of crop organs
!           Maximum N demand of leaves
            NDEML  = (NMAXL*(WLVG+GLV*DELT)-ANLV)/DELT 
            IF (NDEML .LT. 0.) NDEML = 0.
!           Maximum N demand of stems
            NDEMS  = (NMAXL*0.5*(WST+GST*DELT)-ANST)/DELT
            IF (NDEMS .LT. 0.) NDEMS = 0.
!           Maximum N demand of storage organs
            NDEMSX = NMAXSO*GSO
            IF (NDEMSX .LT. 0.) NDEMSX = 0.
!           Minimum nitrogen demand of storage organs 
            NDEMSN = NMINSO*GSO
            IF (NDEMSN .LT. 0.) NDEMSN = 0.

!========== Calculate translocation of N from organs, in kg/ha/d
!           It is assumed that potential demand by storage organ is first met by translocation
!           No translocation before DVS = 0.95
            IF (DVS .LT. 0.95) THEN
              ATNLV = 0.
              ATNST = 0.
              ATN   = 0.
              NTSO = 0.
            ELSE
!             Maximum translocation amount from leaves and stems
              ATNLV = MAX(0., ANLV-WLVG*RFNLV)
              ATNST = MAX(0., ANST-WST*RFNST)
!             Maximum translocation amount from roots as fraction of that of shoot
              ATNRT = (ATNLV+ATNST)*FNTRT
              ATN   = ATNLV+ATNST+ATNRT
!             Daily translocation is total pool divided by time constant
              NTSO = ATN/TCNTRF
!             Translocation is limited between minimum (NDEMSN) and maximum (NDEMSX)
              NTSO = LIMIT(NDEMSN,NDEMSX,NTSO)
            END IF

!---------- Actual N translocation rates from plant organs, in kg/ha/d
            NTLV  = NTSO*ATNLV/NOTNUL(ATN)
            NTST  = NTSO*ATNST/NOTNUL(ATN)
            NTRT  = NTSO*ATNRT/NOTNUL(ATN)

!========== Calculate nitrogen uptake 
!           Available N uptake is mimimum of soil supply and maximum crop uptake
            NUPP = MIN(NMAXUP, TNSOIL)
            IF (NUPP .LT. 0.) NUPP = 0.

!           Sum total maximum uptake rates from leaves, stems and storage organs
            NDEMC  = (NDEML+NTLV)+(NDEMS+NTST)+(NDEMSX-NTSO)

!           Actual uptake per plant organ is minimum of availability and demand 
            NALV   = MAX(0.,MIN(NDEML+NTLV, NUPP*((NDEML+NTLV)/NOTNUL(NDEMC))))
            NAST   = MAX(0.,MIN(NDEMS+NTST, NUPP*((NDEMS+NTST)/NOTNUL(NDEMC))))
            NASO   = MAX(0.,MIN(NDEMSX-NTSO, NUPP*((NDEMSX-NTSO)/NOTNUL(NDEMC))))
!           Total uptake by crop from the soil
            NACR   = NALV+NAST+NASO

!========== Calculate net N flows to plant organs (daily rates)
!           Transplanting shock: remove N
            NSHKLV =ANLV*(1.-PLTR)
            NSHKST =ANST*(1.-PLTR)
!           Loss of N from leaves by leaf death
            NLDLV = (LLV+DLDR)*RFNLV
!NEW BB, aug 2003: with senescence and dying of leaves, the total leaf N
!           can go down. Yellow leaves initially have higher N content than RFNLV
            NLDLV  = MAX (NLDLV, ANLV-NMAXL*(WLVG+GLV-LLV-DLDR))

!---------- Net flow to stems and leaves
            NLV = NALV-NTLV-NLDLV-NSHKLV
            NST = NAST-NTST-NSHKST
!---------- Net N flow to storage organ
            NSO = NTSO+NASO

!---------- Net flow to stems and leaves before flowering 
            IF (DVS.LT. 1.) THEN
               NSTAN =NST
               NLVAN =NLV
            ELSE
               NSTAN = 0.
               NLVAN = 0.
            END IF

!====== End if statement for CROPSTA GT 0
        END IF

!        Output writing
         IF (OUTPUT) THEN
           CALL OUTDAT (2, 0, 'NDEML', NDEML)
         CALL ChartOutputRealScalar('NDEML', NDEML)
           CALL OUTDAT (2, 0, 'NDEMC', NDEMC)
         CALL ChartOutputRealScalar('NDEMC', NDEMC)
           CALL OUTDAT (2, 0, 'NUPP', NUPP)
         CALL ChartOutputRealScalar('NUPP',  NUPP)
           CALL OUTDAT (2, 0, 'ANCR', ANCR)
           CALL ChartOutputRealScalar('ANCR',  ANCR)
           CALL OUTDAT (2, 0, 'ANLV', ANLV)
         CALL ChartOutputRealScalar('ANLV',  ANLV)
           CALL OUTDAT (2, 0, 'ANLD', ANLD)
         CALL ChartOutputRealScalar('ANLD',  ANLD)
           CALL OUTDAT (2, 0, 'ANST', ANST)
           CALL ChartOutputRealScalar('ANST',  ANST)
           CALL OUTDAT (2, 0, 'ANSO', ANSO)
           CALL ChartOutputRealScalar('ANSO',  ANSO)
           CALL OUTDAT (2, 0, 'NMAXL', NMAXL)
         CALL ChartOutputRealScalar('NMAXL', NMAXL)
           CALL OUTDAT (2, 0, 'NMINL', NMINL)
         CALL ChartOutputRealScalar('NMINL', NMINL)
           CALL OUTDAT (2, 0, 'FNLV', FNLV)
         CALL ChartOutputRealScalar('FNLV',  FNLV)
         END IF

!=====State updates/integration
      ELSE IF (ITASK.EQ.3) THEN

!------- N amount in plant organs
         ANSO  =INTGRL(ANSO,NSO, DELT)
         ANLV  =INTGRL(ANLV,NLV,DELT)
         ANST  =INTGRL(ANST,NST,DELT)
         ANLD  =INTGRL(ANLD,NLDLV,DELT)
         ANCR  =ANSO+ANLV+ANLD+ANST

!------- N amount in plant organs before flowering
         ANLVA =INTGRL(ANLVA,NLVAN,DELT)
         ANSTA =INTGRL(ANSTA ,NSTAN,DELT)
         ANCRF =ANSTA+ANLVA

!------- Total N uptake from soil
         NALVS = INTGRL(NALVS, NALV, DELT)
         NASTS = INTGRL(NASTS, NAST, DELT)
         NASOS = INTGRL(NASOS, NASO, DELT)
         NACRS = NALVS+NASTS+NASOS

!------- Total N supply by translocation from roots
         NTRTS = INTGRL(NTRTS, NTRT, DELT)

!------- Nitrogen balance check
         NCHCK = ANCR - (NACRS+NTRTS)
         CALL SUBNBC (ANCR,(NACRS+NTRTS),TIME,NBCHK,TERMNL)

!======= Calculate N contents and N stress factors (only if in main field)
         IF (CROPSTA .LT. 4) THEN
            FNLV   = FNLVI
            FNST   = 0.5*FNLVI
            FNSO   = 0.
            NFLV   = NFLVI
            NSLLV  = 1.
            RNSTRS = 1.
         ELSE
!---------- Fraction N in plant organs
            FNLV  =ANLV/NOTNUL(WLVG)
            FNST  =ANST/NOTNUL(WST)
            FNSO  =ANSO/NOTNUL(WSO)
!           Leaf N content in g N/m2 leaf
            IF (LAI .EQ. 0.) THEN
              NFLV = NFLVI
            ELSE 
!              IF (LAI .LT.1. .AND. DVS .LT.1.) THEN
!                  NFLV = (FNLV/NMAXL)*NFLVP
!               ELSE
!                  NFLV = ANLV/(10.*LAI)
!               END IF
! Bouman, Feb 2004 This is a programming 'trick' to enable forcing of observed
!          NFLV as function of day-of-observation. Below the first observation
!          day, NFLV1 is used;l between first and last observation day, interpolated
!          observed values are used; after last observation day, NFLV1 is used again
!          Forcing is determined by the variable NFLV_FRC in the experiment data file.
            NFLV1 = FNLV/(10.*SLA)
            NFLV   = INTGR2(0., NFLV1, DELT, FILEIT, 'NFLV')

            END IF

!---------- Set N stress factor for leaf death
            ANCRPT =WLVG*NMAXL+WST*NMAXL*0.5+WSO*NMAXSO
            IF (ANCR .EQ. 0.) THEN
              NSTRES = 2.
            ELSE
              NSTRES = ANCRPT/ANCR
            END IF
            IF (NSTRES .LT. 1.) NSTRES = 1.
            IF (NSTRES .GT. 2.) NSTRES = 2.
            NSLLV = LINT2('NSLLVT', NSLLVT, INSLLV, NSTRES)

!---------- Set N stress factor for RGRL
            RNSTRS = (FNLV-0.9*NMAXL)/(NMAXL-0.9*NMAXL)
            IF (RNSTRS .GT. 1.) RNSTRS = 1.
            IF (RNSTRS .LT. 0.) RNSTRS = 0.

!======= End IF statement for main field
         END IF

!======= Termination of simulation when there is too little N in leaves
         IF (LAI .GT. 1. .AND. FNLV .LE. 0.5*NMINL) THEN
             WRITE(*,*), 'Leaf N < 0.5*MINIMUM; simulation stopped'
             CALL OUTCOM('Leaf N < 0.5*MINIMUM; simulation stopped')
             TERMNL = .TRUE.
         END IF

!-----Terminal output statements
      ELSE IF (ITASK.EQ.4) THEN

         CALL OPSTOR ('ANCR', ANCR)
!         CALL OPSTOR ('NACRS', NACRS)
!         CALL OPSTOR ('NTRTS', NTRTS)

!     END ITASKS
      END IF

      RETURN
      END


!----------------------------------------------------------------------*
!  SUBROUTINE SUBNBC                                                   *
!  Purpose: This subroutine checks the Crop Nitrogen Balance           *
!           and stops the simulation if the difference between         *
!           CHKIN and CHKFL exceeds 0.1 %                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CHKIN   R4  Accumulated N in the crop (kg N ha-1)                 I  *
! CHKFL   R4  Sum of N supplied by soil and roots (kg N ha-1)       I  *
! TIME    R4  Time of simulation (d)                                T  *
! NBCHK   R4  Nitrogen balance check, relative value to the sums of    *
!             CHKIN and CHKFL (-)                                   O  *
! TERMNL  R4  Flag to indicate if simulation is to stop (-)         O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE SUBNBC (CHKIN,CKCFL,TIME,NBCHK,TERMNL)

      IMPLICIT NONE
!-----Formal parameters
      REAL    CHKIN,CKCFL,TIME,NBCHK
      LOGICAL TERMNL
      SAVE
 
      NBCHK = 2.0*(CHKIN-CKCFL)/(CHKIN+CKCFL+1.E-10)
 
      IF (ABS(NBCHK).GT.0.001) THEN
         WRITE (*,'(A,/,A,F8.3,2(A,F8.2),F6.1)') &
           '* * * Error in Nitrogen Balance, please check * * *', &
           ' NBCHK=',NBCHK,', CHKIN=',CHKIN,', CKCFL=',CKCFL,' at TIME=',TIME
         TERMNL = .TRUE.
      END IF
 
      RETURN
      END

