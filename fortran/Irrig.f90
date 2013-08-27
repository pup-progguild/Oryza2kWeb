!----------------------------------------------------------------------!
!    SUBROUTINE IRRIG                                                  !
!    VERSION 1.1                                                       !
!    Author: B.A.M. Bouman                                             !
!    Date  : December 2001                                             !
!          Version April, 2008                                        !
!    Purpose: To calculate daily irrigation amounts                    !
!    There are 6 irrigation manners:                                   !
!       SWITIR = 0 ! No irrigation; rainfed                            !
!       SWITIR = 1 ! Irrigation supplied as input data                 !
!       SWITIR = 2 ! Irrigation at minimum ponded soil water depth     !
!       SWITIR = 3 ! Irrigation at minimum soil water potential        !
!       SWITIR = 4 ! Irrigation at minimum soil water content          !
!       SWITIR = 5 ! Irrigation at X days after disapp. ponded water   !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     Type  !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEIT  C*  Name of input file (-)                                I  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! DOY     R4  Day number (January 1 = 1) (d)                        I  !
! DELT    R4  Time step of integration (d)                          I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! WL0     R4  Depth of ponded water (mm)                            I  !
! NL      I4  Number of soil layers (-)                             I  !
! WCLQT   R4  Array of actual soil water content/layer (m3 m-3)     I  !
! MSKPA   R4  Array with soil water tension/layer (KPa)             I  !
! IR      R4  Amount of daily irrigation (mm d-1)                   O  !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE IRRIG (ITASK, IUNITD, IUNITL , FILEIT, OUTPUT, &
                        DOY  , DELT  , CROPSTA, WL0   , &
                        DVS, NL   , WCLQT , MSKPA  , IR)
 
      USE CHART

      IMPLICIT NONE
!-----Formal parameters
      INTEGER        ITASK, IUNITD, IUNITL, CROPSTA, NL
      LOGICAL        OUTPUT
      CHARACTER (80) FILEIT
      REAL           DOY, DELT, WL0, IR
      REAL           WCLQT(NL), MSKPA(NL)
!-----Local variables
      INTEGER   IRIRR, SLMIN, SWITIR, WL0CNT, WL0DAY
      REAL      IRC  , IRCU,  IRRI , KPAMIN, WCMIN , WL0MIN
      REAL      RIRRIT(500)
!-----Functions
      REAL LINT2
      REAL DVSIMAX, DVS

! new april 2008
      REAL ISTAGET(15)
      INTEGER I, ISTAG

      SAVE

      IF (ITASK.EQ.1) THEN
!===================================================================!
!     Initialization                                                !
!===================================================================!

! Fill ISTAGET with values above 2.5 
         DO I=1, 15
           ISTAGET(I) = 10.
         END DO
 
!---- Read irrigation data from soil data file
         CALL RDINIT(IUNITD,IUNITL,FILEIT)
!------- Irrigation switch
         CALL RDSINT('SWITIR',SWITIR)

         CALL RDSREA('DVSIMAX', DVSIMAX)
         IF (SWITIR.EQ.1) THEN
            CALL RDAREA('RIRRIT',RIRRIT,500,IRIRR)
            CALL OUTCOM('Irrigation read from table; SWITIR=1')
         ELSE IF (SWITIR.EQ.2) THEN
            CALL RDSREA('WL0MIN',WL0MIN)
            CALL RDSREA('IRRI',IRRI)
            CALL OUTCOM ('Irrigation at minimum water depth; SWITIR=2')
         ELSE IF (SWITIR.EQ.3) THEN
            CALL RDSINT('SLMIN',SLMIN)
            CALL RDSREA('KPAMIN',KPAMIN)
            CALL RDSREA('IRRI',IRRI)
            CALL OUTCOM('Irrigation at min. moist. pressure; SWITIR=3')
         ELSE IF (SWITIR.EQ.4) THEN
            CALL RDSINT('SLMIN',SLMIN)
            CALL RDSREA('WCMIN',WCMIN)
            CALL RDSREA('IRRI',IRRI)
            CALL OUTCOM('Irrigation at min. moist. content; SWITIR=4')
         ELSE IF (SWITIR.EQ.5) THEN
            CALL RDSINT('WL0DAY',WL0DAY)
            CALL RDSREA('IRRI',IRRI)
            CALL OUTCOM('Irrigation at number of days; SWITIR=5')
         ELSE IF (SWITIR.EQ.6) THEN
            CALL RDSINT('SLMIN',SLMIN)
            CALL RDSREA('IRRI',IRRI)
            CALL RDAREA('ISTAGET',ISTAGET,15,ISTAG)
            CALL OUTCOM('Irrigation by stage at most. threshold; SWITIR=6')
         ELSE IF (SWITIR.EQ.0) THEN
            CALL OUTCOM('Rainfed; no irrigation; SWITIR=0')
         ELSE
            CALL FATALERR ('IRRIG','Unknown switch for SWITIR in soil file')
         END IF
!------- Reading of soil data completed
         CLOSE (IUNITD)

         IR     = 0.
         IRC    = 0.
         IRCU   = 0.
         WL0CNT = 0

! April 2008
         IF (ISTAG .GT. 15) THEN
            CALL FATALERR ('ISTAG','Cannot be more than 5 in exp. file')
         END IF 

!===================================================================!
!     Rate calculation section                                      !
!===================================================================!
      ELSE IF (ITASK.EQ.2) THEN

!---    Reset irrigation amount at zero every day as default
        IR = 0.

!-------Set irrigation for main field (i.e., not in seedbed)
!       Irrigation only starts after direct-seeding or at transplanting
!        IF (CROPSTA.LT.3) IR = 0.
!        IF (CROPSTA.GE.3) THEN
!Bouman, May 2003: always irrigation, also of empty field until tranplanting
! Bouman (Sept 2006): no irrigation after certain development stage
             IF (DVS .LT. DVSIMAX) THEN

!---       Rainfed: no irrigation
           IF (SWITIR.EQ.0) THEN
              IR = 0.
!---       Irrigation read from table
           ELSE IF (SWITIR.EQ.1) THEN
              IR = LINT2('RIRRIT',RIRRIT,IRIRR,DOY)
!---       Irrigation at minimum depth of ponded water
           ELSE IF (SWITIR.EQ.2) THEN
              IF (WL0.LE.WL0MIN) THEN
                 IR = IRRI
              ELSE
                 IR = 0.
              END IF
!---       Irrigation at critical soil water tension
           ELSE IF (SWITIR.EQ.3) THEN
              IF(MSKPA(SLMIN).GE.KPAMIN) THEN
                  IR = IRRI
              ELSE
                  IR = 0.
              END IF
!---       Irrigation at minimum soil water content
           ELSE IF (SWITIR.EQ.4) THEN
              IF(WCLQT(SLMIN).LE.WCMIN) THEN
                  IR = IRRI
              ELSE
                  IR = 0.
              END IF
!---       Irrigation at number of days after disappearance of ponded water
!          (defined as 1 mm).
           ELSE IF (SWITIR.EQ.5) THEN
              IF (WL0.LE.1.) THEN
                 IF (WL0CNT.EQ.WL0DAY) THEN
                     IR = IRRI
                     WL0CNT = 0
                 ELSE
                     IR = 0.
                     WL0CNT = WL0CNT + DELT
                 END IF
              ELSE
                 IR = 0.
              END IF
! New April 2008
           ELSE IF (SWITIR.EQ.6) THEN
              IF (DVS .GT. ISTAGET(1).AND. DVS .LE. ISTAGET(2)) THEN
                 IF(MSKPA(SLMIN).GE.ISTAGET(3)) THEN
                    IR = IRRI
                 END IF
              ELSE IF (DVS .GT. ISTAGET(4).AND. DVS .LE. ISTAGET(5)) THEN
                 IF(MSKPA(SLMIN).GE.ISTAGET(6)) THEN
                    IR = IRRI
                 END IF
              ELSE IF (DVS .GT. ISTAGET(7).AND. DVS .LE. ISTAGET(8)) THEN
                 IF(MSKPA(SLMIN).GE.ISTAGET(9)) THEN
                    IR = IRRI
                 END IF
              ELSE IF (DVS .GT. ISTAGET(10).AND. DVS .LE. ISTAGET(11)) THEN
                 IF(MSKPA(SLMIN).GE.ISTAGET(12)) THEN
                    IR = IRRI
                 END IF
              ELSE IF (DVS .GT. ISTAGET(13).AND. DVS .LE. ISTAGET(14)) THEN
                 IF(MSKPA(SLMIN).GE.ISTAGET(15)) THEN
                    IR = IRRI
                 END IF
              END IF


           END IF
        END IF

!
        IF (OUTPUT) THEN
! Bouman (May 2003): this now in soil water balances 
!           CALL OUTDAT (2,0,'IR' ,IR )
!           CALL ChartOutputRealScalar('IR' ,IR)
         END IF

!===================================================================!
!     Integration section                                           !
!===================================================================!
      ELSE IF (ITASK.EQ.3) THEN

! Bouman (May 2003): this now in soil water balances 
         IRCU   = IRCU   + IR*DELT
!-----Cumulative amounts in main field when crop is present
         IF (CROPSTA.GE.3) THEN
            IRC   = IRC   + IR*DELT
         END IF

!===================================================================!
!     Terminal calculations                                         !
!===================================================================!
      ELSE IF (ITASK.EQ.4) THEN
! Bouman (May 2003): this now in soil water balances 
!         CALL OPSTOR ('IRC', IRC)
!         CALL OPSTOR ('IRCU', IRCU)
      END IF

      RETURN
      END

