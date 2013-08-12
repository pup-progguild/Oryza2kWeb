!----------------------------------------------------------------------!
!  SUBROUTINE NSOIL                                                    !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date   : December 2001                                              !
!  Author : B.A.M. Bouman                                              !
!  Purpose: This module calculates the supply of N from the soil.      !
!                                                                      !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEIT  C*  Name of file with experimental data (-)               I  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! DELT    R4  Time step of integration (d)                          I  !
! DAE     R4  Days after emergence (d)                              I  !
! DVS     R4  Development stage of the crop (-)                     I  !
! NACR    R4  Actual N uptake rate  by the crop (kg N ha-1 d-1)     I  !
! TNSOIL  R4  Total N in the soil for uptake by crop (kg ha-1 d-1)  O  !
!                                                                      !
! SUBROUTINES called: -                                                !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE NSOIL(ITASK, IUNITD, IUNITL, FILEIT, OUTPUT, DELT, DAE, &
                    DVS, NACR, TNSOIL)
                    
      USE CHART

        IMPLICIT NONE

!-----Formal parameters
!     INPUT
      INTEGER IUNITD, IUNITL, ITASK
      CHARACTER (*) FILEIT
      LOGICAL OUTPUT
      REAL DELT, DAE, DVS, NACR 

!     OUTPUT
      REAL TNSOIL

!-----Local variables
      INTEGER INX, ILFERT, ILREC
      PARAMETER (INX=100)
      REAL FERT, RECOV, SOILSP, XFERT, NFERTP
      REAL FERTIL(INX), RECNIT(INX)      

!     Used functions
      REAL LINT2, INTGRL

      SAVE

!-----Initialization
      IF (ITASK.EQ.1) THEN

!        Reading input parameters (from experiment file)
         CALL RDINIT(IUNITD, IUNITL, FILEIT)
         CALL RDAREA ('FERTIL', FERTIL, INX, ILFERT)
         CALL RDAREA ('RECNIT', RECNIT, INX, ILREC)
         CALL RDSREA ('SOILSP', SOILSP)
         CLOSE (IUNITD)

!        Initialize state variables
         TNSOIL = 0.
         NFERTP = 0.

!-----Rate calculations
      ELSE IF (ITASK.EQ.2) THEN
!        Daily supply of N by fertilizer application
         FERT  =  LINT2('FERTIL',FERTIL,ILFERT,DAE)
         RECOV =  LINT2('RECNIT',RECNIT,ILREC,DVS)
         XFERT =  FERT*RECOV

!        Output writing
         IF (OUTPUT) THEN
            CALL OUTDAT (2, 0, 'XFERT' , XFERT)
            CALL ChartOutputRealScalar('XFERT', XFERT)
            CALL OUTDAT (2, 0, 'TNSOIL' , TNSOIL)
            CALL ChartOutputRealScalar('TNSOIL', TNSOIL)
            CALL OUTDAT (2, 0, 'NACR'  , NACR)
            CALL ChartOutputRealScalar('NACR', NACR)
       END IF

!-----State updates/integration
      ELSE IF (ITASK.EQ.3) THEN
!        Total N pool present in the soil
         NFERTP = INTGRL(NFERTP,  XFERT-MAX(0.,(NACR-SOILSP)),  DELT)
!        Amount of N availbale for uptake each day
         TNSOIL = NFERTP+SOILSP

!-----Terminal output statements
      ELSE IF (ITASK.EQ.4) THEN

!     END ITASKS
      END IF

      RETURN

      END
