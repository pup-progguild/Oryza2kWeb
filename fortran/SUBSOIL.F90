!          Version august, 2003                                        *
!----------------------------------------------------------------------*
! SUBROUTINE GWTAB                                                     *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)              C,I *
! SWITGW  I4  Groundwater switch (-)                                C  *
! NL      I4  Number of soil layers (-)                             I  *
! DOY     R4  Daynumber (January 1 = 1) (-)                         I  *
! DELT    R4  Time step of integration (d)                          T  *
! WLFL    R4  Array of fluxes out off soil compartments (mm d-1)    I  *
! TKL     R4  Array of thicknesses soil layers (m)                  I  *
! ZWPREV  R4  Groundwater tabel depth of previous day (cm)          O  *
! IGW     I4  Number of shallowest soil compartment in groundwater     *
!             (-)                                                   O  *
! ZW      R4  Depth of groundwater table below soil surface (cm)    O  *
!                                                                      *
! FUNCTIONS used : LINT2                                               *
!----------------------------------------------------------------------*

      SUBROUTINE GWTAB(ITASK,SWITGW,NL,DOY,DELT,WLFL,TKL,ZWPREV,IGW,ZW)
                                                         
      IMPLICIT NONE
 
!     Formal parameters
      INTEGER ITASK,SWITGW,NL,IGW
      REAL    WLFL(NL+1),TKL(NL)
      REAL    DOY,DELT,ZWPREV, ZW
 
!     Local variables
      REAL    ZLL, ZWL

      INCLUDE 'COMMON_GWT.INC'

!-----Used functions
      REAL LINT2

      SAVE
 
      IF (ITASK.EQ.1) THEN
         IF (SWITGW.EQ.1) THEN
            ZW = LINT2('ZWTB',ZWTB,IZWTB,DOY)
         ELSE
            ZW = ZWTBI
         END IF
         ZWPREV = ZW
      END IF
      IGW = 1
      ZLL = 0.
      ZWL = 0.
      DO WHILE ((IGW.LE.NL).AND.(ZWL.LE.0.))
         ZLL = ZLL+TKL(IGW)/10.
         ZWL = ZLL-ZWPREV
         IGW = IGW+1
      END DO
      IF (ZWL.GT.0.) IGW = IGW-1
      IF (ITASK.EQ.3) THEN
         IF (SWITGW.EQ.1) THEN
            ZW = LINT2('ZWTB',ZWTB,IZWTB,DOY)
         ELSE
            ZW = ZW+ZWA*DELT-ZWB*10.*WLFL(IGW)*DELT
            IF (ZW.LT.MINGW) ZW = MINGW
            IF (ZW.GT.MAXGW) ZW = MAXGW
         END IF
      END IF

      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE BACKFL                                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! WL      R4  actual water content (mm)                             I  *
! FLIN    R4  flux into soil compartment (mm d-1)                   I  *
! FLOUT   R4  flux out off soil compartment (mm d-1)                I  *
! EVSWS   R4  Actual evaporation rate soil compartment 1 (m d-1)    I  *
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     I  *
! WLST    R4  Array amount of water per soil compartment at            *
!             saturation (mm)                                       I  *
! DELT    R4  Time step of integration (d)                          T  *
! FLNEW   R4  Boundary flow between soil compartments recalculated     *
!             via subroutine BACKFL (mm d-1)                        O  *
! HLP     R4  Help variable (mm)                                    O  *
!                                                                      *
! SUBROUTINES and FUNCTIONS used : none                                *
!----------------------------------------------------------------------*

      SUBROUTINE BACKFL(I,WL,FLIN,FLOUT,EVSWS,TRWL,WLST,DELT,FLNEW,HLP)
 
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I
      REAL    WL,FLIN,FLOUT,EVSWS,TRWL,WLST,DELT,FLNEW,HLP
      SAVE
 
      HLP = 0.
 
      IF (I.EQ.1) THEN
         HLP = WL+(FLIN-FLOUT-EVSWS-TRWL)*DELT
      ELSE
         HLP = WL+(FLIN-FLOUT-TRWL)*DELT
      END IF
 
      IF (HLP.GT.WLST) THEN
         FLNEW = FLIN-(HLP-WLST)/DELT
      ELSE
         FLNEW = FLIN
      END IF
 
      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE DOWNFL                                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! KSAT    R4  Saturated hydraulic conductivity (cm d-1)             I  *
! FLIN    R4  Flux into soil compartment (mm d-1)                   I  *
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     I  *
! EVSWS   R4  Actual evaporation rate soil compartment 1 (m d-1)    I  *
! WL      R4  Actual water content (mm)                             I  *
! WLFC    R4  Array amount of water per soil compartment at 'field     *
!             capacity' (mm)                                        I  *
! DELT    R4  Time step of integration (d)                          T  *
! FLOUT   R4  Flux out off soil compartment (mm d-1)                O  *
!                                                                      *
! SUBROUTINES and FUNCTIONS used : none                                *
!----------------------------------------------------------------------*

      SUBROUTINE DOWNFL(I,KSAT,FLIN,TRWL,EVSWS,WL,WLFC,DELT,FLOUT)
 
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I
      REAL    KSAT,FLIN,TRWL,EVSWS,WL,WLFC,DELT,FLOUT
      SAVE
 
      IF (I.EQ.1) THEN
         FLOUT = MIN(10.*KSAT,MAX(0.,FLIN-EVSWS-TRWL+(WL-WLFC)/DELT))
      ELSE
         FLOUT = MIN(10.*KSAT,MAX(0.,FLIN-TRWL+(WL-WLFC)/DELT))
      END IF
 
      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE SATFLX                                                    *
!                                                                      *
! Date     : March 1993, Version: 1.0                                  *
! Purpose  : SATFLX determines percolation rate as a                   *
!            function of ponded water depth, hydraulic conductivity    *
!            compacted layer and hydraulic conductivity subsoil        *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! TKL     R4  Array of thicknesses soil layers (m)                  I  *
! NLPUD   I4  Number of puddled soil compartments including plow       *
!             sole (-)                                              I  *
! WL0     R4  Amount of ponded water (mm)                           I  *
! PERC    R4  Percolation rate (cm d-1)                             O  *
!----------------------------------------------------------------------*
 
      SUBROUTINE SATFLX(TKL,NLPUD,WL0,PERC)
 
      IMPLICIT NONE
!-----Formal parameters
      REAL      WL0, PERC
      INTEGER   NLPUD
      REAL      TKL(10)

!-----Local variables
      INCLUDE 'COMMON_SWIT.INC'
      INCLUDE 'COMMON_NUCHT.INC'
      INCLUDE 'COMMON_HYDCON.INC'
      INCLUDE 'COMMON_POWER.INC'

      INTEGER I
      REAL    A, DF, DFX, DGX , DZL , F , F1, F2, FX, GX
      REAL    L, N , M  , HLP1, HLP2, HS, HSPREV, HT, TKLTOT, TINY
      SAVE

      TKLTOT = 0.
!-----Arbitrary initial value for pressure head subsoil
      TINY   = 1.E-5
!---- Pressure head top compartments (WL0 and TKL are in mm!)
      HS     = -10.0
 
      I = 1
      DO WHILE (I.LE.NLPUD-1)
         TKLTOT = TKLTOT+TKL(I)/10.
         I = I+1
      END DO
      HT  = WL0/10.+TKLTOT
 
!------- Thickness plow sole in cm
      DZL = TKL(NLPUD)/10.
 
      IF (SWITKH.EQ.1) THEN
!------- Van Genuchten parameters of unsaturated compartment 3
         N = VGN(NLPUD+1)
         A = VGA(NLPUD+1)
         L = VGL(NLPUD+1)
         M = 1.-1./N
 
!------- Assign arbitrary value to F, the difference function
         F = 10.*TINY
 
         I = 1
         DO WHILE ((ABS(F).GT.TINY).AND.(I.LE.50))
 
            IF (HS.GT.0) THEN
!-------- Estimated pressure head out of range, reset to previous
!-------- value, divided by 2
!               WRITE (*,*) ' HS > 0, RESET'
               HS = (HSPREV/2)
            END IF
 
!-------- Calculate Van Genuchten parameters
          HLP1 = (1.+(A*ABS(HS))**N)**M-(A*ABS(HS))**(N-1.)
          HLP2 =  1.+(A*ABS(HS))**N
          FX   = HLP1**2
          GX   = HLP2**(M*(L+2.))
 
!---------Estimated flux through unsaturated compartment
          F2   = KST(NLPUD+1)*(FX/GX)
 
!---------Calculate derivative of Van Genuchten equation
          DFX  = 2.*HLP1*(M*(HLP2**(M-1.))*(-N)*A*((A*ABS(HS))**(N-1.)) &
                 -A*(-N+1.)*((A*ABS(HS))**(N-2.)))
          DGX  = M*(L+2.)*(HLP2**(M*(L+2.)-1.))*A*(-N) &
                  *((A*ABS(HS))**(N-1.))
 
!---------Estimated flux through saturated compartment
          F1   = KST(NLPUD)*((HT-HS)/DZL+1.)
 
!---------Continue iteration until difference f is approximately zero
          F    = F2-F1
          DF   = KST(NLPUD+1)*((GX*DFX-FX*DGX)/GX**2)+KST(NLPUD)/DZL
 
!---------Keep current value of HS in case next is positive
          HSPREV = HS
 
!---------Estimate new value for pressure head unsaturated compartment
          HS   = HS-F/DF
          I    = I+1

          END DO
      ELSE IF (SWITKH.EQ.2) THEN
!------- Power function for hydraulic conductivity
         N = PN(NLPUD+1)
         F = 10.*TINY
 
         I = 1
         DO WHILE ((ABS(F).GT.TINY).AND.(I.LE.50))
 
            IF (HS.GT.0) HS = HSPREV/2
!-----------Estimated flux through saturated compartment
            F1 = KST(NLPUD)*((HT-HS)/DZL+1.)
 
!-----------Estimated flux through unsaturated compartment
            F2 = KST(NLPUD+1)*(ABS(HS)**N)
 
!-----------Continue iteration until difference f is approximately zero
            F  = F2-F1
            DF = -N*((ABS(HS))**(N-1.))*KST(NLPUD+1)+KST(NLPUD)/DZL
 
!-----------Keep current value of HS in case next is positive
            HSPREV = HS
 
!-----------Estimated new value for pressure head unsaturated compartment
            HS = HS-F/DF
            I  = I+1
 
         END DO
      END IF

!-----Save flux (mm/d)
      PERC = 10.*(F1+F2)/2.

      RETURN
      END

!----------------------------------------------------------------------*
!  SUBROUTINE SHRINK                                                   *
!                                                                      *
!  Version: 1.0                                                        *
!  Date:    April 1994                                                 *
!  Purpose: SHRINK calculates volumetric water content of              *
!           puddled soil compartments                                  *
!           using a shrinkage factor equal to WCSTRP/WCST              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! I       I4  Compartment index (-)                                 I  *
! MNL     I4  Maximum compartment numbers (-)                       I  *
! WL      R4  Actual water content / layer (mm)                     I  *
! TKL     R4  Array of thicknesses soil layers (mm)                 I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  I  *
! WCSTRP  R4  Array saturated volumetric water content ripened      I  *
!             soil per soil compartment (cm3 cm-3)                  I  *
! WCL     R4  Actual water content / layer (cm3 cm-3)               O  *
! TOTPOR  R4  Total porosity / layer (cm3 cm-3)                     O  *
! VL      R4  Thickness soil compartment after shrinkage (mm)       O  *
!                                                                      *
! SUBROUTINES and FUNCTIONS called: none                               *
!----------------------------------------------------------------------*
 
      SUBROUTINE SHRINK(ITASK,MNL,I,WL,TKL,WCST,WCSTRP,WCL,TOTPOR,VL)
 
      IMPLICIT NONE
!-----Formal parameters 
      INTEGER I,ITASK, MNL
      REAL    WL, WCST, TKL, WCSTRP, WCL, TOTPOR, VL
      
!-----Local parameters
      REAL WLLOW(MNL)
      REAL WLST, WLSTRP
      SAVE
 
      WLST   = WCST  *TKL
      WLSTRP = WCSTRP*TKL
 
      IF (ITASK.EQ.1) THEN
         WLLOW(I) = WL
         IF (WL.GE.WLSTRP) THEN
            VL     = TKL
            TOTPOR = WL/TKL
            WCL    = WL/TKL
         ELSE
            VL     = TKL
            TOTPOR = WLSTRP/TKL
            WCL    = WL/TKL
         END IF
      ELSE
         IF (WL.GE.WLLOW(I)) THEN
            VL     = TKL
            WCL    = WL/TKL
         END IF
         IF ((WL.LT.WLLOW(I)).AND.(WL.GE.WLSTRP)) THEN
            WLLOW(I) = WL
            VL       = TKL
            TOTPOR   = WL/TKL
            WCL      = WL/TKL
         END IF
         IF ((WL.LT.WLLOW(I)).AND.(WL.LT.WLSTRP)) THEN
            WLLOW(I) = WL
            VL       = TKL
            TOTPOR   = WLSTRP/TKL
            WCL      = WL/TKL
         END IF
      END IF
 
      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE SUBSL2                                                    *
!                                                                      *
! Author   : C. Rappoldt                                               *
!            M. Wopereis (revision March 1993)                         *
! Date     : January 1986, revised June 1990                           *
!            Slightly changed to work with VG parameters, March 1993   * 
! Purpose  :                                                           *
! Chapter 15 in documentation WOFOST Version 4.1 (1988) This routine   *
! calculates the rate of capillary flow or percolation between         *
! groundwater table and root zone. The stationary flow is found by     * 
! integration of dZL = K.d(MH)/(K + FLW), where Z= height above        *
! groundwater, MH= matric head, K= conductivity and FLW= chosen flow.  *
! In an iteration loop the correct flow is found.  The integration     *
! goes at most over four intervals: [0,45],[45,170], [170,330] and     *
! [330,MH-rootzone] (last one on logarithmic scale).                   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! PF      R4  pF value soil compartment (-)                         I  *
! D       R4  Distance to grounwater table (cm)                     I  *
! I       I4  Compartment index (-)                                 I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  I  *
! FLOW    R4  Capillary rise calculated by subroutine SUBSL2 (mm       *
!             d-1)                                                  O  *
!                                                                      *
! SUBROUTINES called: SUMSKM2                                          *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE SUBSL2(PF,D,I,WCST, FLOW)
 
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I
      REAL    PF, D, WCST, FLOW

!-----Local variables
      INTEGER I1,I2,I3,IINT,IMAX
      REAL    D1, DF, ELOG10, FL, FLW, FU, KMS, LOGST4, MH, PF1, Z 
      REAL    START(4),PFSTAN(9),PGAU(3) ,WGAU(3)
      REAL    DEL(4)  ,PFGAU(12),HULP(12),CONDUC(12)
 
      INCLUDE 'COMMON_NUCHT.INC'
      INCLUDE 'COMMON_HYDCON.INC'
      INCLUDE 'COMMON_POWER.INC'

      REAL K0
      SAVE
 
      DATA ELOG10/2.302585/
      DATA PGAU  /.1127016654,.5,.8872983346/
      DATA WGAU  /.2777778,.4444444,.2777778/
      DATA START /0.,45.,170.,330./
      DATA LOGST4/2.518514/
      DATA PFSTAN/0.705143,1.352183,1.601282,1.771497,2.031409,2.192880, &
                  2.274233,2.397940,2.494110/
 
!-----Calculation of matric head and check on small pF
      PF1  = PF
      D1   = D
      MH   = EXP(ELOG10*PF1)
      IF (PF1.LE.0.) GOTO 90
      IINT  = 0
 
!-----Number and width of integration intervals
      DO I1 = 1,4
         IF (I1.LE.3) DEL(I1) = MIN(START(I1+1),MH)-START(I1)
         IF (I1.EQ.4) DEL(I1) = PF1-LOGST4
         IF (DEL(I1).LE.0.) GOTO 20
         IINT = IINT+1
      END DO
 
!-----Preparation of three-point Gaussian integration
20    DO I1 = 1,IINT
         DO I2 = 1,3
            I3 = 3*(I1-1)+I2
            IF (I1.EQ.IINT) GOTO 30
!--         The three points in the full-width intervals are standard
                           PFGAU(I3) = PFSTAN(I3)
            GOTO 40
30          CONTINUE
!--         The three points in the last interval are calculated
            IF (IINT.LE.3) PFGAU(I3) = LOG10(START(IINT)+PGAU(I2)* &
                                       DEL(IINT))
            IF (IINT.EQ.4) PFGAU(I3) = LOGST4+PGAU(I2)*DEL(IINT)
40          CONTINUE
!         Variables needed in the loop below
!       NEXT LINE CHANGED
!         CONDUC(I3) = EXP (ELOG10*AFGEN (CONTAB,ILCON,PFGAU(I3)))
!       START CHANGES
          CALL SUMSKM2(I,EXP(ELOG10*PFGAU(I3)),WCST,KMS)
          CONDUC(I3) = KMS
!       END CHANGES
            HULP(I3) = DEL(I1)*WGAU(I2)*CONDUC(I3)
            IF (I3.GT.9) HULP(I3) = HULP(I3)*ELOG10*EXP(ELOG10*PFGAU(I3))
         END DO
      END DO
 
!-----Setting upper and lower limit
      FU = 1.27
! NEXT LINE CHANGED
!      FL = -1.*EXP (ELOG10*AFGEN (CONTAB, ILCON, PF1))
! START CHANGES
      CALL SUMSKM2(I,EXP(ELOG10*PF1),WCST,KMS)
      FL = -1.*KMS
! END CHANGES
      IF (MH.LE.D1) FU = 0.
      IF (MH.GE.D1) FL = 0.
      IF (MH.EQ.D1) GOTO 80
 
!-----Iteration loop
      IMAX   = 3*IINT
      DO I1  = 1,15
         FLW = (FU+FL)/2.
         DF  = (FU-FL)/2.
         IF ((DF.LT.0.01).AND.((DF/ABS(FLW)).LT.0.1)) GOTO 80
         Z   = 0.
         DO I2 = 1,IMAX
            Z  = Z+HULP(I2)/(CONDUC(I2)+FLW)
         END DO
         IF (Z.GE.D1) FL = FLW
         IF (Z.LE.D1) FU = FLW
      END DO
 
!-----Output IN MM/D
80    FLOW = 10*(FU+FL)/2.
      RETURN
 
!----In case of small matric head
 
!    NEXT LINE CHANGED
!    K0   = EXP ( ELOG10 * AFGEN (CONTAB,ILCON,-1.) )
!    START CHANGES
90    K0   = KST(I)
!    END CHANGES
!    Flow in mm/d
      FLOW = 10*K0*(MH/D-1.)
 
      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE SUERR                                                     *
!                                                                      *
! Purpose: SUERR checks whether value of variable X is within          *
!          pre-specified domain                                        *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! IMNR    I4  Message number                                        I  *
! X       R4  Value of variable to be checked (variable)            I  *
! XMIN    R4  Minimum allowable value of X (variable)               I  *
! XMAX    R4  Maximum allowable value of X (variable)               I  *
!                                                                      *
! WARNINGS:                                                            *
!                                                                      *
!   X < XMIN * 0.99 and XMIN .NE. -99 then expert message is produced  *
!   X > XMAX * 1.01 and XMAX .NE. -99 then expert message is produced  *
!                                                                      *
! SUBROUTINES called : none                                            *
!                                                                      *
! FUNCTIONS called   : none                                            *
!                                                                      *
! FILE usage         : none                                            *
!----------------------------------------------------------------------*
 
      SUBROUTINE SUERR(IMNR,X,XMIN,XMAX)
      IMPLICIT NONE

!-----Formal paramers
      INTEGER IMNR
      REAL    X, XMIN, XMAX
!-----Local variables
      CHARACTER (38) ERRM(5)
 
!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA ERRM/ &
           'MATRIC SUCTION OUT OF RANGE IN SUMSKM2', &
           'WATER CNT OUT OF RANGE IN SUSLIN      ', &
           'WATER CNT OUT OF RANGE IN SUWCMS2     ', &
           'MATRIC SUCTION OUT OF RANGE IN SUWCMS2', &
           'ONE OR MORE TRWL(I) OUT OF RANGE      '/
 
      IF ((X.LT.XMIN*0.99).AND.(XMIN.NE.-99.) .OR. &
          (X.GT.XMAX*1.01).AND.(XMAX.NE.-99.)) THEN
 
         WRITE (*,'(/,/,A,/,A,/,10X,I2,3(E10.3),/,A)') &
            ' ***fatal error in variable or parameter value ***', &
            '    message number, value, minimum and maximum: ',   &
             IMNR,X,XMIN,XMAX, &
             ERRM(IMNR)
         CALL FATALERR ('SUERRM',' ')
      END IF

      END

!----------------------------------------------------------------------*
! SUBROUTINE SUMSKM2                                                   *
!                                                                      *
! Purpose: SUMSKM2 calculates the hydraulic conductivity at            *
!          given suction for compartment I on the basis of chosen      *
!          option                                                      *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! MS      R4  Soil water suction (cm)                               I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  O  *
! KMS     R4  Hydraulic conductivity (cm d-1)                       O  *
!                                                                      *
! SUBROUTINES called : SUERR, SUWCMS2                                  *
!                                                                      *
! FUNCTIONS called   : none                                            *
!                                                                      *
! FILE usage : none                                                    *
!----------------------------------------------------------------------*
 
      SUBROUTINE SUMSKM2(I,MS,WCST,KMS)
 
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I
      REAL    MS, WCST, KMS
 
!-----Local variables
      REAL    HLP1, HLP2, HLP3, MSAD, TINY, VGM, WCL, WREL

!-----Common blocks
      INCLUDE 'COMMON_NUCHT.INC'
      INCLUDE 'COMMON_HYDCON.INC'
      INCLUDE 'COMMON_POWER.INC'
      INCLUDE 'COMMON_SWIT.INC'

!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA TINY/1.E-10/
      DATA MSAD/1.E7/
 
!-----Check input value MS
      IF (MS.LT.-TINY.OR.MS.GT.1.E8) CALL SUERR(1,MS,0.,1.E8)
 
      IF (MS.GE.MSAD-TINY) THEN
!--------Air dry
         KMS = 0.
      ELSE
!--------Calculate conductivity
         IF (SWITKH.EQ.1) THEN
!-----------Van Genuchten conductivity
            WCL = 0.
!           Dummy value; WCL is returned by SUWCMS2!
            CALL SUWCMS2(I,2,WCST,WCL,MS)
            VGM  = 1.0-1.0/VGN(I)
            WREL = (WCL-VGR(I))/(WCSTRP(I)-VGR(I))
            HLP1 = WREL**VGL(I)
            HLP2 = 1.0-WREL**(1./VGM)
            HLP3 = 1.0-HLP2**VGM
            KMS  = KST(I)*HLP1*HLP3*HLP3
         ELSE IF (SWITKH.EQ.2) THEN
!-----------Power function conductivity
            IF (MS.LE.1.) KMS = KST(I)
            IF (MS.GT.1.) KMS = KST(I)*(MS**PN(I))
         ELSE IF (SWITKH.EQ.5) THEN
!           user can here specify preferred conductivity function;
!           the following two lines should be removed:
            WRITE (*,10)
            STOP
         END IF
         IF (KMS.LT.TINY) KMS = 0.
      END IF

10    FORMAT (///,' *** fatal error; option SWIT3=5 requires ',/, &
              ' specification of conductivity function')

      RETURN
      END

!----------------------------------------------------------------------*
! SUBROUTINE SUWCHK                                                    *
!                                                                      *
! Purpose: SUWCHK checks the soil water balance by comparing           *
!          time-integrated boundary fluxes versus change in            *
!          total amount of water contained in the system.              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CKWFL   R4  Sum of time-integrated boundary fluxes (mm)           I  *
! CKWIN   R4  Change in water storage since start (mm)              I  *
! TIME    R4  Time of simulation (d)                                I  *
!                                                                      *
! SUBROUTINES called : none                                            *
!                                                                      *
! FUNCTIONS called   : none                                            *
!                                                                      *
! FILE usage :       (screen)                                          *
!----------------------------------------------------------------------*
 
      SUBROUTINE SUWCHK(CKWFL,CKWIN,TIME)
 
      IMPLICIT NONE
!-----Formal parameters
      REAL CKWFL, CKWIN, TIME

!-----Local variables
      REAL FUWCHK, XDIF

!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      FUWCHK = 2.0*(CKWIN-CKWFL)/(CKWIN+CKWFL+1.E-10)
      XDIF   = ABS(CKWIN-CKWFL)
      IF (ABS(FUWCHK).GT.0.01.AND.XDIF.GT.1.0) THEN
!-----   Absolute error in water balance exceeds 1 mm
!        and relative error exceeds 1%.
         WRITE (*,10) FUWCHK,CKWIN,CKWFL,TIME
      END IF
10    FORMAT (/'* * * error in water balance, please check * * *',/, &
              ' CKWRD  =',F6.3,' CKWIN=',F8.2,' CKWFL=',F8.2, &
              ' AT TIME = ',F6.1)
      RETURN
      END

!----------------------------------------------------------------------*
!  SUBROUTINE SUWCMS2                                                  *
!                                                                      *
!  Purpose: SUWCMS2 calculates volumetric soil water content from      *
!           soil water suction, and vice versa. Various options are    *
!           offered. See SWIT8 in input file or SAWAH manual.          *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! I       I4  Compartment index (-)                                 I  *
! SWIT4   I4  Switch to set request MS(WCL) or WCL(MS) (-)          I  *
! WCST    R4  Array of water content saturation / layer (cm3 cm-3)  I  *
! WCL     R4  Array of actual water content / layer (cm3 cm-3)     I/O *
! MS      R4  Soil water suction (cm)                              I/O *
!                                                                      *
!  SUBROUTINES called:  SUERR                                          *
!                                                                      *
!  FUNCTION called:     none                                           *
!                                                                      *
!  FILE usage:          none                                           *
!                                                                      *
!***********************************************************************
  
      SUBROUTINE SUWCMS2(I,SWIT4,WCST,WCL,MS)
 
      IMPLICIT NONE
!-----Formal parameters
      INTEGER I   , SWIT4
      REAL    WCST, WCL, MS
 
!-----Local variables
      REAL    HLP1, HLP2, HLP3, HLP4, TINY, VGM, WREL

!-----Common blocks
      INCLUDE 'COMMON_NUCHT.INC'
      INCLUDE 'COMMON_HYDCON.INC'

!-----Variables retain their values between subsequent calls
!     of this subroutine
      SAVE
 
      DATA TINY/0.001/
 
      IF (SWIT4.EQ.1) THEN
!--------Suction calculated from water content
         IF (WCL.LT.WCAD(I).OR.WCL.GT.WCST) &
              CALL SUERR(3,WCL,WCAD(I),WCST)
         IF (WCL.GT.WCSTRP(I)) THEN
!           It is assumed that MS remains zero during shrinkage
            MS = 0.
         ELSE
!-----------Van Genuchten option
            HLP1 = AMAX1(WCAD(I),WCL)
            WREL = (WCL-VGR(I))/(WCSTRP(I)-VGR(I))
            VGM  = 1.-1./VGN(I)
            HLP2 = 1./VGA(I)
            HLP3 = -1./VGM
            HLP4 = 1./VGN(I)
            MS   = HLP2*(WREL**HLP3-1.)**HLP4
         END IF
      ELSE IF (SWIT4.EQ.2) THEN
!--------Water content calculated from suction
         IF (MS.LT.-TINY.OR.MS.GT.1.E8) CALL SUERR(4,MS,0.,1.E8)
!--------Van Genuchten option
         VGM  = 1.-1./VGN(I)
         HLP1 = (MS*VGA(I))**VGN(I)
         WREL = (1.+HLP1)**(-VGM)
         WCL  = WREL*(WCSTRP(I)-VGR(I))+VGR(I)
      END IF
 
      RETURN
      END

