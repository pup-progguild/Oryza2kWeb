!----------------------------------------------------------------------*
! SUBROUTINE PADDY                                                     *
! Version : 2.0                                                        *
! Modified: B.A.M. Bouman December 2001                                *
!          Version august, 2003                                        *
! Purpose : Soil water balance model to calculate soil water content   *
!          and soil water tension at different soil layers in the      *
!          profile                                                     *
!                                                                      *
! Documented in: SARP Research Proceedings (Wopereis et al 1994)       *
!                ORYZA_W: Rice growth model for fully irrigated and    *
!                water-limited conditions                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)               C  *
! IUNITD  I4  Unit that can be used for input files (-)           C/IN *
! IUNITL  I4  Unit for log file messages (-)                      C/IN *
! FILEI2  C*  Name of input file no. 2 (-)                        C/IN *
! OUTPUT  L4  Flag to indicate if output should be done (-)        C/I *
! DOY     R4  Day number (January 1 = 1) (d)                        I  *
! DELT    R4  Time step of integration (d)                          T  *
! TIME    R4  Time of simulation (d)                                T  *
! CROPSTA I4  Crop stage (-)                                        I  *
! ESTAB   C*  Mode of establishment (-)                             I  *
! RAIN    R4  Daily amount of rainfall (mm d-1)                     I  *
! EVSC    R4  Potential soil evaporation rate (mm d-1)              I  *
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     I  *
! TRW     R4  Actual transpiration rate (mm d-1)                    I  *
! IR      R4  Amount of daily irrigation (mm d-1)                   I  *
! NL      I4  Number of soil layers (-)                             I  *
! ZRTMS   R4  Maximum rooting depth of soil profile (m)             O  *
! TKLP    R4  Array of layer thicknesses (m)                        O  *
! TKLT    R4  Depth of simulated soil (m)                           O  *
! WCAAD   R4  Array of water content at air dryness/layer (m3 m-3)  O  *
! WCWP    R4  Array of water content at wilting point/layer (m3 m-3)O  *
! WCFC    R4  Array of water content field capacity/layer (m3 m-3)  O  *
! WCST    R4  Array of water content saturation/layer (m3 m-3)      O  *
! WCL     R4  Array of actual water content/layer (m3 m-3)          O  *
! WL0     R4  Depth of ponded water (mm)                            O  *
! MSKPA   R4  Array of soil water tension/soil layer (kPa)          O  *
!                                                                      *
! SUBROUTINES used: BACKFL, DOWNFL, GWTAB, SHRINK, SUBSL2, SUERR       *
!                   SATFLX, SUWCHK, SUWCMS2                            *
!                                                                      *
! Files included  : COMMON_GWT.inc, COMMON_NUCHT.INC, COMMON_HYDCON.INC*
!                   COMMON_POWER.INC, COMMON_SWOT.INC                  *
!                                                                      *
!----------------------------------------------------------------------*
       SUBROUTINE PADDY (ITASK, IUNITD, IUNITL, FILEI2, OUTPUT, &
                      DOY,    DELT,   TIME,   CROPSTA,  ESTAB, &
                      RAIN,   EVSC,   TRWL,   TRW,      IR, &
                      NL,     ZRTMS,  TKLP,   TKLT, &
                      WCAAD,  WCWP,   WCFC,   WCST,     WCL, &
                      WL0,    MSKPA)
 
!====================================================================*
!     DECLARATIONS                                                   *
!====================================================================*

      USE CHART

        IMPLICIT NONE
!---- Formal parameters
      INTEGER       ITASK,IUNITD,IUNITL,NL,CROPSTA
      LOGICAL       OUTPUT
      CHARACTER (*) FILEI2
      REAL          DOY,DELT,TIME,RAIN,EVSC,TRW, IR
      REAL          ZRTMS,TKLT,WL0
      CHARACTER (*) ESTAB

      INTEGER      MNL
      PARAMETER   (MNL=10)
      REAL         TRWL(MNL) ,TKLP(MNL), MSKPA(MNL)
      REAL         WCAAD(MNL),WCWP(MNL),WCFC(MNL),WCST(MNL),WCL(MNL)

!-----Local variables
      CHARACTER (3) RIWCLI
      CHARACTER (5) SCODE
      INTEGER       WCLINT(3*MNL)
      INTEGER SWITVP,SWITGW
      INTEGER SWITPD,SWITPF
      INTEGER NLPUD

      REAL TKL(MNL),WLFL(MNL+1),CAPRI(MNL),GWFILL(MNL)
      REAL WCLI(MNL)
      REAL WLAD(MNL), WLFC(MNL),WLST(MNL),WL(MNL),ZL(MNL),MS(MNL)
      REAL MSUC(MNL), WLCH(MNL), KSAT(MNL)
      REAL FLOW
 
      REAL VL(MNL)  , TOTPOR(MNL)
      REAL WCCR
 
      LOGICAL CRACKS,GRWAT,INQOBS,PUDDLD,RWCLI
      INTEGER I, IGW, I2, J ,K

      INCLUDE 'COMMON_GWT.INC'
      INCLUDE 'COMMON_NUCHT.INC'
      INCLUDE 'COMMON_HYDCON.INC'
      INCLUDE 'COMMON_POWER.INC'
      INCLUDE 'COMMON_SWIT.INC'

      REAL CAPTOT, CKWIN  , CKWFL , DRAIN , DSPW 
      REAL EVSD  , EVSH   , EVSW  , EVSWS
      REAL FACT  , FIXPERC, FLNEW , GWCHK , GWCUM1  , GWTOT
      REAL PERC  , PERCOL, PFCR  , PROREL
      REAL REST  , RUNOF 
      REAL SURREL, TINY   , UPRICUM1   
      REAL WCUM  , WCUMI  , WCUMCH, WCUMCO
      REAL WL0I  , WL0CH  , WL0CO , WL0FCUM1, WL0FILL, WL0MX
      REAL ZW    , ZWPREV
      REAL CAPTOTCUM1, DRAICUM1, EVSWCUM1, TRWCUM1, RAINCUM1, IRCUM1
      REAL RUNOFCUM1,PERCCUM1 
      REAL CAPTOTCUM2, DRAICUM2, EVSWCUM2, TRWCUM2, RAINCUM2, IRCUM2
      REAL RUNOFCUM2, PERCCUM2 
      REAL CAPTOTCUM3, DRAICUM3, EVSWCUM3, TRWCUM3, RAINCUM3, IRCUM3
      REAL RUNOFCUM3,PERCCUM3 
         
!-----Used functions
      REAL GETOBS, LINT2, INSW, INTGRL, INTGR2

!-----LINT functions
      INTEGER   ILPMAX, IPERTB
      PARAMETER (ILPMAX=700)
      REAL PERTB(ILPMAX)

! Bouman: Percolation rate can vary in time
      INTEGER IMX, IPTABLE
      PARAMETER (IMX=20)
      REAL PTABLE(IMX)

      SAVE

      IF (ITASK.EQ.1) THEN
!====================================================================*
!     INITIALIZATION                                                 *
!====================================================================*
         CRACKS = .FALSE.
         PUDDLD = .FALSE.
         GRWAT  = .FALSE.
         RWCLI  = .FALSE.
         ZW     = 0.
         PERC   = 0.
         TINY   = 1.0E-5

!----- Read input from soil data file
         CALL RDINIT(IUNITD,IUNITL,FILEI2)

!----- Read code to recognize the correctness of supplied soil file
         CALL RDSCHA('SCODE',SCODE)
         IF (SCODE .NE. 'PADDY') THEN
            CALL FATALERR ('PADDY','Wrong soil input file for PADDY water balance')
         END IF

!----- Puddled / non-puddled switch
         CALL RDSINT('SWITPD',SWITPD)
         IF (SWITPD.EQ.1) THEN
            PUDDLD = .TRUE.
            CALL OUTCOM('PADDY: Puddled soil (SWITPD=1)')
         ELSE IF (SWITPD.EQ.0) THEN
            PUDDLD = .FALSE.
            CALL OUTCOM('PADDY: Non-puddled soil (SWITPD=0)')
         ELSE
            CALL FATALERR ('PADDY','Unknown switch for SWITPD')
         END IF

!---- Groundwater present / not present switch
         CALL RDSINT('SWITGW',SWITGW)
         IF (SWITGW.EQ.1.OR.SWITGW.EQ.2) THEN
            GRWAT = .TRUE.
       CALL OUTCOM('PADDY: Groundwater may be present (SWITGW=1 OR 2)')
         ELSE IF (SWITGW.EQ.0) THEN
            GRWAT = .FALSE.
            CALL OUTCOM('PADDY: Groundwater below profile (SWITGW=0)')
         ELSE
            CALL FATALERR ('PADDY','Unknown switch for SWITGW')
         END IF

!------- PF switch
         CALL RDSINT('SWITPF',SWITPF)

! BB: Added SWITVP=2
!------- Variable percolation rate switch
         CALL RDSINT('SWITVP',SWITVP)
         IF (SWITVP.EQ.0) THEN
            CALL OUTCOM('PADDY: Percolation as function of groundwater (SWITVP=0)')
         ELSE IF (SWITVP.EQ.-1) THEN
            CALL OUTCOM('PADDY: Fixed percolation (SWITVP=-1)')
         ELSE IF (SWITVP.EQ.2) THEN
            CALL OUTCOM('PADDY: Fixed percolation as function of time (SWITVP=2)')
         ELSE IF (SWITVP.EQ.1) THEN
            IF (SWITPD.EQ.0) THEN
               CALL FATALERR ('PADDY','Check SWITVP-SWITPD combination')
            END IF
            CALL OUTCOM('PADDY: Percolation calculated (SWITVP=1)')
         ELSE
            CALL FATALERR ('PADDY','Unknown switch for SWITVP')
         END IF

         CALL RDSINT('NL',NL)
         IF (NL.GT.10) CALL FATALERR ('PADDY','too many soil layers NL')
 
         CALL RDFREA('TKL'  ,TKL ,MNL,NL)
         CALL RDFREA('WCLI' ,WCLI,MNL,NL)
         CALL RDSCHA('RIWCLI',RIWCLI)
         CALL RDFINT('WCLINT',WCLINT,3*MNL,3*NL)
         IF (RIWCLI.EQ.'YES') THEN
            RWCLI=.TRUE.
         ELSE IF (RIWCLI.EQ.'NO') THEN
            RWCLI=.FALSE.
         ELSE
            CALL FATALERR ('PADDY','unknown selection for Yes or NO')
         END IF

         IF (SWITPF.EQ.1) THEN
            CALL RDFREA('VGA' ,VGA ,MNL,NL)
            CALL RDFREA('VGL' ,VGL ,MNL,NL)
            CALL RDFREA('VGN' ,VGN ,MNL,NL)
            CALL RDFREA('VGR' ,VGR ,MNL,NL)
            CALL RDFREA('WCST',WCST,MNL,NL)
         ELSE IF (SWITPF.EQ.0) THEN
!------- Only moisture contents at sat., fc, wp, and air dry given
            CALL RDFREA('WCST',WCST,MNL,NL)
            CALL RDFREA('WCFC',WCFC,MNL,NL)
            CALL RDFREA('WCWP',WCWP,MNL,NL)
            CALL RDFREA('WCAD',WCAD, 10,NL)
! Fill air dryness water content array WCAAD to avoid 
! errors in argument list/COMMON block with WCAD
            DO I=1,NL
              WCAAD(I)=WCAD(I)
            END DO
         ELSE
            CALL FATALERR ('PADDY','Unknown switch for SWITPF')
         END IF

!---- Maximum rooting depth soil
         CALL RDSREA('ZRTMS',ZRTMS)
!---- Bund height
         CALL RDSREA('WL0MX',WL0MX)
!---- Initial ponded water depth
         CALL RDSREA('WL0I',WL0I)
!---- Saturated hyraulic conductivity
         CALL RDFREA('KST',KST,10,NL)
! BB: Added SWITVP=2
!---- Percolation rate 
         IF (SWITVP.EQ.-1) THEN
             CALL RDSREA('FIXPERC',FIXPERC)
         ELSE IF (SWITVP.EQ.0) THEN
             CALL RDAREA('PERTB',PERTB,ILPMAX,IPERTB)
         ELSE IF (SWITVP.EQ.2) THEN
             CALL RDAREA('PTABLE',PTABLE,IMX,IPTABLE)
         END IF
 
!---- Number of puddled soil compartments
         IF (PUDDLD) THEN
            CALL RDSINT('NLPUD',NLPUD)
            IF (NLPUD.GE.NL) CALL FATALERR('PADDY','NLPUD CANNOT BE GREATER THAN NL')
            IF (NLPUD.LE.0 ) CALL FATALERR('PADDY','NLPUD MUST BE GREATER THAN 0')
         END IF
!---- Volumetric water contents of ripened previously puddled
!     compartments
         IF (PUDDLD) CALL RDFREA('WCSTRP',WCSTRP,MNL,NL)
 
!---- Critical pF value for cracking
         IF (PUDDLD) CALL RDSREA('PFCR',PFCR)
 
!------- KH switch
         CALL RDSINT('SWITKH',SWITKH)
         IF (SWITKH.NE.0.AND.SWITKH.NE.1.AND.SWITKH.NE.2) &
              CALL FATALERR('PADDY','PLEASE CHECK VALUE SWITKH IN SOIL DATA FILE')
         IF (SWITKH.EQ.0) THEN
            IF (SWITVP.EQ.1) CALL FATALERR &
            ('PADDY','PLEASE CHECK VALUES OF SWITKH AND SWITVP IN SOIL DATA FILE')
         END IF
         IF (SWITKH.EQ.1) THEN
!-----------KH defined in terms of Van Genuchten parameters
            CALL RDFREA('KST',KST,10,NL)
            CALL RDFREA('VGA',VGA,10,NL)
            CALL RDFREA('VGL',VGL,10,NL)
            CALL RDFREA('VGN',VGN,10,NL)
            CALL RDFREA('VGR',VGR,10,NL)
         ELSE IF (SWITKH.EQ.2) THEN
!-----------KH defined in terms of power function
            CALL RDFREA('KST',KST,10,NL)
            CALL RDFREA('PN' ,PN ,10,NL)
         END IF
 
         IF (GRWAT) THEN
            IF (SWITGW.EQ.1) THEN
               CALL RDAREA('ZWTB',ZWTB,ILZMAX,IZWTB)
            ELSE IF (SWITGW.EQ.2) THEN
               CALL RDSREA('MAXGW',MAXGW)
               CALL RDSREA('MINGW',MINGW)
               CALL RDSREA('ZWA'  ,ZWA  )
               CALL RDSREA('ZWB'  ,ZWB  )
               CALL RDSREA('ZWTBI',ZWTBI)
               IF (MINGW.GT.MAXGW) CALL FATALERR &
                  ('PADDY','PLEASE CHECK MINGW OR MAXGW IN SOIL DATA FILE')
            END IF
         END IF
 
!------- Reading of soil data completed
         CLOSE (IUNITD)
 
         I = 1
         DO WHILE (I.LE.NL)
            KSAT(I) = KST(I)
            IF (.NOT.PUDDLD) WCSTRP(I) = WCST(I)
            I = I+1
         END DO
 
         IF (SWITPF.EQ.1) THEN
            I = 1
            DO WHILE (I.LE.NL)
               CALL SUWCMS2(I,2,WCST(I),WCFC(I),100. )
               CALL SUWCMS2(I,2,WCST(I),WCWP(I),1.6E4)
               CALL SUWCMS2(I,2,WCST(I),WCAD(I),1.0E7)
               I = I+1
            END DO
         END IF
 
         I = 1
         DO WHILE (I.LE.NL)
            IF (WCLI(I).LT.WCAD(I).OR.WCLI(I).GT.WCST(I)) &
                 CALL SUERR(3,WCLI(I),WCAD(I),WCST(I))
            IF (WCSTRP(I).GT.WCST(I)) CALL FATALERR &
                 ('PADDY','PLEASE CHECK VALUES WCSTRP AND WCST')
            I = I+1
         END DO
 
         TKLT = 0.
 
!---- Convert TKL from m into mm; calculate water contents in mm
         I = 1
         DO WHILE (I.LE.NL)
! For ORYZA1 subroutine   (dimension: m)
            TKLP(I) = TKL(I)
            TKLT    = TKLT+TKL(I)
! End for ORYZA1 subroutine
            TKL(I)  = 1000*TKL(I)
            WLFC(I) = WCFC(I)*TKL(I)
            WLAD(I) = WCAD(I)*TKL(I)
            WLST(I) = WCST(I)*TKL(I)
            WL(I)   = WCLI(I)*TKL(I)
            I = I+1
         END DO
 
!------- Initialize SHRINK subroutine
         IF (PUDDLD) THEN
!------- Calculate water content when cracks penetrate through a
!        soil compartment
            IF (SWITPF.EQ.1) THEN
               CALL SUWCMS2(NLPUD,2,WCST(NLPUD),WCCR,10**PFCR)
            ELSE
               IF (PFCR.LE.4.2.AND.PFCR.GE.0.) THEN
                  WCCR = WCWP(NLPUD)+((WCFC(NLPUD)-WCWP(NLPUD))/2.2)* &
                         (4.2-PFCR)
               ELSE IF (PFCR.GT.4.2.AND.PFCR.LE.7.) THEN
                  WCCR = WCAD(NLPUD)+((WCWP(NLPUD)-WCAD(NLPUD))/2.8)* &
                         (7.0-PFCR)
               ELSE
                  CALL FATALERR ('PADDY','PLEASE CHECK VALUE PFCR IN SOIL DATA FILE')
               END IF
            END IF
 
            I = 1
            DO WHILE (I.LE.NL.AND.I.LE.NLPUD)
               CALL SHRINK(ITASK,MNL,I,WL(I),TKL(I),WCST(I),WCSTRP(I),WCL(I), &
                           TOTPOR(I),VL(I))
               I = I+1
            END DO
          END IF
 
!----- Depth of top of compartments
         I = 1
         DO WHILE (I.LE.NL)
            IF (I.EQ.1) THEN
               ZL(I) = 0.
            ELSE
               ZL(I) = ZL(I-1)+TKL(I-1)/10.
            END IF
            I = I+1
         END DO
 
!------- Check groundwater table depth
         IF (GRWAT) CALL GWTAB (ITASK,SWITGW,NL,DOY,DELT,WLFL,TKL,ZWPREV, &
                                IGW,ZW)
 
!---- Initialization of state variables
 
!------- Initial ponded water depth (mm)
         WL0 = WL0I
 
!------- Initial (total) water content in soil profile (mm)
         I = 1
         DO WHILE (I.LE.NL)
            WCL(I) = WCLI(I)
            WCUMI  = WCUMI+WL(I)
            I = I+1
         END DO

         WCUM = WCUMI
 
!----- Set days since last ponded water
       DSPW = 1.
 
!------- Set cumulative amounts
         WCUMCO = 0.
         WL0CO  = 0.
         WL0FCUM1 = 0.
         UPRICUM1 = 0.
         GWCUM1   = 0.

         PERCCUM1 = 0.
         CAPTOTCUM1 = 0.
         RAINCUM1  = 0.
         IRCUM1   = 0.
         RUNOFCUM1 = 0.
         EVSWCUM1 = 0.
         TRWCUM1  = 0.
         DRAICUM1 = 0.

         PERCCUM2 = 0.
         CAPTOTCUM2 = 0.
         RAINCUM2  = 0.
         IRCUM2   = 0.
         RUNOFCUM2 = 0.
         EVSWCUM2 = 0.
         TRWCUM2  = 0.
         DRAICUM2 = 0.

         PERCCUM3 = 0.
         CAPTOTCUM3 = 0.
         RAINCUM3  = 0.
         IRCUM3   = 0.
         RUNOFCUM3 = 0.
         EVSWCUM3 = 0.
         TRWCUM3  = 0.
         DRAICUM3 = 0.

  !====================================================================*
!     RATE CALCULATION SECTION                                       *
!====================================================================*
      ELSE IF (ITASK.EQ.2) THEN

!-----Reinitialize water contents at direct seeding or transplanting
!     if requested
        IF (RWCLI) THEN
           IF ((ESTAB.EQ.'TRANSPLANT'  .AND. CROPSTA.EQ.3) .OR. &
               (ESTAB.EQ.'DIRECT-SEED' .AND. CROPSTA.EQ.1)) THEN
             WL0 = MIN(WL0I,WL0MX)
             DO I=1,NL
                WCL(I) = WCST(I)
                WL(I)  = WCL(I)*TKL(I)
             END DO
           END IF
        END IF

         WL0CH  = 0.
         WCUMCH = 0.
         RUNOF  = 0.
         EVSW   = 0.
         EVSWS  = 0.
         CAPTOT = 0.
         GWTOT  = 0.
         DRAIN  = 0.
 
!------- Reset rates to 0
         I = 1
         DO WHILE (I.LE.NL)
            WLFL(I)  = 0.
            WLCH(I)  = 0.
            CAPRI(I) = 0.
            MS(I)    = 0.
!            MSUC(I)  = 0.
!            MSKPA(I) = 0.
            GWFILL(I)= 0.
            I = I+1
         END DO
         WLFL(I) = 0.

!---- Set percolation to zero:
         PERC = 0.

!------- 1. Ponded water on field
         IF (WL0.GE.TINY) THEN
 
!---------- Reset number of days after ponded water
            DSPW = 1.
 
!---------- 1.1 Ponded water can sustain evaporation and transpiration
            IF (WL0/DELT+RAIN+IR.GE.EVSC+TRW) THEN
 
!------------- Calculate change in ponded water depth (mm/d)
               WL0CH = RAIN+IR-EVSC-TRW
 
!------------- Reset transpiration losses per soil layer at zero
!------------- as transpiration is taken from ponded water
               I = 1
               DO WHILE (I.LE.NL)
                  TRWL(I) = 0
                  I = I+1
               END DO
 
!------------- For water balance check
               EVSW  = EVSC
               EVSWS = 0.

! BB: Added SWITVP=2
!------------- Set fixed percolation rate PERCOL
               IF (SWITVP.EQ.-1) THEN
                  PERCOL = FIXPERC
               ELSE IF (SWITVP.EQ.0) THEN
                  PERCOL = LINT2('PERTB',PERTB,IPERTB,ZW)
               ELSE IF (SWITVP.EQ.2) THEN
                  PERCOL = LINT2('PTABLE',PTABLE,IPTABLE,TIME)
               END IF

!--------------For nonpuddled soils and puddled soils with no cracks
               IF (.NOT.CRACKS) THEN
!--------------Calculate percolation rate (mm/d)
                  IF (SWITVP.EQ.0 .OR. SWITVP.EQ.-1 .OR. SWITVP.EQ.2) THEN
                     IF (WL0/DELT+WL0CH.GE.PERCOL) THEN
                        PERC = PERCOL
                     ELSE
                        PERC = WL0/DELT+WL0CH
                     END IF
                  ELSE
                     CALL SATFLX(TKL,NLPUD,WL0,PERC)
                     IF (WL0/DELT+WL0CH.LE.PERC) PERC = WL0/DELT+WL0CH
                  END IF

!---------- Recalculate change in ponded water depth (mm/d)
                  WL0CH = WL0CH-PERC
 
!---------- Calculate runoff (mm/d) if ponded water depth
!---------- exceeds bund height
                  IF (WL0+WL0CH*DELT.GE.WL0MX) THEN
                     RUNOF = (WL0+WL0CH*DELT-WL0MX)/DELT
                     WL0CH = WL0CH-RUNOF
                  END IF
 
                  I = 1
                  DO WHILE (I.LE.NL+1)
                     WLFL(I) = PERC
                     I = I+1
                  END DO
 
!--------------For puddled soils with cracks 
               ELSE
 
!---------- Calculate flow through boundaries of soil layers
                  WLFL(1) = RAIN+IR-EVSC-TRW
                  I = 1
                  DO WHILE (I.LE.NL)
                     CALL DOWNFL(I,KSAT(I),WLFL(I),TRWL(I),EVSWS,WL(I), &
                                 WLFC(I),DELT,WLFL(I+1))
                     I = I+1
                  END DO
 
                  I = NL
                  DO WHILE (I.GE.1)
                     CALL BACKFL(I,WL(I),WLFL(I),WLFL(I+1),EVSWS, &
                                 TRWL(I),WLST(I),DELT,FLNEW,REST)
                     WLFL(I) = FLNEW
                     I = I-1
                  END DO
                  WL0CH = MAX(0.,(REST-WLST(1))/DELT)
                  IF (WL0+WL0CH*DELT.GE.WL0MX) THEN
                     RUNOF = (WL0+WL0CH*DELT-WL0MX)/DELT
                     WL0CH = WL0CH-RUNOF
                  END IF
               END IF
 
!---------- 1.2 Ponded water depth can sustain evaporation but
!----------     only part of transpiration
 
            ELSE IF ((WL0/DELT+RAIN+IR.GE.EVSC).AND. &
                     (WL0/DELT+RAIN+IR.LT.EVSC+TRW)) THEN
 
!------------- Calculate change in ponded water depth (mm/d)
               WL0CH = -WL0/DELT
 
!------------- Percolation is zero because no ponded water left
               PERC = 0.
               I = 1
               DO WHILE (I.LE.NL+1)
                  WLFL(I) = PERC
                  I = I+1
               END DO
 
!------------- Correct transpiration losses per soil layer as
!------------- transpiration losses are partly covered by ponded water
 
               I = 1
               DO WHILE (I.LE.NL)
                  TRWL(I) = ((TRW+EVSC-RAIN-IR-WL0/DELT)/TRW)*TRWL(I) &
                            *DELT
                  I = I+1
               END DO
 
!------------- For water balance check
               EVSW  = EVSC
               EVSWS = 0.
 
!---------- 1.3 Ponded water can sustain part of evaporation only
            ELSE IF (WL0/DELT+RAIN+IR.LT.EVSC) THEN
 
!------------- Calculate change in ponded water depth (mm/d)
               WL0CH   = -WL0/DELT
               PERC    = 0.
               WLFL(1) = RAIN+IR
               I = 2
               DO WHILE (I.LE.NL+1)
                  WLFL(I) = PERC
                  I = I+1
               END DO
 
!------------- Calculate contribution of first soil layer to
!------------- evaporation
               EVSW  = MIN(EVSC+WL0CH,WL(1)/DELT-WLAD(1)/DELT+RAIN+IR)
               EVSWS = EVSW

!------------- For water balance check
               EVSW = WL0/DELT+EVSWS
 
            END IF
 
         ELSE
 
!---------- 2. No ponded water on surface
!---------- Calculate evaporation rate from soil surface (mm/d)
            EVSH = MIN(EVSC,MAX(0.,(WL(1)-WLAD(1))/DELT+RAIN+IR))
            EVSD = MIN(EVSC,0.6*EVSC*(SQRT(DSPW)-SQRT(DSPW-1.))+RAIN+IR)
            EVSW = INSW(DSPW-1.1,EVSH,EVSD)
            EVSW = MIN(EVSW,MAX(0.,RAIN+IR+(WL(1)-WLAD(1))/DELT))
 
            EVSWS   = EVSW
            DSPW    = DSPW+1.
            WLFL(1) = RAIN+IR
 
            I = 1
            DO WHILE (I.LE.NL)
               CALL DOWNFL(I,KSAT(I),WLFL(I),TRWL(I),EVSWS,WL(I),WLFC(I), &
                           DELT,WLFL(I+1))
               I = I+1
            END DO
 
            I = NL
            DO WHILE (I.GE.1)
               CALL BACKFL(I,WL(I),WLFL(I),WLFL(I+1),EVSWS,TRWL(I), &
                           WLST(I),DELT,FLNEW,REST)
               WLFL(I) = FLNEW
               I = I-1
            END DO
 
            WL0CH = MAX(0.,(REST-WLST(1))/DELT)

            IF (WL0+WL0CH*DELT.GE.WL0MX) THEN
               RUNOF = (WL0+WL0CH*DELT-WL0MX)/DELT
               WL0CH = WL0CH-RUNOF
            END IF
         END IF
 
         IF (GRWAT) THEN
!------- Drain compartments in groundwater
            I = IGW
            DO WHILE (I.LE.NL)
               IF (WL(I).GE.WLFC(I)) THEN
                  DRAIN = (WL(I)-WLFC(I))/DELT
                  WLFL(I+1) = DRAIN+MAX(0.,WLFL(I)-TRWL(I))
               ELSE
                  WLFL(I+1) = MAX(0.,WLFL(I)-TRWL(I)+(WL(I)-WLFC(I)) &
                              /DELT)
               END IF
               I = I+1
            END DO
 
            I = NL
            GWTOT = 0.
            WL0FILL = 0.
            DO WHILE (I.GE.1)
               GWFILL(I) = 0.
!---------- Check if groundwater in soil layer
!---------- If groundwater table is negative it is assumed that
!---------- this represents water on the soil surface
               IF ((ZW.LT.0).AND.(-10*ZW.GT.WL0+WL0CH)) THEN 
                  WL0FILL = -10*ZW-(WL0+WL0CH)
               END IF
               GWCHK = MAX(0.,ZW-ZL(I)-0.5*TKL(I)/10.)
               IF (GWCHK.EQ.0.) THEN
                  IF (I.EQ.1) THEN
                     GWFILL(I) = MAX(0.,(WLST(I)-WL(I))/DELT+TRWL(I)+ &
                                 WLFL(I+1)+EVSWS-WLFL(I))
                  ELSE
                     GWFILL(I) = MAX(0.,(WLST(I)-WL(I))/DELT+TRWL(I)+ &
                                 WLFL(I+1)-WLFL(I))
                  END IF
                  GWTOT = GWTOT+GWFILL(I)
               END IF
 
!------ Capillary rise
               FLOW = 0.
               IF (WL(I).GT.WLAD(I).AND.WL(I).LT.WLFC(I).AND.ZW.GT.ZL(I) &
                   +TKL(I)/10.) THEN
                  IF ((SWITKH.NE.0).AND.(SWITPF.EQ.0)) THEN
                     IF (WCL(I).GE.WCFC(I)) THEN
                        FACT = MAX(0., &
                               MIN(1.,(WCST(I)-WCL(I))/(WCST(I)-WCFC(I))))
                        MS(I) = 10.**(FACT*2.0)
                        IF (WCL(I).GE.WCST(I)) MS(I) = 0.
                     ELSE IF (WCL(I).GE.WCWP(I).AND.WCL(I).LT.WCFC(I)) THEN 
                        FACT = MAX(0., &
                               MIN(1.,(WCL(I)-WCWP(I))/(WCFC(I)-WCWP(I))))
                        MS(I) = 10.**(4.2-FACT*2.2)
                     ELSE IF (WCL(I).LT.WCWP(I)) THEN
                        FACT = MAX(0., &
                               MIN(1.,(WCL(I)-WCAD(I))/(WCWP(I)-WCAD(I))))
                        MS(I) = 10.**(7.0-FACT*2.8)
                     END IF
                  END IF
                  IF ((SWITKH.NE.0).AND.(SWITPF.EQ.1)) &
                    CALL SUWCMS2(I,1,WCST(I),WCL(I),MS(I))
                  IF (MS(I).GT.100.) &
                    CALL SUBSL2(LOG10(MS(I)),ZW-ZL(I)+0.5*TKL(I)/10.,I, &
                      WCST(I),FLOW)
!                 If flow negative (percolation) then reset at zero
                  IF (FLOW.LT.0) FLOW = 0.
                  IF (I.EQ.1) THEN
                     CAPRI(I) = MIN(FLOW,(WLST(I)-WL(I))/DELT+EVSWS+TRWL &
                                (I)+WLFL(I+1)-WLFL(I))
                  ELSE
                     CAPRI(I) = MIN(FLOW,(WLST(I)-WL(I))/DELT+TRWL(I)+ &
                                WLFL(I+1)-WLFL(I))
                  END IF
 
               END IF
               CAPTOT = CAPTOT+CAPRI(I)
               I = I-1
            END DO
 
         END IF
 
         I = 1
         DO WHILE (I.LE.NL)
            IF (I.EQ.1) THEN
               WLCH(I) = WLFL(I)-WLFL(I+1)-TRWL(I)-EVSWS+CAPRI(I) &
                        +GWFILL(I)
            ELSE
               WLCH(I) = WLFL(I)-WLFL(I+1)-TRWL(I)+CAPRI(I)+GWFILL(I)
            END IF
            WCUMCH = WCUMCH+WLCH(I)
            I = I+1
         END DO

         IF (OUTPUT) THEN
            CALL OUTDAT(2,0,'WL0',WL0)
            CALL ChartOutputRealScalar('WL0',WL0)
            CALL OUTDAT(2,0,'ZW',ZW)
            CALL ChartOutputRealScalar('ZW',-ZW)
            CALL OUTDAT(2,0,'IR',IR)
            CALL ChartOutputRealScalar('IR',IR)
            CALL OUTDAT(2,0,'IRCUM2',IRCUM2)
            CALL ChartOutputRealScalar('IRCUM2',IRCUM2)
            CALL OUTDAT(2,0,'RAINCUM2',RAINCUM2)
            CALL ChartOutputRealScalar('RAINCUM2',RAINCUM2)
!            CALL OUTDAT(2,0,'RUNOFCUM2',RUNOFCUM2)
!            CALL ChartOutputRealScalar('RUNOFCUM2',RUNOFCUM2)
            CALL OUTDAT(2,0,'EVSW',EVSW)
            CALL ChartOutputRealScalar('EVSW',EVSW)
            CALL OUTDAT(2,0,'EVSWCUM2',EVSWCUM2)
            CALL ChartOutputRealScalar('EVSWCUM2',EVSWCUM2)
            CALL OUTDAT(2,0,'TRWCUM2',TRWCUM2)
            CALL ChartOutputRealScalar('TRWCUM2',TRWCUM2)
            CALL OUTDAT(2,0,'GWTOT',GWTOT)
            CALL ChartOutputRealScalar('GWTOT',GWTOT)
            CALL OUTDAT(2,0,'CAPTOT',CAPTOT)
            CALL ChartOutputRealScalar('CAPTOT',CAPTOT)
!            CALL OUTDAT(2,0,'WLCH(NL)',WLCH(NL))
!            CALL ChartOutputRealScalar('WLCH(NL)',WLCH(NL))
            IF (NL.GE.1) CALL OUTDAT(2,0,'MSKPA1',MSKPA(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA1',MSKPA(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'MSKPA2',MSKPA(2))
            IF (NL.GE.2) CALL ChartOutputRealScalar('MSKPA2',MSKPA(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'MSKPA3',MSKPA(3))
            IF (NL.GE.3) CALL ChartOutputRealScalar('MSKPA3',MSKPA(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'MSKPA4',MSKPA(4))
            IF (NL.GE.4) CALL ChartOutputRealScalar('MSKPA4',MSKPA(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'MSKPA5',MSKPA(5))
            IF (NL.GE.5) CALL ChartOutputRealScalar('MSKPA5',MSKPA(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'MSKPA6',MSKPA(6))
            IF (NL.GE.6) CALL ChartOutputRealScalar('MSKPA6',MSKPA(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'MSKPA7',MSKPA(7))
            IF (NL.GE.7) CALL ChartOutputRealScalar('MSKPA7',MSKPA(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'MSKPA8',MSKPA(8))
            IF (NL.GE.8) CALL ChartOutputRealScalar('MSKPA8',MSKPA(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'MSKPA9',MSKPA(9))
            IF (NL.GE.9) CALL ChartOutputRealScalar('MSKPA9',MSKPA(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'MSKPA10',MSKPA(10))
            IF (NL.GE.10) CALL ChartOutputRealScalar('MSKPA10',MSKPA(10))

            IF (NL.GE.1) CALL OUTDAT(2,0,'WCL1',WCL(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('WCL1',WCL(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'WCL2',WCL(2))
            IF (NL.GE.2) CALL ChartOutputRealScalar('WCL2',WCL(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'WCL3',WCL(3))
            IF (NL.GE.3) CALL ChartOutputRealScalar('WCL3',WCL(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'WCL4',WCL(4))
            IF (NL.GE.4) CALL ChartOutputRealScalar('WCL4',WCL(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'WCL5',WCL(5))
            IF (NL.GE.5) CALL ChartOutputRealScalar('WCL5',WCL(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'WCL6',WCL(6))
            IF (NL.GE.6) CALL ChartOutputRealScalar('WCL6',WCL(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'WCL7',WCL(7))
            IF (NL.GE.7) CALL ChartOutputRealScalar('WCL7',WCL(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'WCL8',WCL(8))
            IF (NL.GE.8) CALL ChartOutputRealScalar('WCL8',WCL(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'WCL9',WCL(9))
            IF (NL.GE.9) CALL ChartOutputRealScalar('WCL9',WCL(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'WCL10',WCL(10))
            IF (NL.GE.10) CALL ChartOutputRealScalar('WCL10',WCL(10))

            IF (NL.GE.1) CALL OUTDAT(2,0,'WLFL1',WLFL(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('WLFL1',WLFL(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'WLFL2',WLFL(2))
            IF (NL.GE.2) CALL ChartOutputRealScalar('WLFL2',WLFL(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'WLFL3',WLFL(3))
            IF (NL.GE.3) CALL ChartOutputRealScalar('WLFL3',WLFL(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'WLFL4',WLFL(4))
            IF (NL.GE.4) CALL ChartOutputRealScalar('WLFL4',WLFL(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'WLFL5',WLFL(5))
            IF (NL.GE.5) CALL ChartOutputRealScalar('WLFL5',WLFL(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'WLFL6',WLFL(6))
            IF (NL.GE.6) CALL ChartOutputRealScalar('WLFL6',WLFL(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'WLFL7',WLFL(7))
            IF (NL.GE.7) CALL ChartOutputRealScalar('WLFL7',WLFL(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'WLFL8',WLFL(8))
            IF (NL.GE.8) CALL ChartOutputRealScalar('WLFL8',WLFL(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'WLFL9',WLFL(9))
            IF (NL.GE.9) CALL ChartOutputRealScalar('WLFL9',WLFL(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'WLFL10',WLFL(10))
            IF (NL.GE.10) CALL ChartOutputRealScalar('WLFL10',WLFL(10)) 
            CALL OUTDAT(2,0,'WLFLOUT',WLFL(NL+1))
            CALL ChartOutputRealScalar('WLFLOUT',WLFL(NL+1))

            IF (NL.GE.1) CALL OUTDAT(2,0,'CAPRI1',CAPRI(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('CAPRI1',CAPRI(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'CAPRI2',CAPRI(2))
            IF (NL.GE.2) CALL ChartOutputRealScalar('CAPRI2',CAPRI(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'CAPRI3',CAPRI(3))
            IF (NL.GE.3) CALL ChartOutputRealScalar('CAPRI3',CAPRI(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'CAPRI4',CAPRI(4))
            IF (NL.GE.4) CALL ChartOutputRealScalar('CAPRI4',CAPRI(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'CAPRI5',CAPRI(5))
            IF (NL.GE.5) CALL ChartOutputRealScalar('CAPRI5',CAPRI(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'CAPRI6',CAPRI(6))
            IF (NL.GE.6) CALL ChartOutputRealScalar('CAPRI6',CAPRI(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'CAPRI7',CAPRI(7))
            IF (NL.GE.7) CALL ChartOutputRealScalar('CAPRI7',CAPRI(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'CAPRI8',CAPRI(8))
            IF (NL.GE.8) CALL ChartOutputRealScalar('CAPRI8',CAPRI(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'CAPRI9',CAPRI(9))
            IF (NL.GE.9) CALL ChartOutputRealScalar('CAPRI9',CAPRI(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'CAPRI10',CAPRI(10))
            IF (NL.GE.10) CALL ChartOutputRealScalar('CAPRI10',CAPRI(10))

             IF (INQOBS (FILEI2,'WL0')) THEN
                    CALL OUTDAT (2, 0, 'WL0_OBS',GETOBS(FILEI2,'WL0'))
                    CALL ChartOutputRealScalar('WL0_OBS',GETOBS(FILEI2,'WL0'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL1')) THEN
                    CALL OUTDAT (2, 0, 'WCL1_OBS',GETOBS(FILEI2,'WCL1'))
                    CALL ChartOutputRealScalar('WCL1_OBS',GETOBS(FILEI2,'WCL1'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL2')) THEN
                    CALL OUTDAT (2, 0, 'WCL2_OBS',GETOBS(FILEI2,'WCL2'))
                    CALL ChartOutputRealScalar('WCL2_OBS',GETOBS(FILEI2,'WCL2'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL3')) THEN
                    CALL OUTDAT (2, 0, 'WCL3_OBS',GETOBS(FILEI2,'WCL3'))
                    CALL ChartOutputRealScalar('WCL3_OBS',GETOBS(FILEI2,'WCL3'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL4')) THEN
                    CALL OUTDAT (2, 0, 'WCL4_OBS',GETOBS(FILEI2,'WCL4'))
                    CALL ChartOutputRealScalar('WCL4_OBS',GETOBS(FILEI2,'WCL4'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL5')) THEN
                    CALL OUTDAT (2, 0, 'WCL5_OBS',GETOBS(FILEI2,'WCL5'))
                    CALL ChartOutputRealScalar('WCL5_OBS',GETOBS(FILEI2,'WCL5'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL6')) THEN
                    CALL OUTDAT (2, 0, 'WCL6_OBS',GETOBS(FILEI2,'WCL6'))
                    CALL ChartOutputRealScalar('WCL6_OBS',GETOBS(FILEI2,'WCL6'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL7')) THEN
                    CALL OUTDAT (2, 0, 'WCL7_OBS',GETOBS(FILEI2,'WCL7'))
                    CALL ChartOutputRealScalar('WCL7_OBS',GETOBS(FILEI2,'WCL7'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL8')) THEN
                    CALL OUTDAT (2, 0, 'WCL8_OBS',GETOBS(FILEI2,'WCL8'))
                    CALL ChartOutputRealScalar('WCL8_OBS',GETOBS(FILEI2,'WCL8'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL9')) THEN
                    CALL OUTDAT (2, 0, 'WCL9_OBS',GETOBS(FILEI2,'WCL9'))
                    CALL ChartOutputRealScalar('WCL9_OBS',GETOBS(FILEI2,'WCL9'))
                  ENDIF
            IF (INQOBS (FILEI2,'WCL10')) THEN
                      CALL OUTDAT (2, 0, 'WCL10_OBS',GETOBS(FILEI2,'WCL10'))
                    CALL ChartOutputRealScalar('WCL10_OBS',GETOBS(FILEI2,'WCL10'))
                  ENDIF

            IF (INQOBS (FILEI2,'MSKPA1')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA1_OBS',GETOBS(FILEI2,'MSKPA1'))
                    CALL ChartOutputRealScalar('MSKPA1_OBS',GETOBS(FILEI2,'MSKPA1'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA2')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA2_OBS',GETOBS(FILEI2,'MSKPA2'))
                    CALL ChartOutputRealScalar('MSKPA2_OBS',GETOBS(FILEI2,'MSKPA2'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA3')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA3_OBS',GETOBS(FILEI2,'MSKPA3'))
                    CALL ChartOutputRealScalar('MSKPA3_OBS',GETOBS(FILEI2,'MSKPA3'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA4')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA4_OBS',GETOBS(FILEI2,'MSKPA4'))
                    CALL ChartOutputRealScalar('MSKPA4_OBS',GETOBS(FILEI2,'MSKPA4'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA5')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA5_OBS',GETOBS(FILEI2,'MSKPA5'))
                    CALL ChartOutputRealScalar('MSKPA5_OBS',GETOBS(FILEI2,'MSKPA5'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA6')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA6_OBS',GETOBS(FILEI2,'MSKPA6'))
                    CALL ChartOutputRealScalar('MSKPA6_OBS',GETOBS(FILEI2,'MSKPA6'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA7')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA7_OBS',GETOBS(FILEI2,'MSKPA7'))
                    CALL ChartOutputRealScalar('MSKPA7_OBS',GETOBS(FILEI2,'MSKPA7'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA8')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA8_OBS',GETOBS(FILEI2,'MSKPA8'))
                      CALL ChartOutputRealScalar('MSKPA8_OBS',GETOBS(FILEI2,'MSKPA8'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA9')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA9_OBS',GETOBS(FILEI2,'MSKPA9'))
                    CALL ChartOutputRealScalar('MSKPA9_OBS',GETOBS(FILEI2,'MSKPA9'))
                  ENDIF
            IF (INQOBS (FILEI2,'MSKPA10')) THEN
                    CALL OUTDAT (2, 0, 'MSKPA10_OBS',GETOBS(FILEI2,'MSKPA10'))
                    CALL ChartOutputRealScalar('MSKPA10_OBS',GETOBS(FILEI2,'MSKPA10'))
                  ENDIF
         
      ENDIF
 
!====================================================================*
!     STATE UPDATE SECTION                                           *
!====================================================================*
      ELSE IF (ITASK.EQ.3) THEN
 
         IF (GRWAT) THEN
!------- New groundwater table depth
            ZWPREV = ZW
            CALL GWTAB(ITASK,SWITGW,NL,DOY,DELT,WLFL,TKL,ZWPREV,IGW,ZW)
         END IF

!----    Integration of state variables
         WL0 = INTGRL(WL0,WL0CH+WL0FILL,DELT)

! Force observed water content values per layer, if available and
! selected; else integrate simulated values
         IF (NL.GE.1)  WCL(1) = INTGR2 (WCL(1),WLCH(1)/TKL(1),DELT,FILEI2,'WCL1')
         IF (NL.GE.2)  WCL(2) = INTGR2(WCL(2),WLCH(2)/TKL(2),DELT,FILEI2,'WCL2')
         IF (NL.GE.3)  WCL(3) = INTGR2(WCL(3),WLCH(3)/TKL(3),DELT,FILEI2,'WCL3')
         IF (NL.GE.4)  WCL(4) = INTGR2(WCL(4),WLCH(4)/TKL(4),DELT,FILEI2,'WCL4')
         IF (NL.GE.5)  WCL(5) = INTGR2(WCL(5),WLCH(5)/TKL(5),DELT,FILEI2,'WCL5')
         IF (NL.GE.6)  WCL(6) = INTGR2(WCL(6),WLCH(6)/TKL(6),DELT,FILEI2,'WCL6')
         IF (NL.GE.7)  WCL(7) = INTGR2(WCL(7),WLCH(7)/TKL(7),DELT,FILEI2,'WCL7')
         IF (NL.GE.8)  WCL(8) = INTGR2(WCL(8),WLCH(8)/TKL(8),DELT,FILEI2,'WCL8')
         IF (NL.GE.9)  WCL(9) = INTGR2(WCL(9),WLCH(9)/TKL(9),DELT,FILEI2,'WCL9')
         IF (NL.GE.10) WCL(10)=INTGR2(WCL(10),WLCH(10)/TKL(10),DELT,FILEI2,'WCL10')

! Limit water content by saturation and air-dryness values per layer
! and calculate amount of water in each layer
         DO I=1,NL
            WCL(I) = MIN(WCST(I),MAX(WCL(I),WCAD(I)))
            WL(I)  = WCL(I)*TKL(I)
         END DO

! Interpolate for soil layers in between observed layers
         I = 1
         DO WHILE (I.LT.3*NL)
            I2 = WCLINT(I)
            J  = WCLINT(I+1)
            K  = WCLINT(I+2)
            IF (WCLINT(I).NE.WCLINT(J) &
               .AND. WCLINT(I).NE.WCLINT(K)) THEN
               WCL(I2) = (WCL(J)+WCL(K))/2.
               WCL(I2) = MIN(WCST(I2),MAX(WCL(I2),WCAD(I2)))
               WL(I2)  = WCL(I2)*TKL(I2)
            END IF
            I = I+3
         END DO

         IF (PUDDLD) THEN
            I = 1
            DO WHILE (I.LE.NL.AND.I.LE.NLPUD)
               CALL SHRINK(ITASK,MNL,I,WL(I),TKL(I),WCST(I),WCSTRP(I),WCL(I), &
                           TOTPOR(I),VL(I))
               IF (WCL(I).LT.WCCR) THEN
                  KSAT(I) = 1000.
!                  WRITE(IUNITL,*)
                  WRITE(*,*) 'Cracks reached compartment ',I,' at time ', TIME
               END IF
               IF (WCL(NLPUD).LT.WCCR) CRACKS = .TRUE.
               WLST(I) = VL(I)*TOTPOR(I)
               WLFC(I) = WLST(I)
               I = I+1
            END DO
          END IF


!------- Calculate moisture suction in KPa
         DO I = 1,NL
!-----------If van Genuchten parameters are available
            IF (SWITPF.EQ.1) THEN
!--------------Get moisture suction MSUC(I) in cm H2O
               CALL SUWCMS2(I,1,WCST(I),WCL(I),MSUC(I))
!-----------If pF curve data are given, use interpolation
            ELSE
!--------------Calculate moisture suction MSUC(I) in cm H2O
               IF (WCL(I).GE.WCFC(I)) THEN
                  FACT    = MAX(0., &
                            MIN(1.,(WCST(I)-WCL(I))/(WCST(I)-WCFC(I))))
                  MSUC(I) = 10.**(FACT*2.0)
                  IF (WCL(I).GE.WCST(I)) MSUC(I) = 0.
               ELSE IF (WCL(I).GE.WCWP(I).AND.WCL(I).LT.WCFC(I)) THEN 
                  FACT    = MAX(0., &
                            MIN(1.,(WCL(I)-WCWP(I))/(WCFC(I)-WCWP(I))))
                  MSUC(I) = 10.**(4.2-FACT*2.2)
               ELSE IF (WCL(I).LT.WCWP(I)) THEN
                  FACT    = MAX(0., &
                            MIN(1.,(WCL(I)-WCAD(I))/(WCWP(I)-WCAD(I))))
                  MSUC(I) = 10.**(7.0-FACT*2.8)
               END IF
            END IF
!           Note: MSKPA(I) is matrix moisture suction in kPa!
            MSKPA(I) = (MSUC(I)/10.)
         END DO

!---- Cumulative amounts
! BAS, 8 SEPTEMBER 2006: 3 CUMULATIVE AMOUNTS
!---- Cumulative amounts From STTIME onwards
            CAPTOTCUM1 = CAPTOTCUM1 + CAPTOT*DELT
            IRCUM1   = IRCUM1  +        IR*DELT
            DRAICUM1 = DRAICUM1+WLFL(NL+1)*DELT
            UPRICUM1 = UPRICUM1+    CAPTOT*DELT
            GWCUM1   = GWCUM1  +     GWTOT*DELT
            WL0FCUM1 = WL0FCUM1+   WL0FILL*DELT
            EVSWCUM1 = EVSWCUM1+     EVSW *DELT
            RAINCUM1 = RAINCUM1+     RAIN *DELT
            RUNOFCUM1 = RUNOFCUM1+     RUNOF*DELT
            TRWCUM1  = TRWCUM1 +     TRW  *DELT
            PERCCUM1  = PERCCUM1  + PERC *DELT

!------ from emergence onward, both in direct-seeded and in transplanted systems
         IF (CROPSTA.GE.1) THEN
            CAPTOTCUM2 = CAPTOTCUM2 + CAPTOT*DELT
            IRCUM2   = IRCUM2  +        IR*DELT
            RUNOFCUM2 = RUNOFCUM2 - RUNOF*DELT
            RAINCUM2  = RAINCUM2  + RAIN *DELT
            TRWCUM2   = TRWCUM2   + TRW  *DELT
            EVSWCUM2  = EVSWCUM2  + EVSW *DELT
            PERCCUM2  = PERCCUM2  + PERC *DELT
            DRAICUM2  = DRAICUM2  + WLFL(NL+1)*DELT
         END IF

!------ From transplanting onward, only in transplanted systems
         IF (ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) THEN
            CAPTOTCUM3 = CAPTOTCUM3 + CAPTOT*DELT
            IRCUM3   = IRCUM3  +        IR*DELT
            RUNOFCUM3 = RUNOFCUM3 - RUNOF*DELT
            RAINCUM3  = RAINCUM3  + RAIN *DELT
            TRWCUM3   = TRWCUM3   + TRW  *DELT
            EVSWCUM3  = EVSWCUM3  + EVSW *DELT
            PERCCUM3  = PERCCUM3  + PERC *DELT
            DRAICUM3  = DRAICUM3  + WLFL(NL+1)*DELT
         END IF
 
!----- Water balance check
       WCUM   = WCUM+WCUMCH*DELT
 
!----- Contribution of profile to water balance, since start
       PROREL = WCUMCH
       WCUMCO = WCUMCO+PROREL*DELT

!----- Contribution of surface water to water balance, since start
       SURREL = WL0CH+WL0FILL
       WL0CO  = WL0CO+SURREL*DELT
 
!----- Total change in system water content
       CKWIN  = WCUMCO+WL0CO

!----- Total of external contributions to system water content
       CKWFL  = IRCUM1+RAINCUM1-RUNOFCUM1- &
                EVSWCUM1-TRWCUM1+UPRICUM1-DRAICUM1+GWCUM1+WL0FCUM1
!----- Check this
       CALL SUWCHK(CKWFL,CKWIN,TIME)

!====================================================================*
!     TERMINAL CALCULATIONS                                          *
!====================================================================*
      ELSE IF (ITASK.EQ.4) THEN
!---  Terminal output writing
!         CALL OPSTOR ('RAINCUM1' , RAINCUM1 )
!         CALL OPSTOR ('IRCUM1' , IRCUM1 )
!         CALL OPSTOR ('RUNOFCUM1', RUNOFCUM1)
!         CALL OPSTOR ('TRWCUM1'  , TRWCUM1  )
!         CALL OPSTOR ('EVSWCUM1' , EVSWCUM1 )
!         CALL OPSTOR ('DRAICUM1', DRAICUM1)
!         CALL OPSTOR ('CAPTOTCUM1', CAPTOTCUM1)

         IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL OPSTOR ('RAINCUM2' , RAINCUM2 )
            CALL OPSTOR ('IRCUM2' , IRCUM2 )
            CALL OPSTOR ('RUNOFCUM2', RUNOFCUM2)
            CALL OPSTOR ('TRWCUM2'  , TRWCUM2  )
            CALL OPSTOR ('EVSWCUM2' , EVSWCUM2 )
            CALL OPSTOR ('DRAICUM2', DRAICUM2)
            CALL OPSTOR ('CAPTOTCUM2', CAPTOTCUM2)
         END IF

         IF (ESTAB.EQ.'TRANSPLANT') THEN
           CALL OPSTOR ('RAINCUM3' , RAINCUM3 )
            CALL OPSTOR ('IRCUM3' , IRCUM3 )
            CALL OPSTOR ('RUNOFCUM3', RUNOFCUM3)
            CALL OPSTOR ('TRWCUM3'  , TRWCUM3  )
            CALL OPSTOR ('EVSWCUM3' , EVSWCUM3 )
            CALL OPSTOR ('DRAICUM3', DRAICUM3)
            CALL OPSTOR ('CAPTOTCUM3', CAPTOTCUM3)
         END IF

      END IF

      RETURN
      END
