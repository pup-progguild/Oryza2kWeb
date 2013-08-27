!------------------------------------------------------------------------!
!  SUBROUTINE DRSAHE                                                     !
!                                                                        !
!  Author : Daniel van Kraalingen                                        !
!  Date   : August 2002; Bouman                                          !
!  Version: 2; adapted for ORYZA2000 model (aerobic rice version)        !
!                                                                        !
!  Purpose: Tipping bucket water balance routine                         !
!                                                                        !
!  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)       !
!  name   type meaning                                      units  class !
!  ----   ---- -------                                      -----  ----- !
!  control                                                               !
!  ITASK   I4  determines action of routine                   -     C,I  !
!  IUNITD  I4  unit number to be used, see file usage below   -   IN,C,I !
!  IUNITL  I4  unit number in use for LOG FILE                -   IN,C,I !
!              = 0, no log file is used or assumed to exist              !
!              > 0, error messages are written to log file               !
!  FILIN   C*  name of file with soil data                    -   IN,C,I !
!  NLXM    I4  number of layers as declared in calling program-     IN   !
!                                                                        !
!  time variables                                                        !
!  TIME    R4  simulation time                                d     T,I  !
!  DELT    R4  time step                                      d     T,I  !
!  OUTPUT  L4  Flag to indicate if output should be done      -      I   !
!  TERMNL  L4  Flag to indicate if simulation is to stop (-)        I/O  !
!                                                                        !
!  dynamic input                                                         !
!  EVSC    R4  potential evaporation rate                    mm/d    I   !
!  RAIN    R4  rainfall rate                                 mm/d    I   !
!  IR      R4  irrigation rate                               mm/d    I   !
!  TRWL    R4  actual transpiration rate per layer           mm/d    I   !
!  ZRT     R4  rooting depth                                  m      I   !
!                                                                        !
!  soil description (available after initial call)                       !
!  NL      I4  number of layers specified in input file       -      O   !
!  TKL     R4  thickness of soil compartments                 m      O   !
!  ZRTMS   R4  maximum rooting depth as soil characteristic   -      O   !
!  WCAD    R4  volumetric water content airdry                -      O   !
!  WCWP    R4  volumetric water content at wilting point      -      O   !
!  WCFC    R4  volumetric water content at field capacity     -      O   !
!  WCST    R4  volumetric water content at saturation         -      O   !
!                                                                        !
!  dynamic output                                                        !
!  WL0     R4 depth of ponded water (always 0)                mm     O   !
!  WCLQT   R4  volumetric soil water content per layer        cm/cm  O   !
!  MSPKA   R4  soil water tension per layer                   kPa    O   !
!------------------------------------------------------------------------!
     SUBROUTINE DRSAHE (ITASK , IUNITD, IUNITL, FILEI2 ,       &       
                        DELT  , OUTPUT, TERMNL, ESTAB, CROPSTA,   &         
                        NL    , EVSC  , RAIN  , IR, ZRT,   &
                        TRWL  , TKL   , TKLT  , ZRTMS , WL0 ,  &
                        WCAD  , WCWP  , WCFC  , WCST ,         &
                        WCLQT , MSKPA)

      USE CHART

      IMPLICIT NONE

!-----Formal parameters
      INTEGER ITASK, IUNITD, IUNITL
      CHARACTER FILEI2*(*)
      LOGICAL OUTPUT, TERMNL

      REAL DELT, EVSC, RAIN, EVSW, IR, WL0, TKLT, ZRT, ZRTMS, INFLCUM1
      REAL DRAICUM1, EVSWCUM1, TRWCUM1, IRCUM1, RAINCUM1, RUNOFCUM1 
      REAL DRAICUM2, EVSWCUM2, TRWCUM2, IRCUM2, RAINCUM2, RUNOFCUM2
      REAL DRAICUM3, EVSWCUM3, TRWCUM3, IRCUM3, RAINCUM3, RUNOFCUM3

      INTEGER NLXM, NL, CROPSTA
      PARAMETER (NLXM=10)
      REAL TRWL(NLXM), MSKPA (NLXM) 

!-----Local variables

      CHARACTER (5) SCODE
      CHARACTER (*) ESTAB
      LOGICAL INQOBS

!-----Soil description arrays
      INTEGER ITYL(NLXM)
      REAL TKL(NLXM)   , TYL(NLXM)  , DEPTH(NLXM)
      REAL WCAD(NLXM)  , WCWP(NLXM) , WCFC(NLXM) , WCST(NLXM)
      REAL WCLQTI(NLXM), WCLCH(NLXM), WCL(NLXM), WCLQT(NLXM), WCLQTM(NLXM)
      REAL FLXCU(NLXM+1), FLXQT(NLXM+1), MSUC(NLXM)
      REAL FACT, FRNOF, RAINMIN

      REAL TKLCU 
      INTEGER NTKLTR, NLCRIT, I, I2, J, K
      INTEGER WCLINT(3*NLXM)

!-----Soil characteristics according to Rijtema/Driessen system
!     The number of soil types defined is NRDTYP
      INTEGER NRDTYP
      PARAMETER (NRDTYP=20)
      REAL MSWCAT(NRDTYP), WCSTT(NRDTYP), MSWCA(NLXM)

!-----Soil characteristics according to Van Genuchten system
!     The number of soil types defined is NVGTYP
      INTEGER NVGTYP
      PARAMETER (NVGTYP=2)
      REAL VGWRT(NVGTYP), VGWST(NVGTYP), VGAT(NVGTYP), VGNT(NVGTYP)
      REAL VGA(NLXM)    , VGR(NLXM)    , VGN(NLXM)
      REAL VGM, TMPR1, TMPR2

!-----Linear interpolation on user-defined log scale
      REAL PFWC00(NLXM), PFWC01(NLXM), PFWC02(NLXM), PFWC03(NLXM)
      REAL PFWC04(NLXM), PFWC05(NLXM), PFWC06(NLXM), PFWC07(NLXM)
      REAL PFWC08(NLXM), PFWC09(NLXM), PFWC10(NLXM)
      REAL PF(22)

!-----Parameters for field capacity, wilting point and airdry
      REAL FIELD, WILTP, AIRDR
      PARAMETER (FIELD = 1.0E2, WILTP = 1.6E4, AIRDR = 1.0E7)

!-----Functions
      REAL LINT, INTGRL, GETOBS, INTGR2

!-----Control, switch, temporary and miscellaneous variables
      INTEGER IL, TMPI1
      INTEGER SWIT6, SWIT8, SWIT9
      REAL WCUM, WCUMO, TRW, FLOW, CAP, DRAIQT, WCUMCH, CHECK
      REAL TRCH, EVSCL
      REAL VAR(NLXM), RESOIL(NLXM)
      REAL SUM, EES, EVSW2, EVSH, EVSD, RDSLR, DSLR
      REAL RNOFF, WEFF, INFL

      SAVE

!     Soil type properties according to the Rijtema/Driessen combination.
!     Data from these tables will be used at the simultaneous occurrence
!     of the following switch values:

!     The 6 soil properties are given in data tables for 20 soils.
!     By defining the soil type TYL(I) for each layer I, a consistent
!     combination of soil properties is selected. Data refer to the
!     twenty standard soil types according to Rijtema (as described
!     by Driessen, 1986).
!
!      1. Coarse sand                11. Fine sandy loam
!      2. Medium coarse sand (mcs)   12. Silt loam
!      3. Medium fine sand           13. Loam
!      4. Fine sand                  14. Sandy clay loam
!      5. Humous loamy mcs           15. Silty clay loam
!      6. Light loamy mcs            16. Clay loam
!      7. Loamy mcs                  17. Light clay
!      8. Loamy fine sand            18. Silty clay
!      9. Sandy loam                 19. Heavy clay
!     10. Loess loam                 20. Peat

      DATA MSWCAT  /0.0853, 0.0450, 0.0366, 0.0255, 0.0135, &
                    0.0153, 0.0243, 0.0299, 0.0251, 0.0156, &
                    0.0186, 0.0165, 0.0164, 0.0101, 0.0108, &
                    0.0051, 0.0085, 0.0059, 0.0043, 0.0108/
 
!     saturated soil moisture content, dimensionless (Rijtema/Driessen)
      DATA WCSTT   /0.3950, 0.3650, 0.3500, 0.3640, 0.4700, &
                    0.3940, 0.3010, 0.4390, 0.4650, 0.4550, &
                    0.5040, 0.5090, 0.5030, 0.4320, 0.4750, &
                    0.4450, 0.4530, 0.5070, 0.5400, 0.8630/

!-----Soil type properties according to the van Genuchten system.
!     The 6 soil properties are given in data tables for NVGTYP soils.
!     (for declarations, see above).  By defining the soil type TYL(I)
!     for each layer I, a consistent combination of soil properties
!     is selected. In the case presented here, data refer to
!     a few soil types of the Lovinkhoeve (Peter de Willigen)
!     To be extended by user.
!    
!      1. Lovinkhoeve 12b
!      2. Lovinkhoeve 16a

!     TETA-r, dimensionless
      DATA VGWRT / 0.0448,  0.0000/
!     TETA-s, dimensionless
      DATA VGWST / 0.4012,  0.4505/
!     ALPHA in cm-1
      DATA VGAT  / 0.0036,  0.0067/
!     N, dimensionless
      DATA VGNT  / 1.5007,  1.2318/

      IF (DELT.GT.1.) CALL FATALERR ('DRSAHE','delt too large')

      IF (ITASK.EQ.1) THEN

!        ----------------------
!        Initialization section
!        ----------------------

!------  Set depth of ponded water (dummy for ORYZA model) to 0:
         WL0 = 0.

!        Read input file

         CALL RDINIT (IUNITD, IUNITL, FILEI2)

!----- Read code to recognize the correctness of supplied soil file
         CALL RDSCHA('SCODE',SCODE)
         IF (SCODE .NE. 'SAHEL') THEN
               WRITE (*,*) 'Wrong soil input file for SAHEL water balance'
               CALL OUTCOM('Wrong soil input file for SAHEL water balance')
               TERMNL = .TRUE.
         END IF

         CALL RDSINT ('NL', NL)

         IF (NL.GT.NLXM) THEN 
               WRITE (*,*) 'ERROR SAHEL: too many soil layers defined in data file'
               CALL OUTCOM('SAHEL: too many soil layers defined in data file')
               TERMNL = .TRUE.
          END IF

         IF (NLXM.LT.NL) THEN
               WRITE (*,*) 'ERROR SAHEL: too few layers in external arrays'
               CALL OUTCOM('SAHEL: too few layers in external arrays')
               TERMNL = .TRUE.
         END IF 

!        Read thicknesses, and evaporation proportionality factor
         CALL RDFREA ('TKL', TKL, NLXM, NL)
         CALL RDSREA ('EES', EES)
!        Maximum rooting depth soil
         CALL RDSREA('ZRTMS',ZRTMS)
!        Fraction runoff from rain
         CALL RDSREA('FRNOF',FRNOF)
!        Minimum amount of rain that will not runoff
         CALL RDSREA('RAINMIN',RAINMIN)

!        Indicator set for interpolation of observed soil water contents if forced
         CALL RDFINT('WCLINT',WCLINT,3*NLXM,3*NL)

!        Read switches
         CALL RDSINT ('SWIT9', SWIT9)
         CALL RDSINT ('SWIT8', SWIT8)

         IF (SWIT9.EQ.1) THEN
!           Moisture characteristics by user-defined parameters

            CALL RDFREA ('WCST', WCST, NLXM, NL)

            IF (SWIT8.EQ.1) THEN

!              Driessen moisture characteristic
               CALL RDFREA ('MSWCA', MSWCA, NLXM, NL)
               DO IL=1,NL
                  WCFC(IL) = WCST(IL)*EXP (-MSWCA(IL)*LOG (FIELD)**2)
                  WCWP(IL) = WCST(IL)*EXP (-MSWCA(IL)*LOG (WILTP)**2)
                  WCAD(IL) = WCST(IL)*EXP (-MSWCA(IL)*LOG (AIRDR)**2)
               END DO

            ELSE IF (SWIT8.EQ.2) THEN

!              Van Genuchten moisture characteristic
               CALL RDFREA ('VGA', VGA, NLXM, NL)
               CALL RDFREA ('VGR', VGR, NLXM, NL)
               CALL RDFREA ('VGN', VGN, NLXM, NL)

               DO IL=1,NL
                  VGM = 1.-1./VGN(IL)

                  TMPR1    = (FIELD*VGA(IL))**VGN(IL)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCFC(IL) = TMPR2*(WCST(IL)-VGR(IL))+VGR(IL)

                  TMPR1    = (WILTP*VGA(IL))**VGN(IL)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCWP(IL) = TMPR2*(WCST(IL)-VGR(IL))+VGR(IL)

                  TMPR1    = (AIRDR*VGA(IL))**VGN(IL)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCAD(IL) = TMPR2*(WCST(IL)-VGR(IL))+VGR(IL)
               END DO

            ELSE IF (SWIT8.EQ.3) THEN

!              Linear interpolation on user-defined log scale

!              Read pF values
               CALL RDFREA ('PFWC00', PFWC00, NLXM, NL)
               CALL RDFREA ('PFWC01', PFWC01, NLXM, NL)
               CALL RDFREA ('PFWC02', PFWC02, NLXM, NL)
               CALL RDFREA ('PFWC03', PFWC03, NLXM, NL)
               CALL RDFREA ('PFWC04', PFWC04, NLXM, NL)
               CALL RDFREA ('PFWC05', PFWC05, NLXM, NL)
               CALL RDFREA ('PFWC06', PFWC06, NLXM, NL)
               CALL RDFREA ('PFWC07', PFWC07, NLXM, NL)
               CALL RDFREA ('PFWC08', PFWC08, NLXM, NL)
               CALL RDFREA ('PFWC09', PFWC09, NLXM, NL)
               CALL RDFREA ('PFWC10', PFWC10, NLXM, NL)

!              Set up relative moisture content values
               PF(2)  = 0.
               PF(4)  = 0.1
               PF(6)  = 0.2
               PF(8)  = 0.3
               PF(10) = 0.4
               PF(12) = 0.5
               PF(14) = 0.6
               PF(16) = 0.7
               PF(18) = 0.8
               PF(20) = 0.9
               PF(22) = 1.0

!              Fill array with pf values for subsequent soil layers
               DO IL=1,NL
                  PF(1)  = PFWC00(IL)
                  PF(3)  = PFWC01(IL)
                  PF(5)  = PFWC02(IL)
                  PF(7)  = PFWC03(IL)
                  PF(9)  = PFWC04(IL)
                  PF(11) = PFWC05(IL)
                  PF(13) = PFWC06(IL)
                  PF(15) = PFWC07(IL)
                  PF(17) = PFWC08(IL)
                  PF(19) = PFWC09(IL)
                  PF(21) = PFWC10(IL)
                  WCFC(IL) = WCST(IL)* MAX (0.01, LINT (PF,22,LOG10(FIELD)))
                  WCWP(IL) = WCST(IL)* MAX (0.01, LINT (PF,22,LOG10(WILTP)))
                  WCAD(IL) = WCST(IL)* MAX (0.01, LINT (PF,22,LOG10(AIRDR)))
               END DO

            ELSE IF (SWIT8.EQ.4) THEN

!              User must specify pf-curve parameters to be read
!              and include error check
               CALL RDFREA ('WCST', WCST, NLXM, NL)
               CALL RDFREA ('WCFC', WCFC, NLXM, NL)
               CALL RDFREA ('WCWP', WCWP, NLXM, NL)
               CALL RDFREA ('WCAD', WCAD, NLXM, NL)

            ELSE
               CALL FATALERR ('DRSAHE','Illegal SWIT8 value')
            END IF


         ELSE IF (SWIT9.EQ.2) THEN

!           Physical properties from soil type number
            CALL RDFREA ('TYL', TYL, NLXM, NL)

            IF (SWIT8.EQ.1) THEN
!              Driessen moisture characteristic
               DO IL=1,NL
                  ITYL(IL) = NINT (TYL(IL))
                  TMPI1    = ITYL(IL)
                  WCST(IL) = WCSTT(TMPI1)
                  WCFC(IL) = WCST(IL)*EXP (-MSWCAT(TMPI1)*LOG(FIELD)**2)
                  WCWP(IL) = WCST(IL)*EXP (-MSWCAT(TMPI1)*LOG(WILTP)**2)
                  WCAD(IL) = WCST(IL)*EXP (-MSWCAT(TMPI1)*LOG(AIRDR)**2)
               END DO

            ELSE IF (SWIT8.EQ.2) THEN
!              Van Genuchten moisture characteristic
               DO IL=1,NL
                  ITYL(IL) = NINT (TYL(IL))
                  TMPI1    = ITYL(IL)

                  WCST(IL) = VGWST(TMPI1)
                  VGM      = 1.-1./VGNT(TMPI1)

                  TMPR1    = (FIELD*VGAT(TMPI1))**VGNT(TMPI1)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCFC(IL) = TMPR2*(WCST(IL)-VGWRT(TMPI1))+VGWRT(TMPI1)

                  TMPR1    = (WILTP*VGAT(TMPI1))**VGNT(TMPI1)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCWP(IL) = TMPR2*(WCST(IL)-VGWRT(TMPI1))+VGWRT(TMPI1)

                  TMPR1    = (AIRDR*VGAT(TMPI1))**VGNT(TMPI1)
                  TMPR2    = (1.+TMPR1)**(-VGM)
                  WCAD(IL) = TMPR2*(WCST(IL)-VGWRT(TMPI1))+VGWRT(TMPI1)

               END DO
            ELSE
               CALL FATALERR ('DRSAHE','SWIT8 wrong value ; should be 1 or 2')
            END IF
         ELSE

            CALL FATALERR ('DRSAHE','SWIT9 wrong value ; should be 1 or 2')

         END IF

!        Check consistency of hydrological characteristics
         DO IL=1,NL
            IF (WCST(IL).LE.0..OR.WCST(IL).GT.1.) THEN
               CALL FATALERR ('DRSAHE','Saturation point < 0 or > 1')
            ELSE IF (WCFC(IL).LE.0..OR.WCFC(IL).GT.1.) THEN
               CALL FATALERR ('DRSAHE','Field capacity < 0 or > 1')
            ELSE IF (WCWP(IL).LE.0..OR.WCWP(IL).GT.1.) THEN
               CALL FATALERR ('DRSAHE','Wilting point < 0 or > 1')
            ELSE IF (WCAD(IL).LE.0..OR.WCAD(IL).GT.1.) THEN
               CALL FATALERR ('DRSAHE','Air dry < 0 or > 1')
            END IF

            IF (WCFC(IL).GT.WCST(IL)) THEN
               CALL FATALERR ('DRSAHE','Field capacity > Saturation')
            ELSE IF (WCWP(IL).GT.WCFC(IL)) THEN
               CALL FATALERR ('DRSAHE','Wilting point > Field capacity')
            ELSE IF (WCAD(IL).GT.WCWP(IL)) THEN
               CALL FATALERR ('DRSAHE','Air dry > Wilting point')
            END IF
         END DO

!        Initial water contents
         CALL RDSINT ('SWIT6', SWIT6)

         IF (SWIT6.EQ.1) THEN
!           At field capacity
            DO IL=1,NL
               WCLQTI(IL) = WCFC(IL)
            END DO
         ELSE IF (SWIT6.EQ.2) THEN
!           At wilting point
            DO IL=1,NL
               WCLQTI(IL) = WCWP(IL)
            END DO
         ELSE IF (SWIT6.EQ.3) THEN
!           At observed moisture contents
            CALL RDFREA ('WCLQTM', WCLQTM, NLXM, NL)
            DO IL=1,NL
               WCLQTI(IL) = WCLQTM(IL)
               IF (WCLQTI(IL).GT.WCFC(IL)) THEN
                  WRITE (*,'(2A)') &
                  ' WARNING from DRSAHE: initial soil moisture content larger than field capacity'
                  IF (IUNITL.GT.0) WRITE (IUNITL,'(2A)') &
                  ' WARNING from DRSAHE: initial soil moisture content larger than field capacity'
!                   WCLQTI(IL) = WCFC(IL)
               ELSE IF (WCLQTI(IL).LT.WCAD(IL)) THEN
                  WRITE (*,'(2A)') &
                    ' WARNING from DRSAHE: initial soil moisture', &
                    ' content less than air dry'
                  IF (IUNITL.GT.0) WRITE (IUNITL,'(2A)') &
                    ' WARNING from DRSAHE: initial soil moisture', &
                    ' content less than air dry'
!                  WCLQTI(IL) = WCAD(IL)
               END IF
            END DO
         ELSE
            CALL FATALERR ('DRSAHE', 'SWIT6 wrong value ; should be 1, 2 or 3')
         END IF

!        End of reading from data file
         CLOSE (IUNITD)

!        Calculate array with depths
         DEPTH(1) = 0.5*TKL(1)
         DO IL=2,NL
            DEPTH(IL) = DEPTH(IL-1)+0.5*TKL(IL-1)+0.5*TKL(IL)
         END DO

!        Initialize remaining variables
         WCUM  = 0.
         DO IL=1,NL
            WCLQT(IL) = WCLQTI(IL)
            WCUM      = WCUM+WCLQT(IL)*TKL(IL)*1000.
            FLXCU(IL) = 0.
            FLXQT(IL) = 0.
            MSKPA(IL) = 0.
         END DO

!        Set not used elements to zero
         DO IL=NL+1,NLXM
            TKL(IL)  = 0.
            WCAD(IL) = 0.
            WCWP(IL) = 0.
            WCFC(IL) = 0.
            WCST(IL) = 0.
            WCLQT(IL) = 0.
            WCL(IL)   = 0.
            MSKPA(IL) = 0.
            FLXCU(IL) = 0.
            FLXQT(IL) = 0.
         END DO

         FLXCU(NLXM+1) = 0.
         FLXQT(NLXM+1) = 0.

         INFLCUM1 = 0.

         RAINCUM1  = 0.
         IRCUM1    = 0.
         RUNOFCUM1 = 0.
         EVSWCUM1  = 0.
         TRWCUM1   = 0.
         DRAICUM1  = 0.

         RAINCUM2  = 0.
         IRCUM2    = 0.
         RUNOFCUM2 = 0.
         EVSWCUM2  = 0.
         TRWCUM2   = 0.
         DRAICUM2  = 0.

         RAINCUM3  = 0.
         IRCUM3    = 0.
         RUNOFCUM3 = 0.
         EVSWCUM3  = 0.
         TRWCUM3   = 0.
         DRAICUM3  = 0.

         EVSW   = 0.
         DSLR   = 1.

!        Calcuate depth of profile
         TKLT = 0.
         DO IL=1,NL
            TKLT    = TKLT+TKL(IL)
         END DO

      ELSE IF (ITASK.EQ.2) THEN

!        ------------------------
!        Rate calculation section
!        ------------------------

!        Determine rates of change of water balance

!        Check: external evaporation rate should be positive
         IF (EVSC.LT.0.) THEN
               CALL FATALERR ('DRSAHE','Soil evaporation should be positive')
         END IF

!        change sign of evaporation
         EVSCL = -EVSC

!        Check: rainfall should be positive
         IF (RAIN.LT.0.) THEN
               WRITE (*,*) 'SAHEL: Rainfall is negative => simulation stopped'
               CALL OUTCOM('SAHEL: Rainfall is negative => simulation stopped')
               TERMNL = .TRUE.
         END IF

!        Check: transpiration should be positive
         DO IL=1,NL
            IF (TRWL(IL).LT.0.) THEN
               WRITE (*,*) 'SAHEL: Transp should be positive=> simulation stopped'
               CALL OUTCOM('SAHEL: Transp should be positive=> simulation stopped')
               TERMNL = .TRUE.
            ELSE IF (TRWL(IL).GT.0..AND.WCL(IL).LT.WCWP(IL)) THEN
! BAS: STILL TO CHECK THIS:
               WRITE (*,*) 'SAHEL: Transpiration from layer below wilting point'
               CALL OUTCOM('SAHEL: Transp from layer below wilting point')
               TERMNL = .TRUE.
            END IF
         END DO

!        Set rates of change to zero and make local water status
!        array equal to current water status, reset fluxes
         DO IL=1,NL
            WCLCH(IL) = 0.
            WCL(IL)   = WCLQT(IL)
            FLXQT(IL) = 0.
         END DO
         FLXQT(NL+1) = 0.

!        Cumulate transpiration
         TRW = 0.
         DO IL=1,NL
            TRW = TRW-TRWL(IL)
         END DO

!        Effectuate transpiration on local status array
!        (transpiration has a positive value)
         DO IL=1,NL
            TRCH  = DELT*TRWL(IL)/(TKL(IL)*1000.)
            TMPR1 = WCL(IL)-TRCH
            IF (TRWL(IL).GT.0.) THEN
               IF (WCL(IL).GT.WCWP(IL).AND. &
                   TMPR1.LT.WCWP(IL).AND.TMPR1.GE.WCAD(IL)) THEN
                  WRITE (*,'(A,/,2A,I3)') ' WARNING from DRSAHE:', &
                    '   Transpiration extracted water below', &
                    ' wilting point in layer:',IL
               ELSE IF (TMPR1.LT.WCAD(IL)) THEN
                  WRITE (*,'(A,/,2A,I3)') ' ERROR in DRSAHE:', &
                    '   Transpiration extracted water below', &
                    ' air dry in layer:',IL
                  CALL FATALERR ('DRSAHE',' ')
               END IF
               WCL(IL) = TMPR1
            END IF
         END DO

         RNOFF = MAX (0., FRNOF*(RAIN-RAINMIN))
         INFL  = IR+RAIN-RNOFF

         IF (INFL.GT.0.5) THEN
            IF (WCL(1).GT.WCAD(1)) THEN
               EVSH  = MAX (EVSCL, -((WCL(1)-WCAD(1))*TKL(1)*1000.)/DELT-INFL)
            ELSE
               EVSH = 0.
            END IF
            EVSW2 = EVSH
            RDSLR = -(DSLR-1.)/DELT
         ELSE
            EVSD  = MAX (EVSCL, 0.6*EVSCL*(SQRT (DSLR+1.)-SQRT(DSLR))-INFL)
            EVSW2 = EVSD
            RDSLR = 1.
         END IF

!        Calculate array for exponential extinction of evaporation
         SUM = 0.
         DO IL=1,NL
            IF (WCL(IL).GT.WCAD(IL)) THEN
               VAR(IL) = TKL(IL)*1000.*(WCL(IL)-WCAD(IL))* &
                         EXP (-EES*(DEPTH(IL)-0.25*TKL(IL)))
               SUM     = SUM+VAR(IL)
            END IF
         END DO

!        Effectuate evaporation on local status array and calculate
!        the actual soil evaporation
         EVSW = 0.
         DO IL=1,NL
            IF (SUM.GT.0.) THEN
!              Water available somewhere in profile
               RESOIL(IL) = EVSW2*VAR(IL)/SUM
!              calculate available moisture (mm) in layer
               TMPR1 = MAX (WCL(IL)-WCAD(IL),0.)*TKL(IL)*1000.
!              make sure evaporation cannot exceed available moisture
               TMPR2 = MIN (-RESOIL(IL)*DELT,TMPR1)/DELT
               RESOIL(IL) = -TMPR2

!              convert units from mm to a fraction
               EVSW    = EVSW+RESOIL(IL)
               WCL(IL) = WCL(IL)+RESOIL(IL)*DELT/(TKL(IL)*1000.)
            ELSE
!              Water not available in profile
               RESOIL(IL) = 0.
            END IF
         END DO

!        Effectuate infiltration on local status array
!        nog een keer naar delt kijken
         FLOW = 0.
         IF (INFL.GT.0.) THEN
            FLOW     = INFL
            FLXQT(1) = FLOW
            DO IL=1,NL
               CAP = (WCFC(IL)-WCL(IL))*TKL(IL)*1000.
               IF (CAP.LE.FLOW*DELT) THEN
!                 water flow does not fit into compartment
                  WCL(IL) = WCFC(IL)
                  FLOW = FLOW-CAP/DELT
               ELSE
!                 water flow does fit into compartment
                  WCL(IL) = WCL(IL)+FLOW*DELT/(TKL(IL)*1000.)
                  FLOW = 0.
               END IF
               FLXQT(IL+1) = FLOW
            END DO
         END IF
         DRAIQT = -FLOW

         WCUMCH = 0.
         DO IL=1,NL
            WCLCH(IL) = (WCL(IL) - WCLQT(IL)) / DELT
            WCUMCH    = WCUMCH+WCLCH(IL)*TKL(IL)*1000.
         END DO

!        Calculate efficiency of rainfall and irrigation application
         WEFF = ABS (TRWCUM1/(WCUM+DRAICUM1+EVSWCUM1+RUNOFCUM1+INFLCUM1))

         IF (OUTPUT) THEN
!           Output rates
            CALL OUTDAT (2, 0, 'IR  '  , IR)
            CALL ChartOutputRealScalar('IR',IR)
            CALL OUTDAT (2, 0, 'IRCUM2  '  , IRCUM2)
            CALL ChartOutputRealScalar('IRCUM2',IRCUM2)
            CALL OUTDAT (2, 0, 'RAINCUM2 '  , RAINCUM2)
            CALL ChartOutputRealScalar('RAINCUM2',RAINCUM2)
            CALL OUTDAT (2, 0, 'EVSW  ', -EVSW)
            CALL ChartOutputRealScalar('EVSW',-EVSW)

            IF (NL.GE.1) CALL OUTDAT(2,0,'WCL1',WCL(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('WCL1',WCLQT(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'WCL2',WCL(2))
            IF (NL.GE.2) CALL ChartOutputRealScalar('WCL2',WCLQT(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'WCL3',WCL(3))
            IF (NL.GE.3) CALL ChartOutputRealScalar('WCL3',WCLQT(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'WCL4',WCL(4))
            IF (NL.GE.4) CALL ChartOutputRealScalar('WCL4',WCLQT(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'WCL5',WCL(5))
            IF (NL.GE.5) CALL ChartOutputRealScalar('WCL5',WCLQT(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'WCL6',WCL(6))
            IF (NL.GE.6) CALL ChartOutputRealScalar('WCL6',WCLQT(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'WCL7',WCL(7))
            IF (NL.GE.7) CALL ChartOutputRealScalar('WCL7',WCLQT(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'WCL8',WCL(8))
            IF (NL.GE.8) CALL ChartOutputRealScalar('WCL8',WCLQT(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'WCL9',WCL(9))
            IF (NL.GE.9) CALL ChartOutputRealScalar('WCL9',WCLQT(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'WCL10',WCL(10))
            IF (NL.GE.10) CALL ChartOutputRealScalar('WCL10',WCLQT(10))

            IF (NL.GE.1) CALL OUTDAT(2,0,'MSKPA1',MSKPA(1))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA1',MSKPA(1))
            IF (NL.GE.2) CALL OUTDAT(2,0,'MSKPA2',MSKPA(2))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA2',MSKPA(2))
            IF (NL.GE.3) CALL OUTDAT(2,0,'MSKPA3',MSKPA(3))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA3',MSKPA(3))
            IF (NL.GE.4) CALL OUTDAT(2,0,'MSKPA4',MSKPA(4))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA4',MSKPA(4))
            IF (NL.GE.5) CALL OUTDAT(2,0,'MSKPA5',MSKPA(5))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA5',MSKPA(5))
            IF (NL.GE.6) CALL OUTDAT(2,0,'MSKPA6',MSKPA(6))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA6',MSKPA(6))
            IF (NL.GE.7) CALL OUTDAT(2,0,'MSKPA7',MSKPA(7))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA7',MSKPA(7))
            IF (NL.GE.8) CALL OUTDAT(2,0,'MSKPA8',MSKPA(8))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA8',MSKPA(8))
            IF (NL.GE.9) CALL OUTDAT(2,0,'MSKPA9',MSKPA(9))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA9',MSKPA(9))
            IF (NL.GE.10) CALL OUTDAT(2,0,'MSKPA10',MSKPA(10))
            IF (NL.GE.1) CALL ChartOutputRealScalar('MSKPA10',MSKPA(10))

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
         END IF

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

!        Checks on calculated rate variables
         DO IL=1,NL
            IF (RESOIL(IL).GT.0.) CALL FATALERR &
              ('DRSAHE','evaporation rate greater than zero')
         END DO

      ELSE IF (ITASK.EQ.3) THEN

!        ===========
!        Integration
!        ===========

         WCUMO = WCUM

!         DO IL=1,NL
!           Water content per layer
!            WCLQT(IL) = INTGRL (WCLQT(IL), WCLCH(IL), DELT)
!         END DO

         DO IL=1,NL+1
            FLXCU(IL) = INTGRL (FLXCU(IL), FLXQT(IL), DELT)
         END DO

! Force observed water content values per layer, if available and
! selected; else integrate simulated values
         IF (NL.GE.1)  WCLQT(1) = INTGR2(WCLQT(1),WCLCH(1),DELT,FILEI2,'WCL1')
         IF (NL.GE.2)  WCLQT(2) = INTGR2(WCLQT(2),WCLCH(2),DELT,FILEI2,'WCL2')
         IF (NL.GE.3)  WCLQT(3) = INTGR2(WCLQT(3),WCLCH(3),DELT,FILEI2,'WCL3')
         IF (NL.GE.4)  WCLQT(4) = INTGR2(WCLQT(4),WCLCH(4),DELT,FILEI2,'WCL4')
         IF (NL.GE.5)  WCLQT(5) = INTGR2(WCLQT(5),WCLCH(5),DELT,FILEI2,'WCL5')
         IF (NL.GE.6)  WCLQT(6) = INTGR2(WCLQT(6),WCLCH(6),DELT,FILEI2,'WCL6')
         IF (NL.GE.7)  WCLQT(7) = INTGR2(WCLQT(7),WCLCH(7),DELT,FILEI2,'WCL7')
         IF (NL.GE.8)  WCLQT(8) = INTGR2(WCLQT(8),WCLCH(8),DELT,FILEI2,'WCL8')
         IF (NL.GE.9)  WCLQT(9) = INTGR2(WCLQT(9),WCLCH(9),DELT,FILEI2,'WCL9')
         IF (NL.GE.10) WCLQT(10)=INTGR2(WCLQT(10),WCLCH(10),DELT,FILEI2,'WCL10')

! Limit water content by saturation and air-dryness values per layer
! and calculate amount of water in each layer
!BB: soil water content can by > field capacity if observed data are forced!
!         DO I=1,NL
!            WCLQT(I) = MIN(WCST(I),MAX(WCL(I),WCAD(I)))
!         END DO

! Interpolate for soil layers in between observed layers
         I = 1
         DO WHILE (I.LT.3*NL)
            I2 = WCLINT(I)
            J  = WCLINT(I+1)
            K  = WCLINT(I+2)
            IF (WCLINT(I).NE.WCLINT(J) &
               .AND. WCLINT(I).NE.WCLINT(K)) THEN
               WCLQT(I2) = (WCLQT(J)+WCLQT(K))/2.
!BB: soil water content can by > field capacity if observed data are forced!
!               WCLQT(I2) = MIN(WCST(I2),MAX(WCL(I2),WCAD(I2)))
            END IF
            I = I+3
         END DO

!===========Checks on simulation run
!-----------If soil moisture content in whole root zone is below WP: abort simulation
         TKLCU  = 0.
         NTKLTR = 1
         DO IL=1,NL
            IF (TKLCU-ZRT.LE.0.) NTKLTR = NTKLTR + 1
            TKLCU = TKLCU+TKL(IL)
         END DO

         NLCRIT   = 0
         DO IL=1,NTKLTR-1
            IF (WCLQT(IL).LT. WCWP(IL)) THEN
               NLCRIT = NLCRIT + 0
            ELSE
               NLCRIT = NLCRIT + 1
            END IF
         END DO

         IF (NLCRIT.LE.0) THEN
            WRITE (*,*) 'soil water below wilting point => simulation stopped'
            CALL OUTCOM('soil water below wilting point  => simulation stopped')
            TERMNL = .TRUE.
         END IF

!------- Calculate moisture suction in KPa (FROM PADY)
         DO IL = 1,NL
!-----------If van Genuchten parameters are available
!            IF (SWITPF.EQ.1) THEN
!--------------Get moisture suction MSUC(IL) in cm H2O
!               CALL SUWCMS2(IL,1,WCST(IL),WCLQT(IL),MSUC(IL))
!-----------If pF curve data are given, use interpolation
!            ELSE
!--------------Calculate moisture suction MSUC(IL) in cm H2O
               IF (WCLQT(IL).GE.WCFC(IL)) THEN
                  FACT    = MAX(0., &
                            MIN(1.,(WCST(IL)-WCLQT(IL))/(WCST(IL)-WCFC(IL))))
                  MSUC(IL) = 10.**(FACT*2.0)
                  IF (WCLQT(IL).GE.WCST(IL)) MSUC(IL) = 0.
               ELSE IF (WCLQT(IL).GE.WCWP(IL).AND.WCLQT(IL).LT.WCFC(IL)) THEN 
                  FACT    = MAX(0., &
                            MIN(1.,(WCLQT(IL)-WCWP(IL))/(WCFC(IL)-WCWP(IL))))
                  MSUC(IL) = 10.**(4.2-FACT*2.2)
               ELSE IF (WCLQT(IL).LT.WCWP(IL)) THEN
                  FACT    = MAX(0., &
                            MIN(1.,(WCLQT(IL)-WCAD(IL))/(WCWP(IL)-WCAD(IL))))
                  MSUC(IL) = 10.**(7.0-FACT*2.8)
               END IF
!            END IF
!           Note: MSKPA(IL) is matrix moisture suction in kPa!
            MSKPA(IL) = (MSUC(IL)/10.)
         END DO

! BAS, 8 SEPTEMBER 2006: 3 CUMULATIVE AMOUNTS
!---- Cumulative amounts From STTIME onwards
         RAINCUM1  = INTGRL (RAINCUM1, RAIN  , DELT)
         RUNOFCUM1 = INTGRL (RUNOFCUM1, RNOFF , DELT)
         INFLCUM1  = INTGRL (INFLCUM1, INFL  , DELT)
         EVSWCUM1  = INTGRL (EVSWCUM1, EVSW  , DELT)
         TRWCUM1   = INTGRL (TRWCUM1 , TRW   , DELT)
         DRAICUM1  = INTGRL (DRAICUM1, DRAIQT, DELT)
         IRCUM1    = INTGRL (IRCUM1,   IR    , DELT)

!------ from emergence onward, both in direct-seeded and in transplanted systems
         IF (CROPSTA.GE.1) THEN
            RAINCUM2  = INTGRL (RAINCUM2, RAIN  , DELT)
            RUNOFCUM2 = INTGRL (RUNOFCUM2, RNOFF , DELT)
            EVSWCUM2  = INTGRL (EVSWCUM2, EVSW  , DELT)
            TRWCUM2   = INTGRL (TRWCUM2 , TRW   , DELT)
            DRAICUM2  = INTGRL (DRAICUM2, DRAIQT, DELT)
            IRCUM2    = INTGRL (IRCUM2,   IR    , DELT)
         END IF

!------ From transplanting onward, only in transplanted systems
         IF (ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) THEN
            RAINCUM3  = INTGRL (RAINCUM3, RAIN  , DELT)
            RUNOFCUM3 = INTGRL (RUNOFCUM3, RNOFF , DELT)
            EVSWCUM3  = INTGRL (EVSWCUM3, EVSW  , DELT)
            TRWCUM3   = INTGRL (TRWCUM3 , TRW   , DELT)
            DRAICUM3  = INTGRL (DRAICUM3, DRAIQT, DELT)
            IRCUM3    = INTGRL (IRCUM3,   IR    , DELT)
         END IF

!        Miscellaneous
         WCUM   = INTGRL (WCUM  , WCUMCH, DELT)
         DSLR   = INTGRL (DSLR  , RDSLR , DELT)

!        Check on value of water content per layer
         DO IL=1,NL
            IF (WCLQT(IL).LT.WCAD(IL)-0.01.OR.WCLQT(IL).LT.0.) CALL FATALERR &
               ('DRSAHE','water content less than air dry')
!BB: soil water content can by > field capacity if observed data are forced!
!            IF (WCLQT(IL).GT.WCFC(IL)+0.01) CALL FATALERR &
!               ('DRSAHE','water content greater than field capacity')
         END DO

!        Check on correctness of balance, use relative error
         CHECK = (WCUMO-WCUM)+DELT*(INFL+DRAIQT+EVSW+TRW)

         IF (ABS (CHECK/(0.5*(WCUMO+WCUM))).GT.0.001) THEN
            CALL FATALERR ('DRSAHE','error in water balance')
         END IF


!============================================================*
!------Terminal section
!============================================================*
      ELSE IF (ITASK.EQ.4) THEN

!---------Store end-of-year data to a special file.
         CALL OPSTOR ('RAINCUM1' , RAINCUM1 )
         CALL OPSTOR ('IRCUM1', IRCUM1)
         CALL OPSTOR ('RUNOFCUM1', -RUNOFCUM1)
         CALL OPSTOR ('TRWCUM1'  , -TRWCUM1  )
         CALL OPSTOR ('EVSWCUM1' , -EVSWCUM1 )
         CALL OPSTOR ('DRAICUM1' , -DRAICUM1)

         IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL OPSTOR ('RAINCUM2' , RAINCUM2 )
            CALL OPSTOR ('IRCUM2', IRCUM2)
            CALL OPSTOR ('RUNOFCUM2', -RUNOFCUM2)
            CALL OPSTOR ('TRWCUM2'  , -TRWCUM2  )
            CALL OPSTOR ('EVSWCUM2' , -EVSWCUM2 )
            CALL OPSTOR ('DRAICUM2' , -DRAICUM2)
         END IF

         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL OPSTOR ('RAINCUM3' , RAINCUM3 )
            CALL OPSTOR ('IRCUM3', IRCUM3)
            CALL OPSTOR ('RUNOFCUM3', -RUNOFCUM3)
            CALL OPSTOR ('TRWCUM3'  , -TRWCUM3  )
            CALL OPSTOR ('EVSWCUM3' , -EVSWCUM3 )
            CALL OPSTOR ('DRAICUM3' , -DRAICUM3)
         END IF

!         CONTINUE
      ELSE
         CALL FATALERR ('DRSAHE','wrong ITASK')
      END IF

      RETURN
      END
