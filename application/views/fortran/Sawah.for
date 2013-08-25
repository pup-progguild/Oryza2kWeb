
      SUBROUTINE SAWAH  (ITASK, IUNITD, IUNITL, FILEI2, OUTPUT, 
     $                  ESTAB,  CROPSTA,
     $                  DOY,    DELT,   TIME,   RAIN,   IR,     EVSC, 
     $                  TRC,    TRWL,   NL,     ZRTMS,  TKL,    TKLT, 
     $                  WCAD,   WCWP,   WCFC,   WCST,   WCLQT, 
     $                  WL0,    MSKPA)

      USE CHART

      IMPLICIT NONE

!-----Formal parameters
      INTEGER   ITASK, IUNITD, IUNITL, CROPSTA

      REAL      DOY, DELT, TIME, RAIN, ZW
      CHARACTER*(*) FILEI2 
      CHARACTER (80) ESTAB
      LOGICAL   OUTPUT, INQOBS
      REAL      GETOBS

!-----Local variables
      INTEGER      NL
      REAL         EVSC, IR, TKLT, TRC, WL0, ZRTMS

      INTEGER      NLXM
      PARAMETER   (NLXM=10)
      REAL         MSKPA(NLXM), TKL(NLXM) , TRWL(NLXM)
      REAL         WCAD(NLXM) , WCWP(NLXM)  , WCFC(NLXM), WCST(NLXM)
      REAL         WCLQT(NLXM)
      REAL         WCL(NLXM)
      
      
!-----For communication with DRSAWA routine
      INTEGER   IOUT, I
      REAL DAY,DTAV
!     number of soil layers
      INTEGER INLAY
!     volumetric water contents
      REAL WCADX(10),WCWPX(10),WCFCX(10),WCSTX(10)
!     rates of change (per day)
      REAL FLXQT(11)
!     status variable
      REAL PRHEAD(10)
      REAL WL0QT, EVSW, ZEQT
!     cumulative amounts
      REAL FLXCU(11)
      REAL DRAICU, EVSWCU, RAINIRCU, RNOFCU, UPRICU
      REAL TRWCU, WCUMCO, WL0CO

!BB new, March 2005, June 2006
      REAL IRC, RAINC, TRWC, EVSWC, TRW, RUNOF, RNOFC

!-----End DRSAWAH communication
      SAVE

!-----Set print setting
      IOUT = 11112

!BB new, March 2005
! initialization of some state variables in main field only!
      IF (ITASK .EQ. 1) THEN
            IRC    = 0.
            RAINC  = 0.
            TRWC   = 0.
            EVSWC  = 0.
            RNOFC  = 0
      END IF

!------Some calculation for communication between ORYZA and SAWAH
         DAY  = DOY
!------No differentiation between rainfall and irrigation is SAWAH
         RAIN = RAIN + IR
!--      In SAWAH, evaporation and transpiration fluxes are negative
         TRC = -1. * TRC
         EVSC = -1. * EVSC
!--      Also set SAWAH-calculated soil evaporation to negative values
         EVSW = -1. * EVSW
         DO I=1,NL
           TRWL(I)=-1. * TRWL(I)
         END DO         

         CALL DRSAWA (ITASK,IUNITD,IUNITL,IOUT,FILEI2, 
     $                   ESTAB, CROPSTA,
     $                   TIME,DAY,DELT,EVSC,RAIN,TRC,TRWL,TRW,RUNOF, 
     $                   INLAY,TKL,TKLT,WCADX,WCWPX,WCFCX,WCSTX, 
     $                   EVSW,FLXQT,WCLQT,WL0QT,ZEQT,ZW, 
     $                   DRAICU,UPRICU,EVSWCU,RAINIRCU,RNOFCU,TRWCU, 
     $                   FLXCU,WCUMCO,WL0CO,PRHEAD,DTAV,ZRTMS)

          NL   = INLAY
          WCAD = WCADX
          WCWP = WCWPX
          WCFC = WCFCX
          WCST = WCSTX

!-----    Standing water depth in ORYZA is in mm (in SAWAH in m):
          WL0  = WL0QT * 1000.
!-----    MSKPA in ORYZA is in kPa (in SAWAH PRHEAD is in hPa):
          DO I=1,NL
             MSKPA(I) = -1. * PRHEAD(I)/10.
             IF (MSKPA(I) .LE. 0.) MSKPA(I) = 0.
          END DO

!------ Separate rainfall and irrigation again
          RAIN = RAIN - IR

!--      Reset transpiration fluxes to positive values
!--      Also set soil evaporation to negative values
         EVSW = -1. * EVSW
         TRC = -1. * TRC
         EVSC = -1. * EVSC
         DO I=1,NL
           TRWL(I)=-1. * TRWL(I)
         END DO

!------Output writing 
      IF (ITASK .EQ. 2) THEN
         IF (OUTPUT) THEN
            INCLUDE 'WRSAWAH.INC'
         END IF
      END IF

!BB new, March 2005
      IF (ITASK .EQ. 3) THEN
! Summation of state variables in main field only!
!------ Start in main field only when crop is present
         IF ((ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) .OR. 
     $      (ESTAB.EQ.'DIRECT-SEED'.AND.CROPSTA.GE.1)) THEN
            IRC    = IRC    + IR*DELT
            RAINC  = RAINC  + RAIN *DELT
            TRWC   = TRWC   + TRW  *DELT
            EVSWC  = EVSWC  + EVSW *DELT
            RNOFC  = RNOFC  + RUNOF * DELT
         END IF
      END IF

!BB new, March 2005
      IF (ITASK .EQ. 4) THEN
         TRWC   = -1. * TRWC
         RNOFCU = -1. * RNOFCU
         RNOFC  = -1. * RNOFC
         CALL OPSTOR ('RAINC' , RAINC )
         CALL OPSTOR ('IRC' , IRC )
         CALL OPSTOR ('TRWC'  , TRWC  )
         CALL OPSTOR ('EVSWC' , EVSWC )
         CALL OPSTOR ('RNOFCU' , RNOFCU )
         CALL OPSTOR ('RNOFC' , RNOFC )
      END IF


      RETURN
      END


!---------------------------------------------------------------------!
!  SUBROUTINE DRSAWA                                                  !
!                                                                     !
!  Authors: Kees Rappoldt and Willem Stol                             !
!  Date   : July 17, 1992                                             !
!  Version: 3.1                                                       !
!                                                                     !
!  NOTE: In 1992 a new version of TTUTIL required adaptation of this  !
!        driver. The CALL's to RDDATA were changed into CALL's to     !
!        a new subroutine RDFREA.                                     !
!                                                                     !
!  Purpose: DRSAWA is the interface between the SAWAH module and the  !
!           user-defined FORTRAN MAIN. It performs the tasks of       !
!           - initialization                                          !
!           - rate calculation                                        !
!           - integration                                             !
!           - terminal treatments                                     !
!        The use of this subroutine is described in the SAWAH user's  !
!        manual. Background information on the used program structure !
!        and documentation on the called utilities can be found in:   !
!        D.W.G.van Kraalingen and C.Rappoldt,                         !
!          Subprograms in simulation models,                          !
!          Simulation Report CABO-TT no 18, 1989.                     !
!        C.Rappoldt and D.W.G.van Kraalingen,                         !
!          Reference manual of the FORTRAN utility library TTUTIL,    !
!          Simulation Report CABO-TT no 20, 1990.                     !
!                                                                     !
!  FORMALRAMETERS:  (I=input,O=output,C=control,IN=init,T=time)       !
! name   type meaning                                      units  class!
! ----   ---- -------                                      -----  -----!
! control                                                              !
! ITASK   I4  determines action of routine                   -     C,I !
! IUNIT   I4  unit number to be used, see file usage below   -   IN,C,I!
! IUNLOG  I4  unit number in use for LOG FILE                -   IN,C,I!
!             = 0, no log file is used or assumed to exist             !
!             > 0, error messages are written to log file              !
! IOUT    I4  output control with five digit integer number  -   IN,C,I!
!             last digit refers to first file, examples                !
!             10001 --> WATER5.OUT and WATER1.OUT                      !
!                11 --> WATER2.OUT and WATER1.OUT                      !
!               110 --> WATER3.OUT and WATER2.OUT                      !
!             11111 --> All 5 output files are produced                !
!             When the last  figure is 2 instead of 1, then            !
!             a table of soil characteristic water contents            !
!             is added to the header of WATER1.OUT. So for             !
!             full output enter IOUT=11112                             !
! FILIN   C*  name of file with soil data                    -   IN,C,I!
!                                                                      !
! time variables                                                       !
! TIME    R4  simulation time                                d     T,I !
! DAY     R4  calendar day number (groundwater,output)       -      I  !
! DELT    R4  time step                                      d     T,I !
!                                                                      !
! dynamic input                                                        !
! EVSC    R4  potential evaporation rate                    mm/d    I  !
! RAIN    R4  rainfall + irrigation rate                    mm/d    I  !
! TRWL    R4  actual transpiration rate per layer           mm/d    I  !
! TRC     R4  potential transpiration rate of crop          mm/d    I  !
!                                                                      !
! soil description (available after initial call)                      !
! INLAY   I4  number of layers specified in input file       -    IN,O !
! TKL     R4  thickness of soil compartments                 m    IN,O !
! WCADX   R4  volumetric water content airdry                -    IN,O !
! WCWPX   R4  volumetric water content at wilting point      -    IN,O !
! WCFCX   R4  volumetric water content at field capacity     -    IN,O !
! WCSTX   R4  volumetric water content at saturation         -    IN,O !
!                                                                      !
! dynamic output                                                       !
! EVSW    R4  actual (realized) evaporation rate            mm/d    O  !
! FLXQT   R4  layer boundary fluxes (rates)                 mm/d    O  !
! WCLQT   R4  volumetric soil water content per layer        -      O  !
! WL0QT   R4  depth of surface water layer                   m      O  !
! ZEQT    R4  depth of evaporation front                     m      O  !
!                                                                      !
! cumulated, derived and help variables                                !
! DRAICU  R4  cumulative drainage by drains                  mm     O  !
! UPRICU  R4  cumulative capp. rise over lower soil boundary mm     O  !
! EVSWCU  R4  cumulative evaporation                         mm     O  !
! RAINIRCU  R4  cumulative rainfall plus irrgation           mm     O  !
! RNOFCU  R4  cumulative runoff                              mm     O  !
! TRWCU   R4  cumulative transpiration                       mm     O  !
! FLXCU   R4  cumulative flux for each layer boundary        mm     O  !
! WCUMCO  R4  contribution of soil storage term to                     !
!             overall water balance                          mm     O  !
! WL0CO   R4  contribution of surface storage term to                  !
!             overall water balance                          mm     O  !
! PRHEAD  R4  pressure head at compartment center           hPa     O  !
! DTAV    R4  DELT-averaged DT value                         d      O  !
!                                                                      !
! SUBPROGRAMS called :                                                 !
!  - from library  SAWAH: SUCONV, SUERR , SUGRHD, SUINTG, SUMFLP       !
!                         SUMSKM, SUSAWA, SUSEFL, SUSLIN, SUSTCH       !
!                         SUSTFL, SUSTHH, SUSTMD, SUSTMS, SUUNST       !
!                         SUWCH , SUWCMS, SUZECA                       !
!  - from library TTUTIL: DECCHK, DECINT, DECREA,  ERROR, EXTENS       !
!                         FOPENG, IFINDC,   ILEN,   LINT, RDAREA       !
!                         RDDATA, RDFREA, RDINDX, RDINIT, RDSINT       !
!                         RDSREA, UPPERC                               !
!                                                                      !
! FILE usage : - Soil definition file opened and closed for ITASK=1    !
!                unit numbers used are IUNIT and IUNIT+1               !
!              - For IOUT>0 output files are generated using the unit  !
!                numbers IUNIT+2,IUNIT+3,IUNIT+4,IUNIT+5 and IUNIT+6   !
!----------------------------------------------------------------------!

      SUBROUTINE DRSAWA (ITASK,IUNIT,IUNLOG,IOUT,FILIN,
     $                   ESTAB, CROPSTA,
     $                   TIME,DAY,DELT,EVSC,RAIN,TRC,TRWL,TRW,RUNOF,
     $                   INLAY,TKL,TKLT,WCADX,WCWPX,WCFCX,WCSTX,
     $                   EVSW,FLXQT,WCLQT,WL0QT,ZEQT,ZW,
     $                   DRAICU,UPRICU,EVSWCU,RAINIRCU,RNOFCU,TRWCU,
     $                   FLXCU,WCUMCO,WL0CO,PRHEAD,DTAV,ZRTMS)

!-----subroutine arguments
      INTEGER ITASK,  IUNIT, IUNLOG,   IOUT,  INLAY
!     simple real variables
      REAL     TIME,    DAY,   DELT,   EVSC,   RAIN, EVSW
      REAL    WL0QT,   ZEQT, DRAICU, UPRICU, EVSWCU, RAINIRCU
      REAL   RNOFCU,  TRWCU, WCUMCO,  WL0CO,   DTAV, TRC
      REAL   TKLT, ZRTMS
!     arrays (length declared below)
      REAL     TRWL,    TKL,  WCADX,  WCWPX,  WCFCX,  WCSTX
      REAL    FLXQT,  WCLQT,  FLXCU, PRHEAD
!     other type
      CHARACTER*(*)   FILIN

!-----local and water model common variables ; integers
      INTEGER     I, IDRAIN,    ILF,  ILOUT, ILZMAX,  ITOLD
      INTEGER  ITYL,   IRUN,  IZWTB, ILUNIT,    IUN,   IERR
      INTEGER    NL, NRDTYP, NVGTYP,  IWRIT,  SWIT3,  SWIT5
      INTEGER SWIT6,  SWIT7,  SWIT8,  SWIT9

!     simple real variables
      REAL    AIRDR,  CKWFL,  CKWIN,    CSA,    CSB,   CSC2
      REAL   DRAIQT, UPRISE,   DTFX,  DTMIN,  DTMX1,  FIELD
      REAL    LTIME, PROREL,  RUNOF, SURREL,   TINY,    TRW
      REAL     WCUM, WCUMCH,  WCUMI,  WILTP,  WL0CH,  WL0MX
      REAL   WL0QTI,   ZECH,  ZEQTI,    ZLT,     ZW,    ZWI
!     arrays (length declared below)
      REAL    KMSA1, KMSA1T,  KMSA2, KMSA2T,  KMSMX, KMSMXT
      REAL      KST,   KSTT,  MSWCA, MSWCAT, PFWC00, PFWC01
      REAL   PFWC02, PFWC03, PFWC04, PFWC05, PFWC06, PFWC07
      REAL   PFWC08, PFWC09, PFWC10,     PN,    TYL,    VGA
      REAL     VGAT,    VGL,   VGLT,    VGN,   VGNT,    VGR
      REAL    VGWRT,  VGWST,  VGKST,   WCAD,   WCFC,  WCLCH
      REAL   WCLEQI, WCLQTI, WCLQTM,   WCST,  WCSTT,   WCWP
      REAL     ZWTB
!-----TAO LI, MAR 25, 2009
      REAL DRAICUM1

!     other types
      CHARACTER*6     STRWCL, STRTRW, STRFLX
      CHARACTER*10    FILNAM
      CHARACTER*80    ESTAB
      INTEGER         CROPSTA
      LOGICAL OKINIT, OPENED, USEFIL
     
!-----functions called
      INTEGER ILEN
      REAL LINT

!-----Soil characteristics according to Rijtema/Driessen
!     The number of soil types defined is NRDTYP
      PARAMETER (NRDTYP=20)
      DIMENSION KMSA1T(NRDTYP), KMSMXT(NRDTYP),  KSTT(NRDTYP)
      DIMENSION KMSA2T(NRDTYP), MSWCAT(NRDTYP), WCSTT(NRDTYP)

!-----Soil characteristics according to Van Genuchten system
!     The number of soil types defined is NVGTYP
      PARAMETER (NVGTYP=2)
      DIMENSION VGWRT(NVGTYP), VGWST(NVGTYP), VGKST(NVGTYP)
      DIMENSION  VGAT(NVGTYP),  VGNT(NVGTYP),  VGLT(NVGTYP)

!-----length:10 (max. nr of layers)
      DIMENSION  TRWL(10), WCLEQI(10), WCLQTM(10)
      DIMENSION WCLQT(10), WCLQTI(10),  WCLCH(10)
      DIMENSION   TKL(10),    TYL(10), PRHEAD(10)

!-----sawah common blocks (cannot contain arguments)
      COMMON /VOLWAT/  WCAD(10) ,  WCFC(10),  WCST(10),  WCWP(10)
      COMMON /HYDCON/  KMSMX(10), KMSA1(10), KMSA2(10),   KST(10)
      COMMON /POWER /  PN(10)
      COMMON /NUCHT /  VGN(10)  ,   VGA(10),   VGR(10),   VGL(10)
      COMMON /PFCURV/  MSWCA(10),PFWC00(10),PFWC01(10),PFWC02(10),
     $                PFWC03(10),PFWC04(10),PFWC05(10),PFWC06(10),
     $                PFWC07(10),PFWC08(10),PFWC09(10),PFWC10(10)
      COMMON /SWITCH/  SWIT3, SWIT5, SWIT7, SWIT8, SWIT9
      COMMON /WRITS /  IWRIT

!-----parameter used in error checking pF-function
      PARAMETER (TINY=0.0001)

!-----copies of volumetric water contents (subroutine arguments)
      DIMENSION WCADX(10), WCWPX(10), WCFCX(10), WCSTX(10)

!-----array with time-integrated values of FLX
!     and DELT averaged value of FLX
      DIMENSION FLXCU(11), FLXQT(11)
      
!-----groundwater table
      PARAMETER (ILZMAX=400)
      DIMENSION ZWTB(ILZMAX)

!-----file header strings
      DIMENSION STRWCL(10),STRTRW(10),STRFLX(11)
!     output control
      DIMENSION OPENED(5), USEFIL(5), IUN(5)

!(BB): Added
      LOGICAL RWCLI
      CHARACTER (3) RIWCLI
      CHARACTER (5) SCODE
      INTEGER       WCLINT(3*10)
!-----variables retain their values between subsequent calls
      SAVE


!--Soil type properties according to the Rijtema/Driessen combination.
!  Data from these tables will be used at the simultaneous occurrence
!  of the following switch values:
!         SWIT9=2  and (SWIT3=1 or 2) and (SWIT8=1)
!  The 6 soil properties are given in data tables for NRDTYP soils.
!  (for declarations, see above).  By defining the soil type TYL(I)
!  for each layer I, a consistent combination of soil properties
!  is selected. Data refer to the twenty standard soil types according
!  to Rijtema (as described by Driessen, 1986).
!
!      1. Coarse sand                11. Fine sandy loam
!      2. Medium coarse sand (mcs)   12. Silt loam
!      3. Medium fine sand           13. Loam
!      4. Fine sand                  14. Sandy clay loam
!      5. Humous loamy mcs           15. Silty clay loam
!      6. Light loamy mcs            16. Clay loam
!      7. Loamy mcs                  17. Light clay
!      8. loamy fine sand            18. Silty clay
!      9. Sandy loam                 19. Heavy clay
!     10. Loess loam                 20. Peat

!     ALPHA in cm-1 (Rijtema/Driessen, simple and extended)
      DATA KMSA1T  /.1960, .1385, .0821, .0500, .0269, .0562, .0378,
     $              .0395, .0750, .0490, .0240, .0200, .0231, .0353,
     $              .0237, .0248, .0274, .0480, .0380, .1045/
     
!     a in cm**2.4/d (Rijtema/Driessen, extended)
      DATA KMSA2T    /.08,   .63,  3.30, 10.90, 15.00,  5.26,  2.10,
     $              16.40,   .24, 22.60, 26.50, 47.30, 14.40, 33.60,
     $               3.60,  1.69,  2.77, 28.20,  4.86,  6.82/
     
!     transition suction in cm (Rijtema/Driessen, extended)
      DATA KMSMXT   /80.0,  90.0, 125.0, 175.0, 165.0, 100.0, 135.0,
     $              200.0, 150.0, 130.0, 300.0, 300.0, 300.0, 200.0,
     $              300.0, 300.0, 300.0,  50.0,  80.0,  50.0/
     
!     saturated conductivity in cm/d (Rijtema/Driessen)
      DATA KSTT   /1120.0, 300.0, 110.0,  50.0,  1.00,   2.3,   .36,
     $              26.50, 16.50, 14.50,  12.0,  6.50,  5.00, 23.50,
     $               1.50,   .98,  3.50,  1.30,   .22,  5.30/
     
!     gamma, dimensionless (Rijtema/Driessen)
      DATA MSWCAT  /.0853, .0450, .0366, .0255, .0135, .0153, .0243,
     $              .0299, .0251, .0156, .0186, .0165, .0164, .0101,
     $              .0108, .0051, .0085, .0059, .0043, .0108/
     
!     saturated soil moisture content, dimensionless (Rijtema/Driessen)
      DATA WCSTT   /.3950, .3650, .3500, .3640, .4700, .3940, .3010,
     $              .4390, .4650, .4550, .5040, .5090, .5030, .4320,
     $              .4750, .4450, .4530, .5070, .5400, .8630/

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

!     Van Genuchten form:
!     TETA-r, dimensionless
      DATA VGWRT /  .0448,   .0000/
!     TETA-s, dimensionless 
      DATA VGWST /  .4012,   .4505/
!     K-s in cm/d
      DATA VGKST /  .7985, 25.134 /
!     ALPHA in cm-1
      DATA VGAT  /  .0036,   .0067/
!     N, dimensionless
      DATA VGNT  / 1.5007,  1.2318/
!     L, dimensionless
      DATA VGLT  /-2.418 ,   .0001/
!     end van Genuchten form

!-----initialize some other variables
      DATA ITOLD/4/, IRUN/0/, OKINIT/.FALSE./
      DATA OPENED /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
!     character strings used for output file headers
      DATA STRWCL/'  WCL1','  WCL2','  WCL3','  WCL4','  WCL5',
     $            '  WCL6','  WCL7','  WCL8','  WCL9',' WCL10'/
      DATA STRTRW/' TRWL1',' TRWL2',' TRWL3',' TRWL4',' TRWL5',
     $            ' TRWL6',' TRWL7',' TRWL8',' TRWL9','TRWL10'/
      DATA STRFLX/'FLXQT1','   ..2','   ..3','   ..4','   ..5',
     $   '   ..6','   ..7','   ..8','   ..9','  ..10','  ..11'/


!-----initial section
!-----===============
      IF (ITASK.EQ.1) THEN
!        error check
         IF (ITOLD.NE.4) CALL FATALERR ('DRSAWA',
     $    'cannot re-initialize model without terminal call')

!        SUSAWA model run number
         IRUN = IRUN + 1
!        local copy of unit number
         IF (IRUN.EQ.1) ILUNIT = IUNIT

!--------indicate to suslin that call is from FORTRAN main
         IWRIT = 1

!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        reading data from input file FILIN
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         CALL RDINIT (ILUNIT,IUNLOG,FILIN)
         WRITE (*,'(2A)') ' DRSAWA reads data from ',FILIN

!(BB): Added
!----- First, read code to recognize the correctness of supplied soil file
         CALL RDSCHA('SCODE',SCODE)
         IF (SCODE .NE. 'SAWAH') THEN
            CALL FATALERR ('SAWAH','Wrong soil input file')
         END IF

!        reading of option switches
         CALL RDSINT ('SWIT3 ',SWIT3 )
         CALL RDSINT ('SWIT5 ',SWIT5 )
         CALL RDSINT ('SWIT6 ',SWIT6 )
         CALL RDSINT ('SWIT7 ',SWIT7 )
         CALL RDSINT ('SWIT8 ',SWIT8 )
         CALL RDSINT ('SWIT9 ',SWIT9 )
!        switch consistency check
         IF (SWIT9.EQ.2 .AND. 
     $    ((SWIT3.EQ.1.AND.SWIT8.NE.1) .OR.
     $     (SWIT3.EQ.2.AND.SWIT8.NE.1) .OR.
     $     (SWIT3.EQ.3.AND.SWIT8.NE.2) .OR.
     $     (SWIT3.NE.1.AND.SWIT3.NE.2.AND.SWIT3.NE.3)))
     $    CALL FATALERR ('DRSAWA','switch inconsistency SWIT3/8/9')

!--------time discretization
         CALL RDSREA ('DTMIN',DTMIN)
         CALL RDSREA ('DTMX1',DTMX1)
         IF (SWIT5.EQ.2) CALL RDSREA ('DTFX',DTFX )

!--------space discretization
!        compartment number + copy to output ; layer thickness
         CALL RDSINT ('NL',NL)
         INLAY = NL
!        exactly NL array elements required:
         CALL RDFREA ('TKL',TKL,10,NL)

!--------initial values of some state variables
!        moisture content; only for SWIT6=2
         IF (SWIT6.EQ.2) CALL RDFREA 
     $    ('WCLQTM',WCLQTM,10,NL)
!        surface water depth
         CALL RDSREA ('WL0QTI',WL0QTI)
!        evaporation front depth
         CALL RDSREA ('ZEQTI ',ZEQTI )

!--------soil hydraulic conductivity and moisture characteristic
         IF (SWIT9.EQ.1) THEN
!           properties not automatically assigned by soil type but
!           quantified by user, per layer and per property
!           conductivity:
            DO 8 I=1,INLAY
               TYL(I) = 0.0
8           CONTINUE
            IF (SWIT3.EQ.1) THEN
               CALL RDFREA ('KST'  ,KST  ,10,NL)
               CALL RDFREA ('KMSA1',KMSA1,10,NL)
            ELSE IF (SWIT3.EQ.2) THEN
               CALL RDFREA ('KST'  ,KST  ,10,NL)
               CALL RDFREA ('KMSA1',KMSA1,10,NL)
               CALL RDFREA ('KMSA2',KMSA2,10,NL)
               CALL RDFREA ('KMSMX',KMSMX,10,NL)
            ELSE IF (SWIT3.EQ.3) THEN
               CALL RDFREA ('KST'  ,KST  ,10,NL)
               CALL RDFREA ('VGN'  ,VGN  ,10,NL)
            ELSE IF (SWIT3.EQ.4) THEN
               CALL RDFREA ('KST'  ,KST  ,10,NL)
               CALL RDFREA ('PN'   ,PN   ,10,NL)
            ELSE IF (SWIT3.EQ.5) THEN
!              user must specify conductivity parameters to be read
!              and include error check
               WRITE (*,'(2(/,A))')
     $          ' Reading of parameters for user-specified',
     $          ' conductivity function not properly adapted in DRSAWA'
               CALL FATALERR 
     $           ('DRSAWA','cannot read conductivity function')
            END IF

!-----------moisture characteristic
            IF (SWIT8.EQ.1) THEN
               CALL RDFREA ('WCST' ,WCST ,10,NL)
               CALL RDFREA ('MSWCA',MSWCA,10,NL)
            ELSE IF (SWIT8.EQ.2) THEN
               CALL RDFREA ('WCST',WCST,10,NL)
               CALL RDFREA ('VGA' ,VGA ,10,NL)
               CALL RDFREA ('VGL' ,VGL ,10,NL)
               CALL RDFREA ('VGR' ,VGR ,10,NL)
               CALL RDFREA ('VGN' ,VGN ,10,NL)
            ELSE IF (SWIT8.EQ.3) THEN
               CALL RDFREA ('WCST',WCST,10,NL)
!              read pF values
               CALL RDFREA ('PFWC00',PFWC00,10,NL)
               CALL RDFREA ('PFWC01',PFWC01,10,NL)
               CALL RDFREA ('PFWC02',PFWC02,10,NL)
               CALL RDFREA ('PFWC03',PFWC03,10,NL)
               CALL RDFREA ('PFWC04',PFWC04,10,NL)
               CALL RDFREA ('PFWC05',PFWC05,10,NL)
               CALL RDFREA ('PFWC06',PFWC06,10,NL)
               CALL RDFREA ('PFWC07',PFWC07,10,NL)
               CALL RDFREA ('PFWC08',PFWC08,10,NL)
               CALL RDFREA ('PFWC09',PFWC09,10,NL)
               CALL RDFREA ('PFWC10',PFWC10,10,NL)

!--------------check inputs for pF curve interpolation
               IERR = 0
               DO 10 I=1,NL
!                 in descending order ?
                  IF (PFWC01(I).GE.PFWC00(I)) IERR=IERR+1
                  IF (PFWC02(I).GE.PFWC01(I)) IERR=IERR+1
                  IF (PFWC03(I).GE.PFWC02(I)) IERR=IERR+1
                  IF (PFWC04(I).GE.PFWC03(I)) IERR=IERR+1
                  IF (PFWC05(I).GE.PFWC04(I)) IERR=IERR+1
                  IF (PFWC06(I).GE.PFWC05(I)) IERR=IERR+1
                  IF (PFWC07(I).GE.PFWC06(I)) IERR=IERR+1
                  IF (PFWC08(I).GE.PFWC07(I)) IERR=IERR+1
                  IF (PFWC09(I).GE.PFWC08(I)) IERR=IERR+1
                  IF (PFWC10(I).GE.PFWC09(I)) IERR=IERR+1
!                 start and end points
                  IF (ABS(PFWC00(I)-7.).GT.TINY) IERR=IERR+1
                  IF (ABS(PFWC10(I)   ).GT.TINY) IERR=IERR+1
10             CONTINUE
!              error occurred ?
               IF (IERR.NE.0) THEN
                  WRITE(*,'(2(/,A),/,1X,I3,A)')
     $             ' Input pF-values for interpolation not in',
     $             ' descending order or invalid start/end points:',
     $             IERR,' ERROR(S) in pF-data points'
                  CALL FATALERR ('DRSAWA','Execution terminated')
               END IF

            ELSE IF (SWIT8.EQ.4) THEN
!              user must specify pf-curve parameters to be read
!              and include error check
               WRITE (*,'(2(/,A))')
     $          ' Reading of parameters for user-specified',
     $          ' pF-function not properly adapted in DRSAWA'
               CALL FATALERR ('DRSAWA','cannot read pF-function')
            END IF

         ELSE IF (SWIT9.EQ.2) THEN
!           physical properties from soil type number
            CALL RDFREA ('TYL',TYL,10,NL)
         ELSE
            CALL FATALERR 
     $      ('DRSAWA','SWIT9 wrong value ; should be 1 or 2')
         END IF


!--------other soil parameters
!        soil evaporation properties
         CALL RDSREA ('CSC2  ',CSC2 )
         CALL RDSREA ('CSA   ',CSA  )
         CALL RDSREA ('CSB   ',CSB  )
!        maximum surface water storage
         CALL RDSREA ('WL0MX ',WL0MX )
!        index of tube/mole drained compartment
         CALL RDSINT ('IDRAIN',IDRAIN)
!        groundwater interpolation table and table length
         CALL RDAREA ('ZWTB',ZWTB,ILZMAX,IZWTB)
         CALL RDSREA ('ZRTMS  ',ZRTMS)

!(BB)* Read information for resetting of moisture content values
         CALL RDSCHA('RIWCLI',RIWCLI)
         CALL RDFINT('WCLINT',WCLINT,3*10,3*NL)
         IF (RIWCLI.EQ.'YES') THEN
            RWCLI=.TRUE.
         ELSE IF (RIWCLI.EQ.'NO') THEN
            RWCLI=.FALSE.
         ELSE
            CALL FATALERR ('PADDY','unknown selection for Yes or NO')
         END IF


!        delete temporary file used by the input routines
         CLOSE (ILUNIT,STATUS='DELETE')
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        reading data from input file FILIN completed
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------initialize local time, used for checking purposes
         LTIME = TIME

!--------initialize cumulative amounts and cumulative differences
         DRAICU = 0.0
         UPRICU = 0.0
         EVSWCU = 0.0
         RAINIRCU = 0.0
         RNOFCU = 0.0
         TRWCU  = 0.0
         WCUMCO = 0.0
         WCUMI  = 0.0
         WL0CO  = 0.0
         DO 20 I=1,NL+1
            FLXCU(I) = 0.
20       CONTINUE


!--------Calculate depth of soil profile
         TKLT = 0.
         DO I=1,NL
            TKLT = TKLT+TKL(I)
         END DO

!--------initialize contribution to daily water balance
         PROREL =0.
         SURREL =0.

!--------physical soil definition for type option 
         IF (SWIT9.EQ.2) THEN
         DO 30 I=1,NL
!           soil type for this layer
            ITYL = NINT(TYL(I))
            IF (ITYL.LE.0) CALL FATALERR ('DRSAWA','soil type <= 0')
            IF (SWIT3.EQ.1 .OR. SWIT3.EQ.2) THEN
!              soil parameters from Rijtema-Driessen tables
               IF (ITYL.GT.NRDTYP) CALL FATALERR ('DRSAWA',
     $          'Rijtema-Driessen soil type number undefined')
               KMSA1(I) = KMSA1T(ITYL)
               KMSA2(I) = KMSA2T(ITYL)
               KMSMX(I) = KMSMXT(ITYL)
               KST(I)   = KSTT(ITYL)
               MSWCA(I) = MSWCAT(ITYL)
               WCST(I)  = WCSTT(ITYL)
            ELSE IF (SWIT3.EQ.3) THEN
!              soil parameters from Van Genuchten tables
               IF (ITYL.GT.NVGTYP) CALL FATALERR ('DRSAWA',
     $          'Van Genuchten soil type number undefined')
               VGR(I)  = VGWRT(ITYL)
               WCST(I) = VGWST(ITYL)
               KST(I)  = VGKST(ITYL)
               VGA(I)  = VGAT(ITYL)
               VGN(I)  = VGNT(ITYL)
               VGL(I)  = VGLT(ITYL)
            END IF
30       CONTINUE
         END IF

         DO 40 I=1,NL
!           water contents at selected suction values
            FIELD = 1.0E2
            WILTP = 1.6E4
            AIRDR = 1.0E7
            CALL SUWCMS (I,2,WCFC(I),FIELD)
            CALL SUWCMS (I,2,WCWP(I),WILTP)
            CALL SUWCMS (I,2,WCAD(I),AIRDR)
!           copies of common variables to calling program
            WCADX(I) = WCAD(I)
            WCWPX(I) = WCWP(I)
            WCFCX(I) = WCFC(I)
            WCSTX(I) = WCST(I)
40       CONTINUE

!        depth of free groundwater level below soil surface on DAY
!        is initialized by lineair interpolation in table ZWTB
         ZWI   = LINT (ZWTB,IZWTB,DAY)

!        depth of water layer on surface and evaporation front
!        are initialised with initial values         
!
         WL0QT = WL0QTI
         ZEQT  = ZEQTI

!--------initial call to water transport model
         CALL SUSAWA (ITASK,IUNLOG,WCLQT,WL0QT,NL,TRWL,EVSC,RAIN,ZWI,
     $              TKL,TYL,DELT,DTMIN,DTMX1,DTFX,IDRAIN,WL0MX,ZEQT,
     $              CSA,CSB,CSC2,WCLCH,WL0CH,WCLEQI,EVSW,RUNOF,UPRISE,
     $              DRAIQT,WCUMCH,ZECH,ZLT,FLXQT,PRHEAD,DTAV)

!--------initial water content
         IF (SWIT6.EQ.1) THEN
!           initialization at equilibrium moisture content
            DO 50 I=1,NL
               WCLQTI(I) = WCLEQI(I)
50          CONTINUE

         ELSE IF (SWIT6.EQ.2) THEN
!           initialization at observed initial moisture content
            DO 60 I=1,NL
               WCLQTI(I) = WCLQTM(I)
60          CONTINUE

         ELSE IF (SWIT6.EQ.3) THEN
!           initialization at wilting point
            DO 70 I=1,NL
               WCLQTI(I)=WCWP(I)
70          CONTINUE
         END IF


!--------initialize status
         DO 80 I=1,NL
!           total water content
            WCUMI    = WCUMI + WCLQTI(I) * TKL(I) * 1000.
!           initialization layer water content
            WCLQT(I) = WCLQTI(I)
80       CONTINUE
         WCUM = WCUMI

!--------open non-existing files ; set logicals for file use
         ILOUT = IOUT
         DO 90 I=1,5
            USEFIL(I) = MOD(ILOUT,10).GT.0
            IF (USEFIL(I).AND..NOT.OPENED(I)) THEN
!              file has yet to be opened
               IUN(I) = ILUNIT + I + 1
               WRITE (FILNAM,'(A,I1,A)') 'WATER',I,'.OUT'
               CALL FOPENG (IUN(I),FILNAM,'NEW','SF',0,'DEL')
               OPENED(I) = .TRUE.
            END IF
            ILOUT = ILOUT / 10
90       CONTINUE

         IF (USEFIL(1)) THEN
!           write header to water1.out
            ILF = ILEN (FILIN)
            WRITE (IUN(1),'(A,I3,A,/,A,/,2A)')
     $       ' SUSAWA RUN',IRUN,' ; SOIL STATUS',
     $       ' ===========================',
     $       ' SOIL DEFINITION READ FROM FILE ',FILIN(1:ILF)
            IF (MOD(IOUT,10).EQ.2) THEN
!              write soil characteristics to this file
               WRITE (IUN(1),'(A,//,2A)')
     $          ' SOIL CHARACTERISTICS PER COMPARTMENT:',
     $          ' COMPARTMENT TYPE NR   TKL(M)   WCAD    WCWP',
     $          '    WCFC    WCST'
               DO 100 I=1,NL
                  WRITE (IUN(1),'(3X,I4,8X,F4.0,4X,F5.3,3X,4(F5.4,3X))')
     $             I,TYL(I),TKL(I),WCAD(I),WCWP(I),WCFC(I),WCST(I)
100            CONTINUE
            END IF
            WRITE (IUN(1),'(/,A,10A6)')
     $       ' DAY   WL0    ZE   ',(STRWCL(I),I=1,NL)
            WRITE (IUN(1),'(A)') '        mm    mm'
         END IF

         IF (USEFIL(2)) THEN
!           write header to water2.out
            WRITE (IUN(2),'(A,I3,A,/,A,/,A,10A7)')
     $       ' SUSAWA RUN',IRUN,' ; TRANSPIRATION/COMPTMNT (MM/DAY)',
     $       ' =====================================================',
     $       ' DAY  ',(STRTRW(I),I=1,NL)
            WRITE (IUN(2),'(1X)')
         END IF

         IF (USEFIL(3)) THEN
!           write header to water3.out
            WRITE (IUN(3),'(A,I3,A,/,A,/,A,//,2A,/)')
     $       ' SUSAWA RUN',IRUN,' ; DAILY WATER IN/OUT (MM/DAY)',
     $       ' =================================================',
     $       ' Note: systems GAINS are POSITIVE, LOSSES NEGATIVE ',
     $       ' DAY    R+IR  RUNOF    TRW  (TRC)    EVSW (EVSC)  ',
     $       ' UPRISE DRAIQT  PROREL  SURREL'
         END IF

         IF (USEFIL(4)) THEN
!           write header to water4.out
            WRITE (IUN(4),'(A,I3,A,/,A,/,A,//,2A,/)')
     $       ' SUSAWA RUN',IRUN,' ; CUMULATIVE AMOUNTS (MM)',
     $       ' =======================================',
     $       ' Note:  system GAINS are POSITIVE, LOSSES NEGATIVE ',
     $       ' DAY    R+IRCU   RNOFCU    TRWCU   EVSWCU   UPRICU ',
     $            '  DRAICU    WCUMCO    WL0CO '
         END IF

         IF (USEFIL(5)) THEN
!           write header to water5.out
            WRITE (IUN(5),'(A,I3,A,/,2A,/,A,11A6)')
     $       ' SUSAWA RUN',IRUN,
     $       ' ; FLUX THROUGH COMPARTMNT INTRFACES (MM/DAY)',
     $       ' =============',
     $       '=============================================',
     $       ' DAY  ',(STRFLX(I),I=1,NL+1)
            WRITE (IUN(5),'(1X)')
         END IF

!        initialization done
         OKINIT = .TRUE.


!-----rate calculation
!-----================
      ELSE IF (ITASK.EQ.2) THEN
!        error check
         IF (.NOT.OKINIT) CALL FATALERR 
     $   ('DRSAWA','model not initialized')
!        check time against local time
         IF (LTIME.NE.TIME) CALL FATALERR 
     $   ('DRSAWA','inconsistent time')

!(BB)-Re-initialize water contents at direct seeding or transplanting
!     if requested
        IF (RWCLI) THEN
           IF ((ESTAB.EQ.'TRANSPLANT'  .AND. CROPSTA.EQ.3) .OR. 
     $          (ESTAB.EQ.'DIRECT-SEED' .AND. CROPSTA.EQ.1)) THEN
             WCUM = 0.
             WL0QT = MIN(WL0QTI,WL0MX)
             DO I=1,NL
                WCLQT(I) = WCST(I)
                WCUM  = WCUM + WCLQTI(I)*TKL(I)*1000.
             END DO
           END IF
        END IF

!        groundwater level for current day
         ZW = LINT (ZWTB,IZWTB,DAY)

!--------rate call to water transport model
         CALL SUSAWA (ITASK,IUNLOG,WCLQT,WL0QT,NL,TRWL,EVSC,RAIN,ZW,
     $              TKL,TYL,DELT,DTMIN,DTMX1,DTFX,IDRAIN,WL0MX,ZEQT,
     $              CSA,CSB,CSC2,WCLCH,WL0CH,WCLEQI,EVSW,RUNOF,UPRISE,
     $              DRAIQT,WCUMCH,ZECH,ZLT,FLXQT,PRHEAD,DTAV)

!--------transpiration rate summed over all layers
         TRW = 0
         DO 110 I=1,NL
            TRW = TRW + TRWL(I)
110      CONTINUE

!--------dynamic output
         IF (USEFIL(1)) WRITE (IUN(1),'(F5.0,2F6.0,2X,10F6.3)') 
     $     DAY,WL0QT*1000.0,ZEQT*1000.0,(WCLQT(I),I=1,NL)
         IF (USEFIL(2)) WRITE (IUN(2),'(F5.0,1X,10F7.1)') 
     $     DAY,(TRWL(I),I=1,NL)
         IF (USEFIL(3)) WRITE
     $     (IUN(3),'(F5.0,2F7.1,2(F7.1,A,F5.1,A),F8.1,F7.1,2F8.1)')
     $     DAY,RAIN,RUNOF,TRW,' (',TRC,')',EVSW,' (',EVSC,')',
     $     UPRISE,DRAIQT,PROREL,SURREL
         IF (USEFIL(4)) WRITE (IUN(4),'(F5.0,6F9.1,1X,2F9.1)') 
     $     DAY,RAINIRCU,RNOFCU,TRWCU,EVSWCU,UPRICU,DRAICU,WCUMCO,WL0CO
         IF (USEFIL(5)) WRITE (IUN(5),'(F5.0,1X,11F6.0)') 
     $     DAY,(FLXQT(I),I=1,NL+1)


!-----integration of (dummy) rates
!-----============================
      ELSE IF (ITASK.EQ.3) THEN
!        error checks
         IF (.NOT.OKINIT) CALL FATALERR 
     $   ('DRSAWA','model not initialized')
         IF (ITOLD.EQ.1) THEN
!           model was initialized in previous call,
!           integration call allowed as a dummy ; nothing done
            CONTINUE
         ELSE IF (ITOLD.EQ.2) THEN
!           in previous call, rates were calculated
!           integration of water status is allowed now

!           increase of local time
            LTIME = LTIME + DELT

!           water status variables ; surface water level
            WL0QT  = WL0QT  + WL0CH  * DELT
!           depth of evaporation front
            ZEQT   = ZEQT   + ZECH   * DELT
            DO 120 I=1,NL
!              water content per layer
               WCLQT(I) = WCLQT(I) + WCLCH(I) * DELT
120         CONTINUE

!           cumulative fluxes at NL+1 depths
            DO 130 I=1,NL+1
               FLXCU(I) = FLXCU(I) + FLXQT(I) * DELT
130         CONTINUE

!           cumulative amounts; gains positive, losses negative
            DRAICU = DRAICU + DRAIQT * DELT
            UPRICU = UPRICU + UPRISE * DELT
            EVSWCU = EVSWCU + EVSW   * DELT
            RAINIRCU = RAINIRCU + RAIN   * DELT
            RNOFCU = RNOFCU + RUNOF  * DELT
            TRWCU  = TRWCU  + TRW    * DELT

!           total water stored in profile
            WCUM   = WCUM   + WCUMCH * DELT
!           contribution of profile to water balance; since start
            PROREL = -WCUMCH
            WCUMCO = WCUMCO + PROREL * DELT
!           contribution of surface water to water balance; since start
            SURREL = -1000.* WL0CH
            WL0CO  = WL0CO + SURREL * DELT

!-----------total change in system water content
            CKWIN = -(WCUMCO + WL0CO)
!           total of external contributions to system water content
            CKWFL = RAINIRCU + RNOFCU + EVSWCU + TRWCU + UPRICU + DRAICU
!           check this
            CALL SUWCHK (CKWFL,CKWIN,LTIME)

! BAS, 8 SEPTEMBER 2006: 3 CUMULATIVE AMOUNTS SAME NAMES AS OTHER WAT BALS
!---- Cumulative amounts From STTIME onwards
            DRAICUM1 = DRAICU + DRAIQT * DELT

         ELSE
!           error
            CALL FATALERR 
     $       ('DRSAWA','integration without valid rates')
         END IF


!-----terminal calculations
!-----=====================
      ELSE IF (ITASK.EQ.4) THEN
!        terminal output
         DO 140 I=1,5
            IF (USEFIL(I)) WRITE
     $       (IUN(I),'(A,///)') ' SIMULATION COMPLETED'
140      CONTINUE
!        model requires initialization from now on:
         OKINIT = .FALSE.

      ELSE
!        illegal switch
         CALL FATALERR ('DRSAWA','1 < ITASK > 4')
      END IF


!     save task as previous task
      ITOLD = ITASK
      RETURN
      END
