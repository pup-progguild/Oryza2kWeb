!----------------------------------------------------------------------!
! Program ORYZA2000: Simulation model for potential and water- and     !
!                    nitrogen-production of rice.                      !
! Version 4.0                                                          !
! Date      : December 2001                                            !
! Programmed: B.A.M. Bouman                                            !
! History   : Adapted from ORYZA1 (1995), and ORYZA_W (1995) models    !
!----------------------------------------------------------------------!
      PROGRAM ORYZA2000
      CALL FSE
      END

!----------------------------------------------------------------------*
!                                                                      *
!                                                                      *
!              FORTRAN Simulation Environment (FSE 2.1)                *
!                            March, 1995                               *
!                                                                      *
! THIS VERSION WITH MINR ADAPTATIONS FOR MODEL ORYZA 4.0 (BOUMAN)      *
!                                                                      *
!     FSE 2.1 is a simulation environment suited for simulation of     *
!     biological processes in time, such as crop and vegetation growth,*
!     insect population development etc.                               *
!                                                                      *
!     The MAIN program, subroutine FSE and subroutine MODELS are       *
!     programmed by D.W.G. van Kraalingen, DLO Institute for           *
!     Agrobiological and Soil Fertility Research (AB-DLO),             *
!     PO Box 14, 6700 AA, Wageningen, The Netherlands (e-mail:         *
!     d.w.g.van.kraalingen@ab.agro.nl).                                *
!                                                                      *
!     FSE version 2.1 is described in:                                 *
!     Kraalingen, D.W.G. van 1995. The FSE system for crop simulation, *
!     Simulation Report no. 23, Institute for Agrobiological and Soil  *
!     Fertility Research and Dept. of Theoretical Production Ecology,  *
!     Agricultural University Wageningen.                              *
!                                                                      *
!     Data files needed for FSE 2.1:                                   *
!          (excluding data files used by models called from MODELS):   *
!        - CONTROL.DAT (contains file names to be used),               *
!        - timer file whose name is specified in CONTROL.DAT,          *
!        - optionally, a rerun file whose name is specified in         *
!          CONTROL.DAT,                                                *
!        - weather data files as specified in timer file               *
!     Object libraries needed for FSE 2.1:                             *
!        - TTUTIL (at least version 3.2)                               *
!        - WEATHER (at least version from 17-Jan-1990)                 *
!     Differences with standard version:                               *
!        - common block FSECM1 activated                               *
!        - calls to OP* subroutines activated                          *
!        - RDFROM logical set to false (unused rerun variables not     *
!          fatal)                                                      *
!        - calls to OBS* routine activated                             *
!----------------------------------------------------------------------*

      SUBROUTINE FSE

      USE CHART

      IMPLICIT NONE

!-----Standard declarations for simulation and output control

      INTEGER       ITASK   , INSETS, ISET  , IPFORM, IL
      LOGICAL       OUTPUT  , TERMNL, RDINQR, STRUNF, ENDRNF
      CHARACTER (1) COPINF, DELTMP
      INTEGER       INPRS   , STRUN , ENDRUN

      INTEGER        IMNPRS
      PARAMETER     (IMNPRS=100)
      CHARACTER (11) PRSEL(IMNPRS)

!-----Declarations for time control
      INTEGER   IDOY, IYEAR
      REAL      DELT, DOY, FINTIM, PRDEL, STTIME, TIME, YEAR

!-----Declarations for weather system
      INTEGER   IFLAG    , ISTAT1, ISTAT2 , ISTN
      REAL      ANGA     , ANGB  , ELEV   , LAT , LONG, RDD
      REAL      TMMN     , TMMX  , VP     , WN  , RAIN
      LOGICAL   WTRMES   , WTRTER
      CHARACTER (80) WTRDIR
      CHARACTER (7)  CNTR
      CHARACTER (6)  WSTAT
      CHARACTER (1)  DUMMY

!-----Declarations for file names and units
      INTEGER   IUNITR   , IUNITD   , IUNITO   , IUNITL   , IUNITC
      CHARACTER (80) FILEON, FILEOL
      CHARACTER (80) FILEIC, FILEIR, FILEIT
      CHARACTER (80) FILEI1, FILEI2, FILEI3, FILEI4, FILEI5

!-----Declarations for observation data facility
      INTEGER   INOD , IOD

      INTEGER   IMNOD
      PARAMETER (IMNOD=100)
      INTEGER   IOBSD(IMNOD)

!-----For communication with OBSSYS routine
      COMMON /FSECM1/ YEAR,DOY,IUNITD,IUNITL,TERMNL

!     File name for control file and empty strings for input files 1-5.
!     WTRMES flags any messages from the weather system

      DATA FILEIC /'CONTROL.DAT'/
      DATA FILEI1 /' '/, FILEI2 /' '/, FILEI3 /' '/
      DATA FILEI4 /' '/, FILEI5 /' '/
      DATA WTRMES /.FALSE./

      DATA STRUNF /.FALSE./, ENDRNF /.FALSE./

!-----Unit numbers for control file (C), data files (D),
!     output file (O), log file (L) and rerun file (R).
      IUNITC = 10
      IUNITD = 20
      IUNITO = 30
      IUNITL = 40
      IUNITR = 50

!-----Open control file and read names of normal output file, log file
!     and rerun file (these files cannot be used in reruns)
      CALL RDINIT (IUNITC,0, FILEIC)
      CALL RDSCHA ('FILEON', FILEON)
      CALL RDSCHA ('FILEOL', FILEOL)
      CALL RDSCHA ('FILEIR', FILEIR)

!     check if start run number was found, if there, read it
      IF (RDINQR('STRUN'))  THEN
         CALL RDSINT ('STRUN',STRUN)
         STRUNF = .TRUE.
      END IF
!     check if end run number was found, if there, read it
      IF (RDINQR('ENDRUN')) THEN
         CALL RDSINT ('ENDRUN',ENDRUN)
         ENDRNF = .TRUE.
      END IF
      CLOSE (IUNITC)

!-----Open output file and possibly a log file
      CALL FOPENS  (IUNITO, FILEON, 'NEW', 'DEL')
      IF (FILEOL.NE.FILEON) THEN
         CALL FOPENS  (IUNITL, FILEOL, 'NEW', 'DEL')
      ELSE
         IUNITL = IUNITO
      END IF

!     initialization of logfile for processing of end_of_run values
!      CALL OPINIT

!-----See if rerun file is present, and if so read the number of rerun
!     sets from rerun file

      CALL RDSETS (IUNITR, IUNITL, FILEIR, INSETS)

!-----Initialise logfile for end-of-year state values
      CALL OPINIT

!======================================================================*
!======================================================================*
!                                                                      *
!                   Main loop and reruns begin here                    *
!                                                                      *
!======================================================================*
!======================================================================*

      IF (.NOT.ENDRNF) THEN
!        no end run was found in control.dat file
         ENDRUN = INSETS
      ELSE
         ENDRUN = MAX (ENDRUN, 0)
         ENDRUN = MIN (ENDRUN, INSETS)
      END IF

      IF (.NOT.STRUNF) THEN
!        no start run was found in control.dat file
         STRUN = 0
      ELSE
         STRUN = MAX (STRUN, 0)
         STRUN = MIN (STRUN, ENDRUN)
      END IF

      CALL ChartInit (IUNITO+2)

      DO ISET=STRUN,ENDRUN

      WRITE (*,'(A)') '   FSE 2.1: Initialize model'

!-----Select data set
      CALL RDFROM (ISET, .FALSE.)

      CALL ChartSetRunID (ISET)


!======================================================================*
!                                                                      *
!                        Initialization section                        *
!                                                                      *
!======================================================================*

      ITASK  = 1
      TERMNL = .FALSE.
      WTRTER = .FALSE.

!-----Read names of timer file and input files 1-5 from control
!     file (these files can be used in reruns)
      CALL RDINIT (IUNITC,IUNITL,FILEIC)
      CALL RDSCHA ('FILEIT', FILEIT)
      IF (RDINQR ('FILEI1')) CALL RDSCHA ('FILEI1', FILEI1)
      IF (RDINQR ('FILEI2')) CALL RDSCHA ('FILEI2', FILEI2)
      IF (RDINQR ('FILEI3')) CALL RDSCHA ('FILEI3', FILEI3)
      IF (RDINQR ('FILEI4')) CALL RDSCHA ('FILEI4', FILEI4)
      IF (RDINQR ('FILEI5')) CALL RDSCHA ('FILEI5', FILEI5)
      CALL RDSREA ('PRDEL' , PRDEL )
      CALL RDSINT ('IPFORM', IPFORM)
      CALL RDSCHA ('COPINF', COPINF)
      CALL RDSCHA ('DELTMP', DELTMP )
      CALL RDSINT ('IFLAG' , IFLAG)

!-----See if observation data variable exists, if so read it
      INOD = 0
      IF (RDINQR('IOBSD')) THEN
         CALL RDAINT ('IOBSD' , IOBSD, IMNOD, INOD)
         IF (IOBSD(1).EQ.0) INOD = 0
      END IF

!-----See if variable with print selection exists, if so read it
      INPRS = 0
      IF (RDINQR('PRSEL')) CALL RDACHA ('PRSEL',PRSEL,IMNPRS,INPRS)

      CLOSE (IUNITC)

!-----Read time, control and weather variables from timer file
      CALL RDINIT (IUNITD  , IUNITL, FILEIT)
      CALL RDSREA ('STTIME', STTIME)
      CALL RDSREA ('FINTIM', FINTIM)
      CALL RDSREA ('DELT'  , DELT  )
      CALL RDSINT ('IYEAR' , IYEAR )
      CALL RDSINT ('ISTN'  , ISTN  )
      CALL RDSCHA ('WTRDIR', WTRDIR)
      CALL RDSCHA ('CNTR'  , CNTR)
      CLOSE (IUNITD)


!---- If STTIME is not a whole day value, transfrom:
      STTIME = 1.0 *INT(STTIME)


!-----Initialize TIMER and OUTDAT routines
      CALL TIMER2 (ITASK, STTIME, DELT, PRDEL, FINTIM, &
                   IYEAR, TIME  , DOY , IDOY , TERMNL, OUTPUT)
      YEAR = REAL (IYEAR)

      CALL OUTDAT (ITASK, IUNITO, 'TIME', TIME)
      CALL ChartInitialGroup

!-----Open weather file and read station information and return
!     weather data for start day of simulation.
!     Check status of weather system, WTRMES flags if warnings or errors
!     have occurred during the whole simulation. WTRTER flags if the run
!     should be terminated because of missing weather

      CALL STINFO (IFLAG , WTRDIR, ' ', CNTR, ISTN, IYEAR, &
                   ISTAT1, LONG  , LAT, ELEV, ANGA, ANGB)
      CALL WEATHR (IDOY  , ISTAT2, RDD, TMMN, TMMX, VP, WN, RAIN)
      IF (ISTAT1.NE.0.OR.ISTAT2.NE.0) WTRMES = .TRUE.
      WSTAT  = '444444'
      IF (ABS (ISTAT2).GE.111111) THEN
         WRITE (WSTAT,'(I6)') ABS (ISTAT2)
      ELSE IF (ISTAT2.EQ.0) THEN
         WSTAT = '111111'
      END IF

!-----initialize OBSSYS routine
      IF (ITASK.EQ.1) CALL OBSINI

!-----Conversion of total daily radiation from kJ/m2/d to J/m2/d
      RDD = RDD*1000.

!-----Call routine that handles the different models
      CALL MODELS (ITASK , IUNITD, IUNITO, IUNITL, &
                   FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                   OUTPUT, TERMNL, &
                   DOY   , IDOY  , YEAR  , IYEAR , STTIME,  &
                   TIME  , DELT  , &
                   LAT   , WSTAT , WTRTER, &
                   RDD   , TMMN  , TMMX  , VP    , WN, RAIN)


!======================================================================*
!                                                                      *
!                      Dynamic simulation section                      *
!                                                                      *
!======================================================================*

      WRITE (*,'(A)') '   FSE 2.1: DYNAMIC loop'

      DO WHILE (.NOT.TERMNL)

!----------------------------------------------------------------------*
!                     Integration of rates section                     *
!----------------------------------------------------------------------*

      IF (ITASK.EQ.2) THEN

!--------Carry out integration only when previous task was rate
!        calculation

         ITASK = 3

!--------Call routine that handles the different models
         CALL MODELS (ITASK , IUNITD, IUNITO, IUNITL, &
                      FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                      OUTPUT, TERMNL, &
                      DOY   , IDOY  , YEAR  , IYEAR , STTIME,  &
                      TIME  , DELT  , &
                      LAT   , WSTAT , WTRTER, &
                      RDD   , TMMN  , TMMX  , VP    , WN, RAIN)

!--------Turn on output when TERMNL logical is set to .TRUE.
         IF (TERMNL.AND.PRDEL.GT.0.) OUTPUT = .TRUE.

      END IF

!----------------------------------------------------------------------*
!               Calculation of driving variables section               *
!----------------------------------------------------------------------*

      ITASK = 2

!-----Write time of output to screen and file
      CALL OUTDAT (2, 0, 'TIME', TIME)
      CALL ChartNewGroup
      CALL ChartOutputRealScalar('TIME', TIME)

      IF (OUTPUT) THEN
         IF (ISET.EQ.0) THEN
            WRITE (*,'(13X,A,I5,A,F7.2)') &
              'Default set, Year:', IYEAR, ', Day:', DOY
         ELSE
            WRITE (*,'(13X,A,I3,A,I5,A,F7.2)') &
              'Rerun set:', ISET, ', Year:', IYEAR, ', Day:', DOY
         END IF
      END IF

!-----Get weather data for new day and flag messages
      CALL STINFO (IFLAG , WTRDIR, ' ', CNTR, ISTN, IYEAR, &
                   ISTAT1, LONG  , LAT, ELEV, ANGA, ANGB)
      CALL WEATHR (IDOY, ISTAT2, RDD, TMMN, TMMX, VP, WN, RAIN)
      IF (ISTAT1.NE.0.OR.ISTAT2.NE.0) WTRMES = .TRUE.
      WSTAT  = '444444'
      IF (ABS (ISTAT2).GE.111111) THEN
         WRITE (WSTAT,'(I6)') ABS (ISTAT2)
      ELSE IF (ISTAT2.EQ.0) THEN
         WSTAT = '111111'
      END IF

!-----Conversion of total daily radiation from kJ/m2/d to J/m2/d
      RDD = RDD*1000.

!----------------------------------------------------------------------*
!               Calculation of rates and output section                *
!----------------------------------------------------------------------*

!-----Call routine that handles the different models
      CALL MODELS (ITASK , IUNITD, IUNITO, IUNITL, &
                   FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                   OUTPUT, TERMNL, &
                   DOY   , IDOY  , YEAR  , IYEAR , STTIME,  &
                   TIME  , DELT  , &
                   LAT   , WSTAT , WTRTER, &
                   RDD   , TMMN  , TMMX  , VP    , WN, RAIN)

      IF (TERMNL.AND..NOT.OUTPUT.AND.PRDEL.GT.0.) THEN
!--------Call model routine again if TERMNL is switched on while
!        OUTPUT was off (this call is necessary to get output to file
!        when a finish condition was reached and output generation
!        was off)
         IF (ISET.EQ.0) THEN
            WRITE (*,'(13X,A,I5,A,F7.2)') &
              'Default set, Year:', IYEAR, ', Day:', DOY
         ELSE
            WRITE (*,'(13X,A,I3,A,I5,A,F7.2)') &
              'Rerun set:', ISET, ', Year:', IYEAR, ', Day:', DOY
         END IF
         OUTPUT = .TRUE.
         CALL OUTDAT (2, 0, 'TIME', TIME)
         CALL ChartNewGroup
         CALL ChartOutputRealScalar('TIME', TIME)
!        patch dvk
         CALL OUTDAT (2, 0, 'ISET', REAL (ISET))
         CALL MODELS (ITASK , IUNITD, IUNITO, IUNITL, &
                      FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                      OUTPUT, TERMNL, &
                      DOY   , IDOY  , YEAR  , IYEAR , STTIME, &
                      TIME  , DELT  , &
                      LAT   , WSTAT , WTRTER, &
                      RDD   , TMMN  , TMMX  , VP    , WN, RAIN)
      END IF

!----------------------------------------------------------------------*
!                             Time update                              *
!----------------------------------------------------------------------*

!-----Check for FINTIM, OUTPUT and observation days
      CALL TIMER2 (ITASK, STTIME, DELT, PRDEL, FINTIM, &
                   IYEAR, TIME  , DOY , IDOY , TERMNL, OUTPUT)
      YEAR = REAL (IYEAR)
      DO IOD=1,INOD,2
         IF (IYEAR.EQ.IOBSD(IOD).AND.IDOY.EQ.IOBSD(IOD+1)) &
             OUTPUT = .TRUE.
      END DO

      END DO

!======================================================================*
!                                                                      *
!                           Terminal section                           *
!                                                                      *
!======================================================================*

      ITASK = 4

      WRITE (*,'(A)') '   FSE 2.1: Terminate model'

      CALL ChartTerminalGroup

!-----Call routine that handles the different models
      CALL MODELS (ITASK , IUNITD, IUNITO, IUNITL, &
                   FILEIT, FILEI1, FILEI2, FILEI3, FILEI4, FILEI5, &
                   OUTPUT, TERMNL, &
                   DOY   , IDOY  , YEAR  , IYEAR , STTIME,  &
                   TIME  , DELT  , &
                   LAT   , WSTAT , WTRTER, &
                   RDD   , TMMN  , TMMX  , VP    , WN, RAIN)

!-----Generate output file dependent on option from timer file
      IF (IPFORM.GE.4) THEN
         IF (INPRS.EQ.0) THEN
            CALL OUTDAT (IPFORM, 0, 'Simulation results',0.)
         ELSE
!           Selection of output variables was in timer file
!           write tables according to output selection array PRSEL
            CALL OUTSEL (PRSEL,IMNPRS,INPRS,IPFORM,'Simulation results')
         END IF
      END IF

      IF (WTRTER) THEN
         WRITE (*,'(/,A,/,/,/)') &
           ' The run was terminated due to missing weather'
         WRITE (IUNITO,'(/,A,/,/,/)') &
           ' The run was terminated due to missing weather'
         IF (IUNITO.NE.IUNITL) WRITE (IUNITL,'(/,A,/,/,/)') &
           ' The run was terminated due to missing weather'
      END IF

!-----Delete temporary output file dependent on switch from timer file
      IF (DELTMP.EQ.'Y'.OR.DELTMP.EQ.'y') CALL OUTDAT (99, 0, ' ', 0.)

      END DO

      IF (INSETS.GT.0) CLOSE (IUNITR)

!-----If input files should be copied to the output file,
!     copy rerun file (if present) and timer file and if there, input
!     files 1-5

      IF (COPINF.EQ.'Y'.OR.COPINF.EQ.'y') THEN
         IF (INSETS.GT.0) CALL COPFL2 (IUNITR, FILEIR, IUNITO, .TRUE.)
         CALL COPFL2 (IUNITD, FILEIT, IUNITO, .TRUE.)
         IF (FILEI1.NE.' ') CALL COPFL2 (IUNITD, FILEI1, IUNITO, .TRUE.)
         IF (FILEI2.NE.' ') CALL COPFL2 (IUNITD, FILEI2, IUNITO, .TRUE.)
         IF (FILEI3.NE.' ') CALL COPFL2 (IUNITD, FILEI3, IUNITO, .TRUE.)
         IF (FILEI4.NE.' ') CALL COPFL2 (IUNITD, FILEI4, IUNITO, .TRUE.)
         IF (FILEI5.NE.' ') CALL COPFL2 (IUNITD, FILEI5, IUNITO, .TRUE.)
      END IF

!-----Delete all .TMP files that were created by the RD* routines
!     during simulation
      CALL RDDTMP (IUNITD)

!-----Write to screen which files contain what
      IL = LEN_TRIM (FILEON)
      WRITE (*,'(/,3A)') ' File: ',FILEON(1:IL), &
        ' contains simulation results'
      WRITE (*,'(2A)') ' File: WEATHER.LOG', &
        ' contains messages from the weather system'
      IL = LEN_TRIM (FILEOL)
      WRITE (*,'(3A,/)') ' File: ',FILEOL(1:IL), &
        ' contains messages from the rest of the model'

!-----Write message to screen and output file if warnings and/or errors
!     have occurred from the weather system, pause and wait for return
!     from user to make sure he has seen this message

      IF (WTRMES) THEN

         WRITE (*,'(/,A,/,A,/,A)') ' WARNING from FSE:', &
           ' There have been errors and/or warnings from', &
           ' the weather system, check file WEATHER.LOG'
         WRITE (IUNITO,'(A,/,A,/,A)') ' WARNING from FSE:', &
           ' There have been errors and/or warnings from', &
           ' the weather system, check file WEATHER.LOG'

         WRITE (*,'(A)') ' Press <Enter>'
         READ  (*,'(A)') DUMMY

      END IF

!-----Close output file and temporary file of OUTDAT
      CLOSE (IUNITO)
      CLOSE (IUNITO+1)

!-----Close log file (if used)
      IF (FILEOL.NE.FILEON) CLOSE (IUNITL)

!-----Close log file of weather system
      CLOSE (91)

!-----Write end_of_run values to file
      CALL OPWRITE (IUNITO)

      RETURN
      END
