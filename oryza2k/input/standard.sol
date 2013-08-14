**********************************************************************
* Soil data file for PADDY soil water balance model.                 *
* File name   : PADDY.DAT                                            *
*                                                   *
**********************************************************************

* Give code name of soil data file to match the water balance PADDY:
SCODE = 'PADDY'

*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*
WL0MX = 50.  ! Bund height (mm)
NL    = 9     ! Number of soil layers (maximum is 10) (-)
TKL   = 3*0.05, 3*0.05, 0.10, 0.20, 0.20 ! Thickness of each soil layer (m)
ZRTMS = 1.0   ! Maximum rooting depth in the soil (m)

*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*
*SWITPD = 0 ! Non puddled
SWITPD = 1  ! Puddled

* If PUDDLED, supply parameters for puddled soil
NLPUD = 3 ! Number of puddled soil layers, including the plow sole (-)
          ! (NLPUD cannot exceed the total number of soil layers NL)
* Saturated volumetric water content of ripened (previously puddled) 
* soil (m3 m-3), for each soil layer:
WCSTRP = 3*0.52, 3*0.55, 2*0.61, 0.64 

* Soil water tension of puddled soil layer at which cracks reach 
* break through the plow sole (pF):
PFCR = 6.0 

*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA 
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*
SWITGW = 0 ! Deep groundwater
*SWITGW = 2 ! Calculate groundwater
*SWITGW = 1 ! Groundwater data

* If DATA, supply table of groundwater table depth (cm; Y-value) 
* as function of calendar day (d; X value):
ZWTB =   1.,500.,
       366.,500.

* If CALCULATE, supply the following parameters:
ZWTBI = 500. ! Initial groundwater table depth (cm)
MINGW = 500. ! Minimum groundwater table depth (cm)
MAXGW = 500. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)

*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP can not be 1 (CALCULATE) for nonpuddled soil
*---------------------------------------------------------------*
SWITVP = -1 ! Fixed percolation rate
*SWITVP = 0 ! Percolation as function of the groundwater depth
*SWITVP = 1 ! Calculate percolation

* If SWITVP = -1, supply fixed percolation rate (mm d-1):
FIXPERC = 10.0  !   3.0

* If SWITVP = 0, supply table of percolation rate (mm d-1; Y-value) 
* as function of water table depth (cm; X value):
*PERTB =   0., 3., 
*         200., 3. 

*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*    function used
*---------------------------------------------------------------*
*SWITKH = 0 ! No data
*SWITKH = 2 ! Power
SWITKH  = 1 ! van Genuchten

*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*
*SWITPF = 0 ! Data
SWITPF  = 1 ! van Genuchten

*---------------------------------------------------------------*
*7.Soil physical properties, these parameters will be used when model
*runs under actual water or nitrogen condition, or even both. Otherwise
*these parameter will not be used.
CLAYX = 0.55,0.55,0.55,0.55,0.53,0.5,3*0.45      !soil clay contents, fraction
SANDX = 0.08,0.08,0.08,0.08,0.1,0.13,3*0.18      !soil sand contents, fraction
BD = 1.17894,1.17894,1.17911,1.42734,1.2695,1.31061,3*1.37562      !g/CM3
*Soil organic carbon and nitrogen content in kg C or N/ha
SOC = 31831.64999,31831.64999,20044.87,48529.56,8632.66799,2673.6648,280.62648,28.063,2.8063      
SON = 2829.48,2829.48,1886.576,4567.488,812.4864,251.639039,26.411903,2.64, 0.264      
SNH4X = 28.295,28.295,18.87,45.67,8.12,2.50,0.26,0.026,0.00264      
SNO3X = 5.66,5.66,3.774,9.134,1.624,0.5,0.052,0.0052,0.00052
*Fresh organic carbon and nitrogen input at soil layers
*FORGANC =200.0,150.0,100.0,50.0,5*0.0           !Fresh organic residue carbon input from previous crop at kg C/ha
*FORGANN =4.0,3.0,2.0,1.0,5*0.0                  !Fresh organic residue nitrogen input from previous crop at kg N/ha
*If the carbonhydrate and cellulous fractions are available in soil residue carbon input
* otherwise, the default values will be used
*FCarboh =
*FCellulo =
*RCarboh =
*RCellulo =

*---------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input
* according to setting of conductivity and water retention switch
*---------------------------------------------------------------*
* Saturated hydraulic conductivity, for each soil layer 
* (cm d-1) (always required!):
KST = 2*127.0, 0.3, 3*35.0, 2*103.0, 42.0

* Saturated volumetric water content, for each soil layer  
* (m3 m-3)(always required!):
WCST = 3*0.52, 3*0.55, 2*0.61, 0.64

* Van Genuchten parameters, for each soil layer
* (needed if SWITKH = 1 and/or SWITPF = 1):
VGA = 3*0.127, 3*0.047, 2*0.078, 0.032 ! a parameter (cm-1)
VGL = 3*-6.2, 3*-0.6, 2*-4.9, -11.1    ! l parameter (-)
VGN = 3*1.119, 3*1.095, 2*1.076, 1.073 ! n parameter (-)
VGR = 9*0.01                           ! residual water content (-)

* Power function parameters, for each soil layer (-)
* (needed if SWITKH = 2):
*PN = 3*-2.5, 3*-2.5, 2*-2.5, -2.5

* Volumetric water content at field capacity, for each soil layer 
* (m3 m-3)(needed if SWITPF = 0):
*WCFC = 3*0.48, 3*0.47, 2*0.52, 0.58

* Volumetric water content at wilting point, for each soil layer 
* (m3 m-3) (needed if SWITPF = 0):
*WCWP = 9*0.21

* Volumetric water content at air dryness, for each soil layer
* (m3 m-3) (needed if SWITPF = 0):
*WCAD = 9*0.01

*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 0.   ! Initial ponded water depth at start of simulation (mm)

* Initial volumetric water content at the start of simulation,
* for each soil layer (m3 m-3):
WCLI = 3*0.52, 3*0.47, 2*0.52, 0.58

* Initial ponded water depth and water contents may be reset:
* Ponded water depth: at minimum of WL0I and WL0MX
* Water contents in all soil layers: at saturation value
* For direct-seeded rice, this happens at sowing, for transplanted
* rice, this happens at transplanting 
* Re-initialize switch RIWCLI is YES or NO
*RIWCLI = 'NO'
RIWCLI = 'YES'

*---------------------------------------------------------------*
* 10. Initialization of soil thermal conditions
*---------------------------------------------------------------*
SATAV = 25.0       !Soil annual avaerage temperature of the first layers
SOILT = 27.0, 25.0, 23.0, 21.0, 19.0, 4*17.0     !Initial soil temperature in each layer
*                  !Have to provide above either one and two of abov paremeter, otherwise,
*                  !model start the calculation of soil temperature at 0 degree

* Table for interpolation of water content between soil layers for
* those layers for which no observations were made: first number is
* the soil layer for which interpolation needs to be done, the second
* is the number of the underlying soil layer, the third is the number 
* of the overlying soil layer. No interpolation is performed when all 
* three numbers are the same: 
WCLINT  = 1, 1, 1,
          2, 2, 2,
          3, 3, 3,
          4, 4, 4,
          5, 5, 5,
          6, 6, 6,
          7, 7, 7,
          8, 8, 8,
          9, 9, 9
*---------------------------------------------------------------*
* 11. Observations/measurements
*    Switches to force observed water content in water balance
*---------------------------------------------------------------*

* WCL1_OBS, WCL2_OBS,...WCL10_OBS: Observed soil water contents 
* in layer 1, 2, ..., 10. Format: year, day number, water content
* Not obligatory to give data


