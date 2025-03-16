MODULE LINE_LENGTHS
   IMPLICIT NONE
   INTEGER, PARAMETER :: maxlen = 120           ! Line length
   ! Ensure maxlenf and maxlensurf are <= maxlen
   INTEGER, PARAMETER :: maxlenf = 70           ! File line length
   INTEGER, PARAMETER :: maxlensurf = 30          ! Surface name line length
   INTEGER, PARAMETER :: maxleng = 20           ! Special string length
   INTEGER, PARAMETER :: maxlenFV = 180    ! Special format line length
   INTEGER, PARAMETER :: maxFV = 15        ! Special format string number
   INTEGER, PARAMETER :: maxlenFW = 40     ! Special format line length
   INTEGER, PARAMETER :: maxFW = 5                ! Special format string number
END MODULE LINE_LENGTHS

!------------------------------------------------------------------
! Generating the attenuation value (Np/m) from the stored data.

REAL(KIND=8) FUNCTION AttenF(AttenIn, AttenFlag, omega)
   USE SPECIAL_CONSTANTS
   USE Lines
   IMPLICIT NONE
   REAL(KIND=8), INTENT(IN) :: Attenin, omega
   INTEGER, INTENT(IN) :: AttenFlag
   IF (AttenFlag .eq. 0) THEN
      AttenF = AttenIn*dBtoNp
   ELSE IF (AttenFlag .eq. 1) THEN
      AttenF = AttenIn*dBtoNp*omega/twopi
   ELSE
      CALL ERROR(120); RETURN
   END IF
END FUNCTION AttenF

!------------------------------------------------------------------
! Currently, forcing phase speed to be non-zero.

REAL(KIND=8) FUNCTION SpeedF(SpeedIn)
   IMPLICIT NONE
   REAL(KIND=8), INTENT(IN) :: Speedin
   If (SpeedIn .eq. 0D0) Then
      SpeedF = 1D-50
   Else
      SpeedF = SpeedIn
   End If
END FUNCTION SpeedF

!------------------------------------------------------------------
! Generating a modulus from a phase speed, attenuation, and density.

FUNCTION ModulusF(Nthe, Medium, Ispeed, MediumIndex)

   USE Reference
   USE Theoretical
   USE Properties
   USE Lines
   IMPLICIT NONE

   COMPLEX(KIND=8) :: ModulusF
   INTEGER, INTENT(IN) :: Nthe, Medium, Ispeed, MediumIndex

   INTEGER :: AttenFlag
   REAL(KIND=8) :: Speed, Atten
   REAL(KIND=8) :: DensityIn, SpeedIn, AttenIn, omega

   Estatus = .true.

   DensityIn = density(Nthe, Medium)
   IF (Ispeed .eq. 1) THEN                ! Longitudinal components
      SpeedIn = cspeed(Nthe, Medium)
      AttenIn = catten(Nthe, Medium)
      AttenFlag = cattenf(Nthe, Medium)
   ELSE IF (Ispeed .eq. 2) THEN        ! Shear components
      SpeedIn = bspeed(Nthe, Medium)
      AttenIn = batten(Nthe, Medium)
      AttenFlag = battenf(Nthe, Medium)
   ELSE
      CALL ERROR(152); RETURN
   END IF

   omega = w0
   Speed = SpeedF(SpeedIn)
   Atten = AttenF(AttenIn, AttenFlag, omega)

   ModulusF = DensityIn/(Dcmplx(1D0/Speed, -Atten/omega))**2

   IF (Dreal(ModulusF) .lt. 0D0) THEN
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') &
         '   Theoretical set Nthe:  ', Nthe
      If (MediumIndex .eq. 0) Then
         WRITE (6, '(1X,A)', ADVANCE='YES') &
            '   Surface:  Unknown (index not passed)'
      Else
         WRITE (6, '(1X,A)', ADVANCE='YES') &
            '   Surface:  ', SurfaceList(MediumIndex)
      End If
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') 'Medium: ', Medium
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') 'Ispeed: ', Ispeed
      WRITE (6, '(1X,A,"(",1PE15.8,",",1PE15.8,")")', ADVANCE='YES') &
         'Modulus: ', ModulusF
      WRITE (6, '(1X,A,F)', ADVANCE='YES') 'Phase speed: ', SpeedIn
      WRITE (6, '(1X,A,F)', ADVANCE='YES') 'Density: ', DensityIn
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') &
         'Attenuation (dB/-): ', AttenIn
      WRITE (6, '(1X,A,F)', ADVANCE='YES') &
         'Phase speed at this freq requires atten less than:', &
         omega*20D0/DLOG(10D0)/SpeedIn
      CALL ERROR(117); RETURN
   END IF

END FUNCTION ModulusF

MODULE GLOBAL_PARAMETERS
   USE LINE_LENGTHS
   IMPLICIT NONE

   !  MaxParts:    Maximum number of parts
   !  LimitPanel:  Maximum number of facets (before reflection)
   !  LimitVertex: Maximum number of vertices (before reflection)
   !  LimitSum:    Maximum number of facets 'seen' (i.e. those facets
   !               whose scattering contribution must be counted)
   !
   !  Narray:      Empirical data array size (see function REFTRANS also)
   !  Nempmax:     Maximum number of Empirical data sets
   !  Nthemax:     Maximum number of Theoretical data sets
   !  LayerLimit:  Maximum Layer index value see mREFLAYER
   !
   !  MapLimitX :  Map 'X half-width' (map is 2*MapLimitX pix wide)
   !  MapLimitY :  Map 'Y half-width' (map is 2*MapLimitY pix high)
   !
   !  The following five parameters control the depth and nature
   !  of the map stack (hidden surface removal and surface
   !  transmission).  Increasing LimitPrev allows decreasing
   !  LimitExtend. If LimitPrev were set to LimitExtendPrev then
   !  LimitExtend could be reduced to a minimum but too much memory
   !  would be used.  A large LimitExtendPrev simply means that
   !  occasionally there are many facets that must be stored in a
   !  particular pixel (and a low LimitExtend means that this is a rare
   !  occurrence).  LimitExtend2Prev and LimitExtend2 are similar,
   !  providing even more depth for a small number of map locations.
   !
   !  Maximum Stack Depth = LimitPrev + LimitExtendPrev +
   !                                            LimitExtendPrev2
   !
   !  LimitPrev :  Limit on previous Panels recorded for MAPprev;
   !               keep >= 1
   !  1st stack ----
   !    LimitExtendPrev :  Limit on previous Panels recorded for
   !                       ExtendPrev
   !    LimitExtend :  Limit on number of MAP locations which can be
   !                   extended
   !  2nd stack ----
   !    LimitExtend2Prev :  Limit on previous Panels recorded for
   !                        Extend2Prev
   !    LimitExtend2 :  Limit on number of 1st stack locations which
   !                    can be extended
   !
   !  LimT:        Limit on number of transmitting surfaces accounted
   !               for over an arbitrary panel
   !  LimTpanel:   Limit on number of transmitting panels (not counting
   !               symmetry). If this limit is to exceed (65536/2)-1,
   !               Tstack2 (and perhaps Tindex) will probably need to
   !               be set to an integer size larger than INTEGER (KIND=2)
   !               to avoid integer overflows
   !  LimPanTrans: Limit on panels that can be covered by transmitting
   !               surfaces
   !  LimAdjoin:   Limit on number of parts set to adjoin another
   !               particular part. Applies to transmitting parts only
   !               and is used to minimize double counting of
   !               transmission due to finite pixel resolution.
   !
   !  Map memory calculation (adjusting stack parameters marked !*!
   !  below):
   !
   !  membytes = MapLimitX*2 * MapLimitY*2 *2 * LimitPrev * 4 ! MAPprev
   !                 + LimitExtendPrev * LimitExtend * 2 * 4   ! ExtendPrev
   !          + LimitExtend                             * 4 ! Extend1to2
   !                 + LimitExtend2Prev * LimitExtend2 * 2 * 4 ! Extend2Prev

   INTEGER, PARAMETER :: MaxParts = 800               ! SUB3: 700
   INTEGER, PARAMETER :: LimitPanel = 200000          ! SUB3: 160000
   INTEGER, PARAMETER :: LimitVertex = 100000         ! SUB3: 90000
   INTEGER, PARAMETER :: LimitSum = 200000            ! SUB3: 200000
   INTEGER, PARAMETER :: MapLimitX = 500
   INTEGER, PARAMETER :: MapLimitY = 150

   !Fin   ! Sub
   INTEGER, PARAMETER :: LimitPrev = 13             !13T !*! 7    !
   INTEGER, PARAMETER :: LimitExtendPrev = 19 !19T !*! 25   !Trade off here
   INTEGER, PARAMETER :: LimitExtend = 21000       !*! 21000!
   INTEGER, PARAMETER :: LimitExtend2Prev = 800    !*! 800  !And here also
   INTEGER, PARAMETER :: LimitExtend2 = 600        !*! 300  !

   INTEGER, PARAMETER :: LimT = 13             !13T   ! 8  ! Tstack limit

   ! Total T panels
   INTEGER, PARAMETER :: LimTpanel = 130000          ! SUB3: 100000
   ! Max panels (T/nonT) covered by T
   INTEGER, PARAMETER :: LimPanTrans = 165000        ! SUB3: 135000

   INTEGER, PARAMETER :: LimAng = 1500

   INTEGER, PARAMETER :: LimDF = 400
   INTEGER, PARAMETER :: LimDFpwl = 2000

   ! To conserve space for complex FFT calcs I did not make VALUE
   ! complex because LimAng needs to be kept large for most non-FFT
   ! calculations.
   ! New arrays introduced: VALUER for the real part, VALUEI for the
   ! imaginary part, and LimAngFFT becomes the reduced number of
   ! storable angles.

   INTEGER, PARAMETER :: LimFFT = 65536 !8192 !4096 !2048
   INTEGER, PARAMETER :: LimAngFFT = 10

   INTEGER, PARAMETER :: Narray = 30
   INTEGER, PARAMETER :: Nempmax = 15

   ! Useful for old runs with largely numerical data
   ! INTEGER, PARAMETER :: Narray = 100
   ! INTEGER, PARAMETER :: Nempmax = 40

   INTEGER, PARAMETER :: LayerLimit = 7
   INTEGER, PARAMETER :: Nthemax = 130

   INTEGER, PARAMETER :: Ntotmax = Nempmax + Nthemax
   INTEGER, PARAMETER :: LimAdjoin = 8

   INTEGER, PARAMETER :: LimMultScat = 15
   INTEGER, PARAMETER :: LimMult2Scat = 4
   INTEGER, PARAMETER :: NlimitFFT = LimFFT*2

   INTEGER, PARAMETER :: Int1Limit = 127                ! Integer*1 limit
   INTEGER, PARAMETER :: Int2Limit = 32767        ! Integer*2 limit
   INTEGER, PARAMETER :: Int4Limit = 2147483647        ! Integer*4 limit

   ! Refer to variable declarations
   INTEGER, PARAMETER :: MePLimit = Int4Limit
   INTEGER, PARAMETER :: MeP2Limit = Int4Limit
   INTEGER, PARAMETER :: MintTstack1 = Int2Limit
   INTEGER, PARAMETER :: MintTstack2 = Int4Limit
   INTEGER, PARAMETER :: MintTindex = Int4Limit

   INTEGER, PARAMETER :: Nreflect = 200        ! Modulo base for MEDIUM
   ! (a simple number)
   ! (must be > Ntotmax?)
   INTEGER, PARAMETER :: BinLimit = 30000 ! Limit on no. of 'time' Bins
   INTEGER, PARAMETER :: LimitFilter = 300 ! Limit on no. of Filt coeffs

   REAL(KIND=8), PARAMETER  :: &
      RTmax = 1.0000001D0, &
      RTsevere = 1.14D0, & ! 1.14 is ~1.1dB above 100%
      TmedDiff = 0.01D0, & ! Default units for ambient medium
      ! difference test. With 0.01, the
      ! TMedIgnore value becomes the
      ! "percent-difference allowable"
      TlimDiff = 0.01D0             ! Default units for T > 1 test.
   ! With 0.01, the TlimIgnore value
   ! becomes the "percent-difference
   ! above unity allowable"

   INTEGER(KIND=1) :: TmedErrorDefault = 25
   ! Number of units for default error
   ! i.e. 25% error if unit is 0.01
   INTEGER(KIND=1) :: TlimErrorDefault = 60
   ! Number of units for default error
   ! i.e. 60 => 60% error if unit is 0.01

   REAL(KIND=8), PARAMETER :: &
      ! eps3 = 1D-16,  &       ! Pre 6/8/2002; too small
      eps3 = 1D-10, &       ! Exp() limit for facet integration;
      ! Arguments below this level use a
      ! linearized approximation for accuracy
      eps4 = 1D-200, &       ! 'Complex underflow' limit
      eps5 = 1D-10

   REAL(KIND=8), PARAMETER :: &
      BiStaticLimit = 45D0   ! Warning for bistatic hidden surface
   ! if BistaticAngle > this value (in deg).

   REAL(KIND=8), PARAMETER :: MIXglobalDefault = 0.2D0
   ! Refer to MIXglobal in code.
   ! This value was chosen to give reasonable
   ! results at "lowish" frequencies.
   ! No weighting (i.e. full blending) by default would use:
   ! REAL (KIND=8), PARAMETER :: MIXglobalDefault = 200D0

END MODULE GLOBAL_PARAMETERS

MODULE Lines
   USE LINE_LENGTHS
   IMPLICIT NONE
   ! The command line 'cline' is kept separate from the other lines
   ! (e.g. 'line', which is used for file reads "between" command line
   ! input.
   CHARACTER(LEN=maxlen) :: cline
   CHARACTER(LEN=maxlen) :: line, line2
   CHARACTER(LEN=maxlen) :: word
   CHARACTER(LEN=maxlen) :: mprompt
   CHARACTER(LEN=maxlen) :: lprompt
END MODULE Lines

MODULE Theoretical
   USE GLOBAL_PARAMETERS
   IMPLICIT NONE
   INTEGER :: Nmed(Nthemax)
   REAL(KIND=8) :: thick(Nthemax, 0:LayerLimit), &
                   density(Nthemax, 0:LayerLimit), &
                   cspeed(Nthemax, 0:LayerLimit), &
                   catten(Nthemax, 0:LayerLimit), &
                   bspeed(Nthemax, 0:LayerLimit), &
                   batten(Nthemax, 0:LayerLimit)
   INTEGER :: cattenf(Nthemax, 0:LayerLimit), &
              battenf(Nthemax, 0:LayerLimit)
END MODULE Theoretical

MODULE ERROR_DATA
   IMPLICIT NONE
   LOGICAL :: Estatus         ! Estatus .true. means no error
END MODULE ERROR_DATA

MODULE SPECIAL_CONSTANTS
   IMPLICIT NONE
   REAL(KIND=8), PARAMETER :: &
      pi = 3.141592653589793238462643D0, &
      halfpi = pi/2D0, &
      quarterpi = pi/4D0, &
      twopi = 2D0*pi, &
      eps = 1D-12, &
      eps2 = 1D-18, &
      degrees = 180D0/pi, &
      ! * degrees converts radians to degrees
      ! / degrees converts degrees to radians
      radians = pi/180D0, &
      ! * radians converts degrees to radians
      vlog10 = 2.3025850929940456840179915D0, &
      dBtoNp = vlog10/20D0
   ! * dBtoNp converts dB/m to p atten in Np/m
   ! / dBtoNp converts p atten in Np/m to dB/m
   COMPLEX(KIND=8), PARAMETER :: &
      imagI = (0D0, 1D0)
END MODULE SPECIAL_CONSTANTS

!------------------------------------------------------------------
! Complex amplitude reflection and transmission coefficients of a
! plane acoustic wave incident on a stack of solid or liquid layers
! at arbitrary angle of incidence.  The complex output carries the
! phase of each component.  The incident wave is in medium 0.
!
! Refer also to program LP.FOR
!
! Uses the procedure of Levesque and Piche
! (J. Acoust. Soc. Am. 92(1), July 1992).
! See also the routine REFLAYER which uses the procedure of Cervenka
! and Challande (1991).
!
! Exp(+i*omega*t) convention.
!
! Displacement potential formalism. With:
!
!   rp: Pressure potential amplitude reflection coefficient
!   tp: Pressure potential amplitude transmission coefficient
!   rs: Shear potential amplitude reflection coefficient
!   ts: Shear potential amplitude transmission coefficient
!
!   RpE: Pressure energy reflectance
!   TpE: Pressure energy transmittance
!   RsE: Shear energy reflectance
!   TsE: Shear energy transmittance
!
! Coefficients involving a vector potential divided by a scalar
! potential (such as ts) have sign that is dependent on the
! coordinate system. Here, the sign of the x and z terms in the
! complex exponent of the potential function have the same signs.
! Brekhovskikh and Ainslee (1993), for example, have opposite
! signs, giving rise to tp with an opposite sign.
!
! Take care in interpreting the energy values if the upper or lower
! media are attenuating.
!
! NB: Attenuation values are in dB per unit length if cattenf of
! battenf is set 0, or dB per unit length per unit Hz if it is set 1.

SUBROUTINE RESPONSE(Input, cosinc, omega, Nthe, MediumIndex, &
                    rp, tp, rs, ts, RpE, TpE, RsE, TsE)
   USE SPECIAL_CONSTANTS
   USE ERROR_DATA
   USE Theoretical
   USE Lines
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: Input, Nthe, MediumIndex
   REAL(KIND=8), INTENT(IN) :: cosinc, omega
   COMPLEX(KIND=8), INTENT(OUT) :: rp, tp, rs, ts
   REAL(KIND=8), INTENT(OUT) :: RpE, RsE, TpE, TsE

   INTEGER :: medium, m, n, ii, last
   REAL(KIND=8) :: theta0, fnorm, amax, rhos22, thetamin, thetamax
   REAL(KIND=8) :: dd, rho, rho0, qp0, qs0, qpn, qsn

   COMPLEX(KIND=8) :: g(2), a0(4), B(4, 4), a(4, 2), atemp(4, 2)
   COMPLEX(KIND=8) :: j(3), c0(3, 6), D(6, 6), c(6), ctemp(6)
   COMPLEX(KIND=8) :: i, s, k, h, cpsq, cp0sq, xi, xi0, &
                      gamma, eta, nu, nu1, nu2, rhos2, ch, sh, ck, sk, hk, &
                      nu1t, nu12, chck, chck1, shsk, cssq, rh, rk, twomuxi, &
                      twomuxi2, xik, xih, gammak, gammahk, xigammahk, &
                      gammah, cs0sq, rhos2h, rhos2k, gamma2hk, cp0, cs0, &
                      LongM, LongM0, mu, mu0, shrh, skrk, rhrk, &
                      nunu1, xi2hk

   Estatus = .true.

   last = Nmed(Nthe)

   i = (0D0, 1D0)

   ! theta should not be exactly zero with this algorithm
   thetamin = 1D-5
   thetamax = halfpi*0.999999D0
   theta0 = ACOS(cosinc)
   If (theta0 .lt. thetamin) theta0 = thetamin
   ! Angles close to 90 degrees can cause problems occasionally
   If (theta0 .gt. thetamax) theta0 = thetamax

   ! s is the Laplace transform variable corresponding to t
   !   For frequency omega, s=i*omega

   s = i*omega

   ! xi is the Laplace transform variable corresponding to x
   !   For plane waves, xi = xi0 (everywhere) where:
   !      xi0 = s*Sin(theta0)/c0,     with
   !      c0 = cp (P input) or cs (SV input) for medium 0

   ! First medium properties

   ! If I let bspeed(Nthe,0) be exactly zero and assume this implies a
   ! purely imaginary modulus mu then the Rp in Figure 4 (0.2mm) of
   ! Levesque and Piche falls off at large angles of incidence when I
   ! calculate it here. Currently I will assume bspeed is small but
   ! non-zero.  With bspeed > 0 the attenuation is essentially
   ! unimportant (ie large attenuations do not seem to be needed
   ! for fluids) and the results match well (but not perfectly?).
   ! If bspeed is to be left at zero then attenuation must be
   ! included (which can be converted to the equivalent imaginary
   ! modulus mu):
   !          If (bspeed(Nthe,0).eq.0D0) Then
   !            mu0 = i*rho0*(omega/(batten(Nthe,0)*dBtoNp))**2/2D0
   !          Else ...
   ! L and P's table of physical properties is unclear/ambiguous to me.

   rho0 = density(Nthe, 0)
   mu0 = ModulusF(Nthe, 0, 2, MediumIndex)
   IF (.not. Estatus) RETURN
   LongM0 = ModulusF(Nthe, 0, 1, MediumIndex)
   IF (.not. Estatus) RETURN
   ! Write (6,*) 'mu 0:', mu0
   ! Write (6,*) 'Longitudinal Modulus 0:', LongM0
   CALL PhysicalMod(LongM0, Mu0, 0, MediumIndex)
   IF (.not. Estatus) RETURN

   cp0sq = LongM0/rho0
   cs0sq = mu0/rho0

   If (Input .eq. 1) Then
      cp0 = Sqrt(cp0sq)
      xi0 = s*Sin(theta0)/cp0
      ! Above is apparently what L & P use. (NB p457 top rhs)
      ! The code:
      !           xi0 = s*Sin(theta0)/cspeed(Nthe,0)
      ! is similar to Cervenka et al (using K real). However it cannot
      ! be correct because if two identical attenuating half-spaces
      ! are used with no intervening layer the reflection is only zero
      ! (as it should be) if the complex cp0 is used.  Remember also
      ! that |Rp| > 0 is not necessarily physically unrealistic if
      ! half-space media are attenuating.  This would seem to make
      ! Cervenka's applied example incorrect since both algorithms
      ! give the results of the example only if K (xi0) is real.
   Else If (Input .eq. 2) Then
      If (bspeed(Nthe, 0) .lt. 1D-3) THEN
         CALL ERROR(119); RETURN
      END IF
      cs0 = Sqrt(cs0sq)
      xi0 = s*Sin(theta0)/cs0
   Else
      CALL ERROR(271); RETURN
   End If

   xi = xi0

   ! Write (6,*) 'xi:', xi
   ! Write (6,*) 'xi2:', xi**2
   ! Write (6,*) 's:', s
   ! Write (6,*) 's2:', s**2

   ! Last medium (n+1) properties

   rho = density(Nthe, last)
   mu = ModulusF(Nthe, last, 2, MediumIndex)
   IF (.not. Estatus) RETURN
   LongM = ModulusF(Nthe, last, 1, MediumIndex)
   IF (.not. Estatus) RETURN
   CALL PhysicalMod(LongM, Mu, last, MediumIndex)
   IF (.not. Estatus) RETURN
   cpsq = LongM/rho
   cssq = mu/rho

   ! Equation (8)
   h = Sqrt(s**2/cpsq - xi**2)
   CALL Physical(h)
   k = Sqrt(s**2/cssq - xi**2)
   CALL Physical(k)

   qpn = Dreal(i*rho*Conjg(h))        ! Equation (42), Energy coefficients
   qsn = Dreal(i*rho*Conjg(k))        ! Equation (42), Energy coefficients

   rhos2 = rho*s**2
   gamma = rhos2 - 2D0*mu*xi**2        ! Equation (27) ff
   twomuxi = 2D0*mu*xi
   hk = h*k

   ! Set up a[last] [equation(63)]
   a(1, 1) = k
   a(1, 2) = xi
   a(2, 1) = -xi
   a(2, 2) = h
   a(3, 1) = twomuxi*k
   a(3, 2) = -gamma
   a(4, 1) = -gamma
   a(4, 2) = -twomuxi*h

   ! Set up c[last] [equation(63)]
   c(1) = hk + xi**2
   c(2) = -rhos2*k
   c(3) = xi*gamma - twomuxi*hk
   c(4) = c(3)
   c(5) = rhos2*h
   c(6) = -twomuxi**2*hk - gamma**2

   ! Write (6,*) 'i, j, an1(i,j):'
   ! m = 1
   ! Do While (m.le.4)
   !   Write (6,*) m,1,a(m,1)
   !   Write (6,*) m,2,a(m,2)
   !   m = m + 1
   ! End Do

   ! Write (6,*) 'cn1(1):', c(1)
   ! Write (6,*) 'cn1(2):', c(2)
   ! Write (6,*) 'cn1(3):', c(3)
   ! Write (6,*) 'cn1(4):', c(4)
   ! Write (6,*) 'cn1(5):', c(5)
   ! Write (6,*) 'cn1(6):', c(6)

   medium = last - 1
   Do While (medium .ge. 1) ! Begin *Medium* loop

      rho = density(Nthe, medium)
      dd = thick(Nthe, medium)
      mu = ModulusF(Nthe, medium, 2, MediumIndex)
      IF (.not. Estatus) RETURN
      LongM = ModulusF(Nthe, medium, 1, MediumIndex)
      IF (.not. Estatus) RETURN
      CALL PhysicalMod(LongM, Mu, medium, MediumIndex)
      IF (.not. Estatus) RETURN

      ! cp is the long phase speed: cp^2 = (lambda + 2*mu)/rho
      cpsq = LongM/rho
      ! cs is the shear phase speed: cs^2 = mu/rho
      cssq = mu/rho

      ! Equation (8)
      h = Sqrt(s**2/cpsq - xi**2)
      CALL Physical(h)
      k = Sqrt(s**2/cssq - xi**2)
      CALL Physical(k)

      ! Defined quantities in APPENDIX (and some from main text):

      rhos2 = rho*s**2
      gamma = rhos2 - 2D0*mu*xi**2        ! Equation (27)

      nu = 2D0*mu*xi**2/rhos2
      eta = rhos2/xi

      ch = (Exp(-h*dd) + Exp(h*dd))/2D0        ! Cosh(-h*dd)
      ck = (Exp(-k*dd) + Exp(k*dd))/2D0        ! Cosh(-k*dd)

      sh = xi*(Exp(-h*dd) - Exp(h*dd))/(2D0*h)        ! (xi/h)Sinh(-h*dd)
      sk = xi*(Exp(-k*dd) - Exp(k*dd))/(2D0*k)        ! (xi/k)Sinh(-k*dd)

      rh = (h/xi)**2
      rk = (k/xi)**2

      nu2 = nu**2                        ! nu^2
      nu1 = 1D0 - nu                        ! 1 - nu
      nunu1 = nu*nu1                        ! nu(1-nu)
      nu1t = 1D0 - 2D0*nu                ! 1-2nu
      nu12 = nu1**2                        ! (1-nu)^2
      chck = ch*ck                        ! c_h c_k
      chck1 = 1D0 - chck                ! 1 - c_h c_k
      shsk = sh*sk                        ! s_h s_k
      shrh = sh*rh                        ! s_h r_h
      skrk = sk*rk                        ! s_k r_k
      rhrk = rh*rk                        ! r_h r_k

      fnorm = Abs(Exp(-h*dd))

      ! fnorm seems to be insufficient? I use the following to minimize
      ! large numbers only, otherwise use 1D0.  But note that amax is
      ! divided into array values so can conceivably cause overflow
      ! itself if 1D0 is used. Not part of L & P's procedure (but fully
      ! consistent with it).
      amax = 1D0

      ! Write (6,*) medium, ' rhos2:', rhos2
      ! Write (6,*) medium, ' h:', h
      ! Write (6,*) medium, ' k:', k
      ! Write (6,*) medium, ' eta:', eta
      ! Write (6,*) medium, ' nu2:', nu2
      ! Write (6,*) medium, ' nu:', nu
      ! Write (6,*) medium, ' nunu1:', nunu1
      ! Write (6,*) medium, ' nu1:', nu1
      ! Write (6,*) medium, ' nu12:', nu12
      ! Write (6,*) medium, ' nu1t:', nu1t
      ! Write (6,*) medium, ' fnorm:', fnorm
      ! Write (6,*) medium, ' shrh:', shrh
      ! Write (6,*) medium, ' ch:', ch
      ! Write (6,*) medium, ' sh:', sh
      ! Write (6,*) medium, ' rh:', rh
      ! Write (6,*) medium, ' ck:', ck
      ! Write (6,*) medium, ' sk:', sk
      ! Write (6,*) medium, ' skrk:', skrk
      ! Write (6,*) medium, ' rk:', rk
      ! Write (6,*) medium, ' rhrk:', rhrk
      ! Write (6,*) medium, ' chck:', chck
      ! Write (6,*) medium, ' chck1:', chck1
      ! Write (6,*) medium, ' shsk:', shsk

      ! Set up B[med]

      B(1, 1) = nu1*ck + nu*ch
      B(1, 2) = nu*skrk - nu1*sh
      B(1, 3) = (ck - ch)/eta
      B(1, 4) = (sh + skrk)/eta
      B(2, 1) = nu1*sk - nu*shrh
      B(2, 2) = nu1*ch + nu*ck
      B(2, 3) = (sk + shrh)/eta
      B(2, 4) = B(1, 3)
      B(3, 1) = eta*nunu1*(ck - ch)
      B(3, 2) = eta*(nu12*sh + nu2*skrk)
      B(3, 3) = B(2, 2)
      B(3, 4) = B(1, 2)
      B(4, 1) = eta*(nu12*sk + nu2*shrh)
      B(4, 2) = B(3, 1)
      B(4, 3) = B(2, 1)
      B(4, 4) = B(1, 1)

      ! Write (6,*) '  i, j, B(i,j):'
      ! m = 1
      ! Do While (m.le.4)
      !   ii = 1
      !   Do While (ii.le.4)
      !     Write (6,*) m,ii,B(m,ii)
      !     ii = ii + 1
      !   End Do
      !   m = m + 1
      ! End Do

      ! Multiply B[med] by B[med+1]*B[med+2]*...*B[last-1]*a[last]
      ! using atemp[]
      m = 1
      Do While (m .le. 4)
         n = 1
         Do While (n .le. 2)
            atemp(m, n) = 0D0
            ii = 1
            Do While (ii .le. 4)
               atemp(m, n) = atemp(m, n) + B(m, ii)*a(ii, n)
               ii = ii + 1
            End Do
            atemp(m, n) = fnorm*atemp(m, n)
            ! For large numbers of liquids with b=0 (1D-50 here) I
            ! find that atemp() and ctemp() can increase to cause
            ! overflow despite the above normalization.  Here, I
            ! evaluate another factor, which must be applied to both
            ! a() and c().
            amax = max(abs(atemp(m, n)), amax)
            n = n + 1
         End Do
         m = m + 1
      End Do

      ! Return atemp[] to a[]
      m = 1
      Do While (m .le. 4)
         n = 1
         Do While (n .le. 2)
            ! Normalize again; see above
            a(m, n) = atemp(m, n)/amax
            n = n + 1
         End Do
         m = m + 1
      End Do

      ! Set up D[med]

      D(1, 1) = chck + 2D0*nunu1*chck1 + (nu12 + nu2*rhrk)*shsk
      D(1, 2) = (ch*sk + ck*shrh)/eta
      D(1, 3) = (nu1t*chck1 - (nu1 - nu*rhrk)*shsk)/eta
      D(1, 4) = D(1, 3)
      D(1, 5) = -(ch*skrk + ck*sh)/eta
      D(1, 6) = (2D0*chck1 - shsk*(1D0 + rhrk))/(eta**2)
      D(2, 1) = eta*(nu12*ck*sh + nu2*ch*skrk)
      D(2, 2) = chck
      D(2, 3) = nu*ch*skrk - nu1*ck*sh
      D(2, 4) = D(2, 3)
      D(2, 5) = -shsk*rk
      D(2, 6) = D(1, 5)
      D(3, 1) = eta*(nunu1*nu1t*chck1 + (nu1*nu12 - nu*nu2*rhrk)*shsk)
      D(3, 2) = nu1*ch*sk - nu*ck*shrh
      D(3, 4) = D(2, 2) - D(1, 1)
      D(3, 3) = D(3, 4) + 1D0
      D(3, 5) = D(2, 4)
      D(3, 6) = D(1, 4)
      D(4, 1) = D(3, 1)
      D(4, 2) = D(3, 2)
      D(4, 3) = D(3, 4)
      D(4, 4) = D(3, 3)
      D(4, 5) = D(2, 3)
      D(4, 6) = D(1, 3)
      D(5, 1) = -eta*(nu2*ck*shrh + nu12*ch*sk)
      D(5, 2) = -shsk*rh
      D(5, 3) = D(4, 2)
      D(5, 4) = D(3, 2)
      D(5, 5) = D(2, 2)
      D(5, 6) = D(1, 2)
      D(6, 1) = eta**2*(2D0*nu2*nu12*chck1 - (nu2**2*rhrk + nu12**2)*shsk)
      D(6, 2) = D(5, 1)
      D(6, 3) = D(4, 1)
      D(6, 4) = D(3, 1)
      D(6, 5) = D(2, 1)
      D(6, 6) = D(1, 1)

      ! Write (6,*) 'i, j, D(i,j):'
      ! m = 1
      ! Do While (m.le.6)
      !   ii = 1
      !   Do While (ii.le.6)
      !     Write (6,*) m,ii,D(m,ii)
      !     ii = ii + 1
      !   End Do
      !   m = m + 1
      ! End Do

      ! Multiply D[med] by D[med+1]*D[med+2]*...*D[last-1]*c[last]
      ! using ctemp[]
      m = 1
      Do While (m .le. 6)
         ctemp(m) = 0D0
         ii = 1
         Do While (ii .le. 6)
            ctemp(m) = ctemp(m) + D(m, ii)*c(ii)
            ii = ii + 1
         End Do
         ctemp(m) = fnorm*ctemp(m)
         m = m + 1
      End Do

      ! Return ctemp[] to c[]
      m = 1
      Do While (m .le. 6)
         ! Normalize c() again, as I did a(); see above
         c(m) = ctemp(m)/amax
         m = m + 1
      End Do

      medium = medium - 1

   End Do ! End *Medium* loop

   ! Do medium 0 matrix products

   mu = mu0
   rho = rho0
   cpsq = cp0sq
   cssq = cs0sq

   ! Equation (8)
   h = Sqrt(s**2/cpsq - xi**2)
   CALL Physical(h)
   k = Sqrt(s**2/cssq - xi**2)
   CALL Physical(k)

   qp0 = Dreal(i*rho*Conjg(h))        ! Equation (42), Energy coefficients
   qs0 = Dreal(i*rho*Conjg(k))        ! Equation (42), Energy coefficients

   rhos2 = rho*s**2
   gamma = rhos2 - 2D0*mu*xi**2        ! Equation (27)

   rhos22 = 2D0*rhos2
   rhos2h = rhos2/h
   rhos2k = rhos2/k
   twomuxi = 2D0*mu*xi
   twomuxi2 = twomuxi**2
   hk = h*k
   xih = xi/h
   xik = xi/k
   xi2hk = xih*xik
   gammak = gamma/k
   gammahk = gammak/h
   xigammahk = xi*gammahk
   gamma2hk = gamma*gammahk

   ! Write (6,*) 'qp0:', qp0
   ! Write (6,*) 'qs0:', qs0
   ! Write (6,*) 'h 0:', h
   ! Write (6,*) 'rhos2 0:', rhos2
   ! Write (6,*) 'mu 0:', mu
   ! Write (6,*) 'k 0:', k
   ! Write (6,*) 'gamma 0:', gamma
   ! Write (6,*) 'twomuxi 0:', twomuxi
   ! Write (6,*) 'hk 0:', hk
   ! Write (6,*) 'rhos2h 0:', rhos2h
   ! Write (6,*) 'rhos2k 0:', rhos2k
   ! Write (6,*) 'xik 0:', xik
   ! Write (6,*) 'gammak 0:', gammak
   ! Write (6,*) 'gammah 0:', gamma/h
   ! Write (6,*) 'gammahk 0:', gammahk
   ! Write (6,*) 'twomuxi2 0:', twomuxi2
   ! Write (6,*) 'xih 0:', xih
   ! Write (6,*) 'xigammahk 0:', xigammahk
   ! Write (6,*) 'gamma2hk 0:', gamma2hk
   ! Write (6,*) 'xi2hk 0:', xi2hk

   If (Input .eq. 1) Then ! P wave input

      ! Set up a0 [equation (64)]
      a0(1) = rhos22*gammak
      a0(2) = -rhos22*twomuxi
      a0(3) = rhos22*xik
      a0(4) = -rhos22

      ! Set up c0 [equation (65)]
      c0(1, 1) = twomuxi2 - gamma2hk
      c0(2, 1) = 2D0*twomuxi*gammak
      c0(3, 1) = twomuxi2 + gamma2hk
      c0(1, 2) = -rhos2k
      c0(2, 2) = 0D0
      c0(3, 2) = -rhos2k
      c0(1, 3) = twomuxi + xigammahk
      c0(2, 3) = 2D0*gammak
      c0(3, 3) = twomuxi - xigammahk
      c0(1, 4) = twomuxi + xigammahk
      c0(2, 4) = -2D0*twomuxi*xik
      c0(3, 4) = twomuxi - xigammahk
      c0(1, 5) = -rhos2h
      c0(2, 5) = 0D0
      c0(3, 5) = rhos2h
      c0(1, 6) = -1D0 + xi2hk
      c0(2, 6) = 2D0*xik
      c0(3, 6) = -(1D0 + xi2hk)

   Else ! SV wave input

      ! Set up a0 [equation (66)]
      gammah = gamma/h
      a0(1) = rhos22*twomuxi
      a0(2) = rhos22*gammah
      a0(3) = -rhos22
      a0(4) = -rhos22*xih

      ! Set up c0 [equation (67)]
      c0(1, 1) = -2D0*twomuxi*gammah
      c0(2, 1) = twomuxi2 - gamma2hk
      c0(3, 1) = twomuxi2 + gamma2hk
      c0(1, 2) = 0D0
      c0(2, 2) = rhos2k
      c0(3, 2) = -rhos2k
      c0(1, 3) = 2D0*twomuxi*xih
      c0(2, 3) = twomuxi + xigammahk
      c0(3, 3) = twomuxi - xigammahk
      c0(1, 4) = -2D0*gammah
      c0(2, 4) = twomuxi + xigammahk
      c0(3, 4) = twomuxi - xigammahk
      c0(1, 5) = 0D0
      c0(2, 5) = rhos2h
      c0(3, 5) = rhos2h
      c0(1, 6) = -2D0*xih
      c0(2, 6) = -1D0 + xi2hk
      c0(3, 6) = -(1D0 + xi2hk)

   End If

   ! Write (6,*) 'a0(1):', a0(1)
   ! Write (6,*) 'a0(2):', a0(2)
   ! Write (6,*) 'a0(3):', a0(3)
   ! Write (6,*) 'a0(4):', a0(4)

   ! Write (6,*) 'i, j, c0(i,j):'
   ! m = 1
   ! Do While (m.le.3)
   !   ii = 1
   !   Do While (ii.le.6)
   !     Write (6,*) m,ii,c0(m,ii)
   !     ii = ii + 1
   !   End Do
   !   m = m + 1
   ! End Do

   ! Multiply a0 by B[1]*B[2]*...*B[last-1]*a[last] to give g [eq(62)]
   g(1) = a0(1)*a(1, 1) + a0(2)*a(2, 1) + a0(3)*a(3, 1) + a0(4)*a(4, 1)
   g(2) = a0(1)*a(1, 2) + a0(2)*a(2, 2) + a0(3)*a(3, 2) + a0(4)*a(4, 2)

   ! Multiply c0 by D[1]*D[2]*...*D[last-1]*c[last] to give j [eq(62)]
   j(1) = c0(1, 1)*c(1) + c0(1, 2)*c(2) + c0(1, 3)*c(3) + c0(1, 4)*c(4) + &
          c0(1, 5)*c(5) + c0(1, 6)*c(6)
   j(2) = c0(2, 1)*c(1) + c0(2, 2)*c(2) + c0(2, 3)*c(3) + c0(2, 4)*c(4) + &
          c0(2, 5)*c(5) + c0(2, 6)*c(6)
   j(3) = c0(3, 1)*c(1) + c0(3, 2)*c(2) + c0(3, 3)*c(3) + c0(3, 4)*c(4) + &
          c0(3, 5)*c(5) + c0(3, 6)*c(6)

   ! Displacement potential reflection and transmission coefficient
   ! amplitudes (NB: exp(+i*omega*t) convention for phase)

   rp = j(1)/j(3)
   rs = j(2)/j(3)

   tp = g(1)/j(3)
   ts = g(2)/j(3)

   ! Energy coefficients (reflectances and transmittances)

   If (Input .eq. 1) Then ! P wave input

      RpE = Abs(rp)**2
      RsE = qs0*Abs(rs)**2/qp0
      TpE = qpn*Abs(tp)**2/qp0
      TsE = qsn*Abs(ts)**2/qp0

   Else ! SV wave input

      RpE = qp0*Abs(rp)**2/qs0
      RsE = Abs(rs)**2
      TpE = qpn*Abs(tp)**2/qs0
      TsE = qsn*Abs(ts)**2/qs0

   End If

END SUBROUTINE RESPONSE

!------------------------------------------------------------------
! Generating the attenuation value (Np/m) from the stored data.

REAL(KIND=8) FUNCTION AttenF(AttenIn, AttenFlag, omega)
   USE SPECIAL_CONSTANTS
   USE Lines
   IMPLICIT NONE
   REAL(KIND=8), INTENT(IN) :: Attenin, omega
   INTEGER, INTENT(IN) :: AttenFlag
   IF (AttenFlag .eq. 0) THEN
      AttenF = AttenIn*dBtoNp
   ELSE IF (AttenFlag .eq. 1) THEN
      AttenF = AttenIn*dBtoNp*omega/twopi
   ELSE
      CALL ERROR(120); RETURN
   END IF
END FUNCTION AttenF

!------------------------------------------------------------------
! Currently, forcing phase speed to be non-zero.

REAL(KIND=8) FUNCTION SpeedF(SpeedIn)
   IMPLICIT NONE
   REAL(KIND=8), INTENT(IN) :: Speedin
   If (SpeedIn .eq. 0D0) Then
      SpeedF = 1D-50
   Else
      SpeedF = SpeedIn
   End If
END FUNCTION SpeedF

!------------------------------------------------------------------
! Generating a modulus from a phase speed, attenuation, and density.

FUNCTION ModulusF(Nthe, Medium, Ispeed, MediumIndex)

   USE Reference
   USE Theoretical
   USE Properties
   USE Lines
   IMPLICIT NONE

   COMPLEX(KIND=8) :: ModulusF
   INTEGER, INTENT(IN) :: Nthe, Medium, Ispeed, MediumIndex

   INTEGER :: AttenFlag
   REAL(KIND=8) :: Speed, Atten
   REAL(KIND=8) :: DensityIn, SpeedIn, AttenIn, omega

   Estatus = .true.

   DensityIn = density(Nthe, Medium)
   IF (Ispeed .eq. 1) THEN                ! Longitudinal components
      SpeedIn = cspeed(Nthe, Medium)
      AttenIn = catten(Nthe, Medium)
      AttenFlag = cattenf(Nthe, Medium)
   ELSE IF (Ispeed .eq. 2) THEN        ! Shear components
      SpeedIn = bspeed(Nthe, Medium)
      AttenIn = batten(Nthe, Medium)
      AttenFlag = battenf(Nthe, Medium)
   ELSE
      CALL ERROR(152); RETURN
   END IF

   omega = w0
   Speed = SpeedF(SpeedIn)
   Atten = AttenF(AttenIn, AttenFlag, omega)

   ModulusF = DensityIn/(Dcmplx(1D0/Speed, -Atten/omega))**2

   IF (Dreal(ModulusF) .lt. 0D0) THEN
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') &
         '   Theoretical set Nthe:  ', Nthe
      If (MediumIndex .eq. 0) Then
         WRITE (6, '(1X,A)', ADVANCE='YES') &
            '   Surface:  Unknown (index not passed)'
      Else
         WRITE (6, '(1X,A)', ADVANCE='YES') &
            '   Surface:  ', SurfaceList(MediumIndex)
      End If
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') 'Medium: ', Medium
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') 'Ispeed: ', Ispeed
      WRITE (6, '(1X,A,"(",1PE15.8,",",1PE15.8,")")', ADVANCE='YES') &
         'Modulus: ', ModulusF
      WRITE (6, '(1X,A,F)', ADVANCE='YES') 'Phase speed: ', SpeedIn
      WRITE (6, '(1X,A,F)', ADVANCE='YES') 'Density: ', DensityIn
      WRITE (6, '(1X,A,I4)', ADVANCE='YES') &
         'Attenuation (dB/-): ', AttenIn
      WRITE (6, '(1X,A,F)', ADVANCE='YES') &
         'Phase speed at this freq requires atten less than:', &
         omega*20D0/DLOG(10D0)/SpeedIn
      CALL ERROR(117); RETURN
   END IF

END FUNCTION ModulusF

!------------------------------------------------------------------
! Physical restriction on the complex transform parameter h.
! (h is equivalent to i*wavenumber.)
! In the original REFLAYER the requirement Re[h] > 0 is equivalent
! to  Im[h] > 0 here for subroutine RESPONSE.

SUBROUTINE Physical(h)
   IMPLICIT NONE
   COMPLEX(KIND=8), INTENT(INOUT) :: h
   IF (Dimag(h) .lt. 0D0) h = -h        ! Case A  (L & P)
! IF ( Dreal(h).lt.0D0 ) h = -h        ! Case B
! h = h
END SUBROUTINE Physical

!------------------------------------------------------------------
! Physical restriction on the relative values of the longitudinal
! modulus (M = lambda+2mu) and and the shear modulus (G=mu).
! Note: the moduli have positive real and imaginary parts in RESPONSE.

SUBROUTINE PhysicalMod(M, G, medium, MediumIndex)

   USE Reference
   USE Lines
   IMPLICIT NONE

   COMPLEX(KIND=8), INTENT(IN) :: M, G
   INTEGER, INTENT(IN) :: medium, MediumIndex

   Estatus = .true.
! Trial for Graham Day; is 2 too restrictive for imag?
! 4/3 suggested
   IF (Dreal(M) .lt. 2D0*DREAL(G) .or. DIMAG(M) .lt. 4D0*DIMAG(G)/3D0) THEN
! IF ( Dreal(M).lt.2D0*Dreal(G) .or. Dimag(M).lt.2D0*Dimag(G) ) THEN
      ! This restriction forces the Lame coefficient lambda to be
      ! positive (Poisson's ratio sigma > 0 if mu > 0) since negative
      ! sigma is not observed.
      ! (See The Mathematical Theory of Elasticity, by Love, for the
      ! less stringent restriction:  -1 < sigma < 1/2)
      WRITE (6, '(1X,A,A)', ADVANCE='YES') &
         'Surface:  ', SurfaceList(MediumIndex)
      WRITE (6, '(1X,A,I5)', ADVANCE='YES') &
         'Medium:', medium
      WRITE (6, '(1X,A,"(",1PE15.8,",",1PE15.8,")")', ADVANCE='YES') &
         'Longitudinal modulus, M:  ', M
      WRITE (6, '(1X,A,"(",1PE15.8,",",1PE15.8,")")', ADVANCE='YES') &
         'Shear modulus, G:         ', G
      ! Trial for Graham Day; is 2 too restrictive for imag?
      ! 4/3 suggested
      line = 'Either  Re(M) < 2 Re(G)  or  Im(M) < (4/3) Im(G)'
      ! line = 'Either  Re(M) < 2 Re(G)  or  Im(M) < 2 Im(G)'
      CALL ERROR(118, line); RETURN
   END IF

END SUBROUTINE PhysicalMod
