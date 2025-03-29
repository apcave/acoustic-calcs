MODULE error_handling
   IMPLICIT NONE
   CHARACTER(LEN=256) :: error_message = "No error"
   INTEGER :: error_code = 0
   LOGICAL :: has_error = .FALSE.

CONTAINS

   SUBROUTINE set_error(code, message)
      INTEGER, INTENT(IN) :: code
      CHARACTER(LEN=*), INTENT(IN) :: message
      print *, 'Error:', code, message
      error_code = code
      error_message = message
      has_error = .TRUE.
   END SUBROUTINE set_error

   SUBROUTINE get_error(code, message)
      INTEGER, INTENT(OUT) :: code
      CHARACTER(LEN=*), INTENT(OUT) :: message

      if (has_error) then
         code = error_code
         message = error_message
      else
         code = 0
         message = 'No error'
      end if
   END SUBROUTINE get_error

END MODULE error_handling

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
      imagI = complex(0D0, 1D0)
END MODULE SPECIAL_CONSTANTS

MODULE acoustic_sim
   USE error_handling
   IMPLICIT NONE

CONTAINS
   ! In this implementation the moduli are constants with frequency as are the wave speeds.
   ! The input attenuation is dB/m.
   ! The moduli are used in the calculation and so the wave speed and attenatuion are converted to moduli.
   ! This conversion requires the frequency of the wave as the moduli to wave speed conversion is frequency dependent.
   SUBROUTINE simulate_composite(isComp, angle, freq, density, thick, cp, attp, LongM, cs, atts, mu, &
                                 rp, tp, rs, ts, &
                                 RpE, TpE, RsE, TsE, &
                                 ret_code, ret_message)
      ! This routine is designed to accept data from python, validate the data and invoke the Fortran simulation.
      USE SPECIAL_CONSTANTS
      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: isComp
      REAL(KIND=8), INTENT(IN) :: angle(:), freq(:), density(:), thick(:), cp(:), attp(:), cs(:), atts(:)
      complex(KIND=8), INTENT(IN):: LongM(:), mu(:)
      REAL(KIND=8), INTENT(INOUT) :: RpE(:), TpE(:), RsE(:), TsE(:)
      complex(kind=8), intent(INOUT) :: rp(:), tp(:), rs(:), ts(:)
      INTEGER, INTENT(OUT) :: ret_code
      CHARACTER(LEN=256), INTENT(OUT) :: ret_message

      ! isComp, is the input incedent wave a compressional wave or shear wave.
      ! angle, is the vector of angles of incidence of the wave (degrees).
      ! freq, is the vector of frequencies of the wave (Hz).
      ! density, is the density of the composite material layers (kg/m^3).
      ! cp, is the compressional wave speed of the composite material layers (m/s).
      ! attp, is the attenuation the compression wave in composite material layers (dB/m).
      ! LongM, is the longitudinal modulus of the composite material layers (MPa).
      ! cs, is the shear wave speed of the composite material layers (m/s).
      ! atts, is the attenuation the shear wave in composite material layers (dB/m).
      ! mu, is the shear modulus of the composite material layers (MPa).
      ! rp, is the reflection coefficient of the compressional wave.
      ! tp, is the transmission coefficient of the compressional wave.
      ! rs, is the reflection coefficient of the shear wave.
      ! ts, is the transmission coefficient of the shear wave.
      ! RpE, is the energy of the reflected compressional wave.
      ! TpE, is the energy of the transmitted compressional wave.
      ! RsE, is the energy of the reflected shear wave.
      ! TsE, is the energy of the transmitted shear wave.
      ! error_code, is the error code, zero for no error.
      ! message, is the error_message.

      integer :: i, numLayers, numCalcs
      real(kind=8) :: cosinc, omega

      if (size(freq) > 1 .and. size(angle) > 1) then
         call set_error(2, 'There can only be a vector of either angle or frequency the over variable is a scaler.')
         return
      end if

      numCalcs = max(size(freq), size(angle))

      numLayers = size(density)

      if (numLayers < 2) then
         call set_error(2, 'There must be at least two layers in the composite material.')
      end if

      if (numLayers /= size(cp) .or. numLayers /= size(attp) .or. numLayers /= size(LongM) .or. numLayers /= size(cs) &
          .or. numLayers /= size(atts) .or. numLayers /= size(mu)) then
         call set_error(2, 'The size of the material properties vectors must be equal.')
      end if

      IF (ANY(angle < 0.0 .and. angle <= 90.0)) THEN
         CALL set_error(2, 'Angle values must be positive and less that 90 degrees')
      END IF

      IF (ANY(freq < 0.0)) THEN
         CALL set_error(2, 'Frequency values must be positive')
      END IF

      IF (ANY(density < 0.0)) THEN
         CALL set_error(2, 'Density values must be positive')
      END IF

      IF (ANY(cp < 0.0)) THEN
         CALL set_error(2, 'Compressional wave speed values must be positive')
      END IF

      IF (ANY(attp < 0.0)) THEN
         CALL set_error(2, 'Attenuation values for compressional wave must be positive')
      END IF

      IF (ANY(cs < 0.0)) THEN
         CALL set_error(2, 'Shear wave speed values must be positive')
      END IF

      IF (ANY(atts < 0.0)) THEN
         CALL set_error(2, 'Attenuation values for shear wave must be positive')
      END IF

      DO i = 1, numLayers
         IF (cp(i) > 0.0 .and. abs(LongM(i)) > 0.0) THEN
            CALL set_error(2, 'Only the compressional wave or the longitudinal modulus can be defined for a layer.')
         END IF

         IF (cs(i) > 0.0 .and. abs(mu(i)) > 0.0) THEN
            CALL set_error(2, 'Only the shear wave or the shear modulus can be defined for a layer.')
         END IF
      END DO

      IF (ANY(REAL(LongM) < 0.0)) THEN
         CALL set_error(2, 'Real part of Longitudinal modulus values must not be less than zero')
      END IF

      IF (ANY(AIMAG(LongM) < 0.0)) THEN
         CALL set_error(2, 'Imaginary part of Longitudinal modulus values must not be less than zero')
      END IF

      IF (ANY(REAL(mu) < 0.0)) THEN
         CALL set_error(9, 'Real part of shear modulus values must not be less than zero')
      END IF

      IF (ANY(AIMAG(mu) < 0.0)) THEN
         CALL set_error(10, 'Imaginary part of the shear modulus values must not be less than zero')
      END IF

      if (has_error) then
         ret_message = error_message
         ret_code = error_code
         return
      end if

      !ALLOCATE (rp(numCalcs), tp(numCalcs), rs(numCalcs), ts(numCalcs))
      !ALLOCATE (RpE(numCalcs), TpE(numCalcs), RsE(numCalcs), TsE(numCalcs))

      DO i = 1, numCalcs
         IF (size(freq) > 1) THEN
            cosinc = COS(angle(1)*radians)
            omega = freq(i)*twopi
         else
            cosinc = COS(angle(i)*radians)
            omega = freq(1)*twopi
         end if

         if (omega == 0.0) then
            if (isComp) Then
               rp(i) = 0.0
               rs(i) = 0.0
               tp(i) = 1.0
               ts(i) = 0.0
               RsE(i) = 0.0
               TsE(i) = 0.0
               RpE(i) = 0.0
               TpE(i) = 1.0
            Else
               rp(i) = 0.0
               rs(i) = 0.0
               tp(i) = 0.0
               ts(i) = 1.0
               RsE(i) = 0.0
               TsE(i) = 1.0
               RpE(i) = 0.0
               TpE(i) = 0.0
            End If
         else

            call RESPONSE(isComp, cosinc, omega, density, thick, cp, cs, attp, atts, LongM, mu, &
                          rp(i), tp(i), rs(i), ts(i), RpE(i), TpE(i), RsE(i), TsE(i))
         end if

         if (has_error) then
            ret_message = error_message
            ret_code = error_code
            return
         end if
      END DO

      ret_message = error_message
      ret_code = error_code

   END SUBROUTINE simulate_composite

   FUNCTION WaveToModulus(rho, c, atten, omega)
      USE error_handling
      USE SPECIAL_CONSTANTS
      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: rho, c, atten, omega
      COMPLEX(KIND=8) :: WaveToModulus

      ! WaveToModulus = rho/(1/c - (atten/omega)*imagI)**2
      WaveToModulus = rho/complex(1D0/c, -(dBtoNp*atten/omega))**2

      IF (real(WaveToModulus) .lt. 0D0) THEN
         call set_error(3, 'The real component of the modulus is negative')
      END IF

      IF (imag(WaveToModulus) .lt. 0D0) THEN
         call set_error(3, 'The imaginary component of the modulus is negative')
      END IF

   END FUNCTION WaveToModulus

   SUBROUTINE CheckModuli(M, G)
      USE error_handling
      IMPLICIT NONE

      COMPLEX(KIND=8), INTENT(IN) :: M, G

      IF (Dreal(M) .lt. 2D0*DREAL(G) .or. DIMAG(M) .lt. 4D0*DIMAG(G)/3D0) THEN
         ! IF ( Dreal(M).lt.2D0*Dreal(G) .or. Dimag(M).lt.2D0*Dimag(G) ) THEN
         ! This restriction forces the Lame coefficient lambda to be
         ! positive (Poisson's ratio sigma > 0 if mu > 0) since negative
         ! sigma is not observed.
         ! (See The Mathematical Theory of Elasticity, by Love, for the
         ! less stringent restriction:  -1 < sigma < 1/2)
         call set_error(3, 'The Lame coefficient lambda is negative')
      END IF

   END SUBROUTINE CheckModuli

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

   SUBROUTINE LayerPhysical(rho, omega, cp, attp, iLongM, cs, atts, imu, LongM, mu)
      USE error_handling
      USE SPECIAL_CONSTANTS
      IMPLICIT NONE

      REAL(KIND=8), INTENT(IN) :: rho, omega, cp, attp, cs, atts
      complex(KIND=8), INTENT(in) :: iLongM, imu
      COMPLEX(KIND=8), INTENT(OUT) :: LongM, mu

      real(kind=8) :: error

      error = 1e3

      if (cs .gt. 0D0) then
         mu = WaveToModulus(rho, cs, atts, omega)
         IF (has_error) RETURN
      else
         mu = imu
      end if

      ! If the modulus real or imag is zero set it equal to a minimal value for numberical stability.
      if (AIMAG(mu) .lt. 1D0) then
         mu = mu + error*imagI
      end if

      if (REAL(mu) .lt. 1D0) then
         mu = mu + error*1D0
      end if

      if (cp .gt. 0D0) then
         LongM = WaveToModulus(rho, cp, attp, omega)
         IF (has_error) RETURN
      else
         LongM = iLongM
      end if

      ! If the modulus real or imag is zero set it equal to a minimal value for numberical stability.
      if (AIMAG(LongM) .lt. 1) then
         LongM = LongM + error*2*imagI
      end if

      if (REAL(LongM) .lt. 1) then
         LongM = LongM + error*1D0
      end if



      CALL CheckModuli(LongM, mu)
      IF (has_error) THEN 
         Write (6, *) '---------------------------------------------'
         Write (6, *) 'omega:', omega
         Write (6, *) 'Density rho:', rho
         Write (6, *) 'Compressional Wave Speed cp:', cp
         Write (6, *) 'Attenuation attp:', attp
         Write (6, *) 'Shear Wave Speed cs:', cs
         Write (6, *) 'Attenuation atts:', atts

         Write (6, *) 'Shear Modulus mu:', mu
         Write (6, *) 'Longitudinal Modulus:', LongM 
         RETURN
      ENDIF


   END SUBROUTINE LayerPhysical

   SUBROUTINE RESPONSE(isComp, cosinc, in_omega, density, thick, cp, cs, attp, atts, vLongM, vmu, &
                       rp, tp, rs, ts, RpE, TpE, RsE, TsE)

      USE error_handling
      USE SPECIAL_CONSTANTS
      IMPLICIT NONE

      logical, INTENT(IN) :: isComp ! 1 for P wave, 2 for SV wave
      REAL(KIND=8), INTENT(IN) :: cosinc, in_omega ! Only one combinatioon is calulcated.
      REAL(KIND=8), INTENT(IN) :: density(:), thick(:), cp(:), cs(:), attp(:), atts(:)
      COMPLEX(KIND=8), INTENT(IN) :: vLongM(:), vmu(:)
      COMPLEX(KIND=8), INTENT(OUT) :: rp, tp, rs, ts
      REAL(KIND=8), INTENT(OUT) :: RpE, RsE, TpE, TsE

      INTEGER :: medium, m, n, ii, last
      REAL(KIND=8) :: theta0, fnorm, amax, thetamin, thetamax, omega
      REAL(KIND=8) :: dd, rho, rho0, qp0, qs0, qpn, qsn

      COMPLEX(KIND=8) :: rhos22 ! Was a real check this is correct

      COMPLEX(KIND=8) :: g(2), a0(4), B(4, 4), a(4, 2), atemp(4, 2)
      COMPLEX(KIND=8) :: j(3), c0(3, 6), D(6, 6), c(6), ctemp(6)
      COMPLEX(KIND=8) :: z, i, s, k, h, cpsq, cp0sq, xi, xi0, &
                         gamma, eta, eta2, nu, nu1, nu2, rhos2, ch, sh, ck, sk, hk, &
                         nu1t, nu12, chck, chck1, shsk, cssq, rh, rk, twomuxi, &
                         twomuxi2, xik, xih, gammak, gammahk, xigammahk, &
                         gammah, cs0sq, rhos2h, rhos2k, gamma2hk, cp0, cs0, &
                         LongM, LongM0, mu, mu0, shrh, skrk, rhrk, &
                         nunu1, xi2hk, chss, shss, ckss, skss
      REAL(KIND=8)  :: hlns, klns, us, tempreal, dmax, bdmax, emin

      omega = in_omega
      if (omega < 1) omega = 1.0

      i = complex(0, 1)
      emin = -709

      last = size(density)

      theta0 = acos(cosinc)
      ! Incidence angle theta should not be exactly zero with this algorithm
      thetamin = eps
      theta0 = max(theta0, thetamin)
      ! theta should not be exactly zero with this algorithm
      thetamin = eps
      thetamax = halfpi*0.999999D0
      theta0 = min(theta0, thetamax)

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

      rho0 = density(1)
      call LayerPhysical(rho0, omega, cp(1), attp(1), vLongM(1), cs(1), atts(1), vmu(1), LongM0, mu0)
      IF (has_error) RETURN

      cp0sq = LongM0/rho0
      cs0sq = mu0/rho0

      If (isComp) Then
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
      Else
         cs0 = Sqrt(cs0sq)
         xi0 = s*Sin(theta0)/cs0
         If (real(cs0) .lt. 1D-3) THEN
            call set_error(3, 'Cannot do shear input with a near liquid layer.')
            IF (has_error) RETURN
         END IF

      End If

      xi = xi0

      ! Write (6, *) 'xi:', xi
      ! Write (6, *) 'xi2:', xi**2
      ! Write (6, *) 's:', s
      ! Write (6, *) 's2:', s**2

      ! Last medium (n+1) properties

      rho = density(last)
      call LayerPhysical(rho, omega, cp(last), attp(last), vLongM(last), cs(last), atts(last), vmu(last), LongM, mu)
      IF (has_error) RETURN

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

      ! Write (6, *) '--------------------------------------------------------------------'
      ! Write (6, *) 'i, j, a(i,j):'
      ! m = 1
      ! Do While (m .le. 3)
      !    ii = 1
      !    Do While (ii .le. 2)
      !       Write (6, *) m, ii, a(m, ii)
      !       ii = ii + 1
      !    End Do
      !    m = m + 1
      ! End Do

      ! Set up c[last] [equation(63)]
      c(1) = hk + xi**2
      c(2) = -rhos2*k
      c(3) = xi*gamma - twomuxi*hk
      c(4) = c(3)
      c(5) = rhos2*h
      c(6) = -twomuxi**2*hk - gamma**2

      ! Write (6, *) 'c(1):', c(1)
      ! Write (6, *) 'c(2):', c(2)
      ! Write (6, *) 'c(3):', c(3)
      ! Write (6, *) 'c(4):', c(4)
      ! Write (6, *) 'c(5):', c(5)
      ! Write (6, *) 'c(6):', c(6)

      medium = last - 1
      Do While (medium .ge. 1) ! Begin *Medium* loop

         rho = density(medium)
         dd = thick(medium)
         call LayerPhysical(rho, omega, cp(medium), attp(medium), vLongM(medium), cs(medium), atts(medium), vmu(medium), LongM, mu)
         IF (has_error) RETURN

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

         ! gamma is related to the propagation angle within the layer
         ! and is not explicitly used here. Commented out.
         ! gamma = rhos2 - 2D0*mu*xi**2        ! Equation (27)

         nu = 2D0*mu*xi**2/rhos2
         eta = rhos2/xi
         eta2 = eta**2
         nu2 = nu**2
         nu1 = 1D0 - nu
         nunu1 = nu*nu1
         nu1t = 1D0 - 2D0*nu
         nu12 = nu1**2

         rh = (h/xi)**2
         rk = (k/xi)**2

         rhrk = rh*rk

         ! Scaling cosh & sinh to avoid overflow/underflow
         ! using David Bartel's algorithm
         z = -h*dd
         call set_scaled_cosh_sinh(z, chss, shss, hlns)
         z = -k*dd
         call set_scaled_cosh_sinh(z, ckss, skss, klns)
         ! Matrix B contains single cosh & sinh factors.
         ! Scaled terms also need the complementary exp() factor
         ! applied

         ck = 0
         sk = 0
         if (-hlns >= emin) then
            tempreal = exp(-hlns)
            ck = tempreal*ckss
            sk = tempreal*skss*xi/k
         end if

         ch = 0
         sh = 0
         if (-klns >= emin) then
            tempreal = exp(-klns)
            ch = tempreal*chss
            sh = tempreal*shss*xi/h
         end if

         ! Valid products for scaled B only (redone for D below)
         shrh = sh*rh                        ! s_h r_h
         skrk = sk*rk                        ! s_k r_k

         ! Unscaled cosh & sinh (Original)
         ! Not used for scaled cosh & sinh
         !
         ! ch = (Exp(-h*dd) + Exp(h*dd))/2D0        ! Cosh(-h*dd)
         ! ck = (Exp(-k*dd) + Exp(k*dd))/2D0        ! Cosh(-k*dd)
         ! sh = xi*(Exp(-h*dd)-Exp(h*dd))/(2D0*h)        ! (xi/h)Sinh(-h*dd)
         ! sk = xi*(Exp(-k*dd)-Exp(k*dd))/(2D0*k)        ! (xi/k)Sinh(-k*dd)

         fnorm = 1

         ! fnorm seems to be insufficient? I use the following to minimize
         ! large numbers only, otherwise use 1D0.  But note that amax is
         ! divided into array values so can conceivably cause overflow
         ! itself if 1D0 is used. Not part of L & P's procedure (but fully
         ! consistent with it).

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

         bdmax = maxval(abs(B))

         us = 0
         if (-hlns >= emin) then
            us = exp(-hlns)
         end if
         if (-klns >= emin) then
            us = us*exp(-klns)
         end if
         if (-klns < emin) then
            us = 0
         end if

         ! Valid values for scaled D only (done for B above)
         ck = ckss
         sk = skss*xi/k
         ch = chss
         sh = shss*xi/h

         shsk = sh*sk                        ! s_h s_k
         chck = ch*ck                        ! c_h c_k
         chck1 = us - chck    ! us - c_h c_k NB: us is 1D0 scaled

         ! Valid products for scaled D only (done for B above)
         shrh = sh*rh                        ! s_h r_h
         skrk = sk*rk                        ! s_k r_k

         ! Set up D[med]
         D(1, 1) = chck + 2D0*nunu1*chck1 + (nu12 + nu2*rhrk)*shsk
         D(1, 2) = (ch*sk + ck*shrh)/eta
         D(1, 3) = (nu1t*chck1 - (nu1 - nu*rhrk)*shsk)/eta
         D(1, 4) = D(1, 3)
         D(1, 5) = -(ch*skrk + ck*sh)/eta
         D(1, 6) = (2D0*chck1 - shsk*(1D0 + rhrk))/eta2
         D(2, 1) = eta*(nu12*ck*sh + nu2*ch*skrk)
         D(2, 2) = chck
         D(2, 3) = nu*ch*skrk - nu1*ck*sh
         D(2, 4) = D(2, 3)
         D(2, 5) = -shsk*rk
         D(2, 6) = D(1, 5)
         D(3, 1) = eta*(nunu1*nu1t*chck1 + (nu1*nu12 - nu*nu2*rhrk)*shsk)
         D(3, 2) = nu1*ch*sk - nu*ck*shrh
         D(3, 3) = D(2, 2) - D(1, 1) + us
         D(3, 4) = D(3, 4) - D(1, 1)
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
         D(6, 1) = eta2*(2D0*nu2*nu12*chck1 - (nu2**2*rhrk + nu12**2)*shsk)
         D(6, 2) = D(5, 1)
         D(6, 3) = D(4, 1)
         D(6, 4) = D(3, 1)
         D(6, 5) = D(2, 1)
         D(6, 6) = D(1, 1)

         dmax = maxval(abs(D))
         bdmax = max(bdmax, dmax)

         ! Multiply B[med] by B[med+1]*B[med+2]*...*B[last-1]*a[last]
         ! using atemp[]
         do m = 1, 4
            do n = 1, 2
               atemp(m, n) = 0
            end do
         end do

         m = 1
         do while (m <= 4)
            n = 1

            do while (n <= 2)
               ii = 1
               do while (ii <= 4)
                  atemp(m, n) = atemp(m, n) + (B(m, ii)/bdmax)*a(ii, n)
                  ii = ii + 1
               end do
               atemp(m, n) = fnorm*atemp(m, n)
               ! For large numbers of liquids with b=0 (1D-50 here) I find that
               ! atemp() and ctemp() can increase to cause overflow despite
               ! the above normalization.  Here, I evaluate another factor,
               ! which must be applied to both a() and c().
               n = n + 1
            end do
            m = m + 1
         end do

         amax = maxval(abs(atemp))

         ! Return atemp[] to a[]
         do m = 1, 4
            do n = 1, 2
               a(m, n) = atemp(m, n)/amax
            end do
         end do

         ! Multiply D[med] by D[med+1]*D[med+2]*...*D[last-1]*c[last]
         ! using ctemp[]
         do m = 1, 6
            ctemp(m) = 0
            do ii = 1, 6
               ctemp(m) = ctemp(m) + (D(m, ii)/bdmax)*c(ii)
            end do
            ctemp(m) = fnorm*ctemp(m)
         end do

         ! Return ctemp[] to c[]
         do m = 1, 6
            c(m) = ctemp(m)/amax
         end do

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

      ! hk = h*k
      xih = xi/h
      xik = xi/k
      xi2hk = xih*xik
      gammak = gamma/k
      gammahk = gammak/h
      xigammahk = xi*gammahk
      gamma2hk = gamma*gammahk

      If (isComp) Then ! P wave input

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

      ! Write (6, *) 'a0(1):', a0(1)
      ! Write (6, *) 'a0(2):', a0(2)
      ! Write (6, *) 'a0(3):', a0(3)
      ! Write (6, *) 'a0(4):', a0(4)

      ! Write (6, *) 'i, j, a(i,j):'
      ! m = 1
      ! Do While (m .le. 3)
      !    ii = 1
      !    Do While (ii .le. 2)
      !       Write (6, *) m, ii, a(m, ii)
      !       ii = ii + 1
      !    End Do
      !    m = m + 1
      ! End Do

      ! Write (6, *) 'i, j, c0(i,j):'
      ! m = 1
      ! Do While (m .le. 3)
      !    ii = 1
      !    Do While (ii .le. 6)
      !       Write (6, *) m, ii, c0(m, ii)
      !       ii = ii + 1
      !    End Do
      !    m = m + 1
      ! End Do

      ! Write (6, *) 'c(1):', c(1)
      ! Write (6, *) 'c(2):', c(2)
      ! Write (6, *) 'c(3):', c(3)
      ! Write (6, *) 'c(4):', c(4)
      ! Write (6, *) 'c(5):', c(5)
      ! Write (6, *) 'c(6):', c(6)

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

      If (isComp) Then ! P wave input

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

      rp = CONJG(rp)
      rs = CONJG(rs)

      tp = CONJG(tp)
      ts = CONJG(ts)

   END SUBROUTINE RESPONSE

   SUBROUTINE set_scaled_cosh_sinh(z, scaled_cosh, scaled_sinh, ln_scale)
      IMPLICIT NONE
      complex(kind=8), INTENT(IN) :: z
      complex(kind=8), INTENT(OUT) :: scaled_cosh, scaled_sinh
      REAL(kind=8), INTENT(OUT) :: ln_scale

      REAL(kind=8) :: x, y, t, w, c, s, a, b, emin

      ! Set real and imaginary parts of argument z of hyperbolic cosine
      x = dreal(z)
      y = dimag(z)

      ! Set natural logarithm of scale factor
      emin = -709.0d0
      ln_scale = abs(x)
      if (x < 0) then
         ! Set exponent t = 2*x in exp(t) = exp(2*x)
         t = 2*x
         ! If exponent will cause an underflow then
         if (t < emin) then
            ! Use reduced formulae
            c = cos(y)/2
            s = sin(y)/2
            ! Note that cmplx() uses the default real. dcmplx() forces
            ! double precision. On VMS Digital fortran, /REAL_SIZE=64
            ! would force cmplx() to return the required precision
            scaled_cosh = complex(c, -s)
            scaled_sinh = complex(-c, s)
         else
            ! Use full formulae
            w = exp(t)
            c = cos(y)/2
            s = sin(y)/2
            a = (w + 1)*c
            b = (w - 1)*s
            scaled_cosh = complex(a, b)
            a = (w - 1)*c
            b = (w + 1)*s
            scaled_sinh = complex(a, b)
         end if
      else if (x == 0) then
         ! Use reduced formulae
         scaled_cosh = complex(cos(y), 0)
         scaled_sinh = complex(0, real(sin(y)))
      else if (x > 0) then
         ! Set exponent t = -2*x in exp(-t) = exp(-2*x)
         t = -2*x
         ! If exponent will cause an underflow then
         if (t < emin) then
            ! Use reduced formulae
            c = cos(y)/2
            s = sin(y)/2
            scaled_cosh = complex(c, s)
            scaled_sinh = scaled_cosh
         else
            ! Use full formulae
            w = exp(t)
            c = cos(y)/2
            s = sin(y)/2
            a = (1 + w)*c
            b = (1 - w)*s
            scaled_cosh = complex(a, b)
            a = (1 - w)*c
            b = (1 + w)*s
            scaled_sinh = complex(a, b)
         end if
      end if

   end SUBROUTINE set_scaled_cosh_sinh

END MODULE acoustic_sim
