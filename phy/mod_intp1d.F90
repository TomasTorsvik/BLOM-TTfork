! ------------------------------------------------------------------------------
! Copyright (C) 2002-2024 Mats Bentsen, Mariana Vertenstein
!
! This file is part of BLOM.
!
! BLOM is free software: you can redistribute it and/or modify it under the
! terms of the GNU Lesser General Public License as published by the Free
! Software Foundation, either version 3 of the License, or (at your option)
! any later version.
!
! BLOM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
! more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with BLOM. If not, see <https://www.gnu.org/licenses/>.
! ------------------------------------------------------------------------------

module mod_intp1d

  implicit none
  private

  public :: intp1d

contains

  real function intp1d(d1,d2,d3,d4,d5,x)

    ! --- ------------------------------------------------------------------
    ! --- Interpolate data in one dimension.
    ! --- ------------------------------------------------------------------

    ! Arguments
    real, intent(in) :: d1,d2,d3,d4,d5,x

    ! Local variables
    real :: a1,a2,a3,b1,b2,b3,b4,b5,c1,c2
    parameter(a1=-3./7.,a2=-15./7.,a3= 3./2., &
              b1= 4./7.,b2=-16./7.,b3=15./7.,b4=-5./7.,b5 = 2./7., &
              c1 = -1./7.,c2=  9./14)
    real :: a,b,c

    a = a1*(d1+d5)+a2*d3+a3*(d2+d4)
    b = b1*d1+b2*d2+b3*d3+b4*d4+b5*d5
    c = c1*(d1+d4)+c2*(d2+d3)

    intp1d = (a*x+b)*x+c

  end function intp1d

end module mod_intp1d
