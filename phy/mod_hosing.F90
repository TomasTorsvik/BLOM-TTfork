! ------------------------------------------------------------------------------
! Copyright (C) 2026 Tomas Torsvik
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

module mod_hosing
! ------------------------------------------------------------------------------
! This module contains variables and procedures for freshwater hosing
! experiments.
!
! Hosing is enabled by setting 'use_hosing = .true.' in namelist 'hosing'. When
! disabled (default), no hosing file is read and no freshwater is added.
!
! A freshwater volume flux per grid cell [m3 s-1] is read from 'fwf_file' and
! converted to a freshwater flux per unit area [kg m-2 s-1] multiplied by
! 'fwf_scale', 'fwf_mask'. The applied hosing flux is
! 'hosing_fraction()*fwf_mask', where the hosing fraction follows a linear ramp
! in time:
!
!    s = min(max(elapsed/fwf_ramp_ndays, 0), 1)
!    hosing_fraction = fwf_frac_start + (fwf_frac_end - fwf_frac_start)*s
!
! Here 'elapsed' is the number of days since 'fwf_ramp_start', computed from
! the model calendar date. The ramp is thus independent of the run type
! (startup, hybrid, branch, continue). Before 'fwf_ramp_start' the hosing
! fraction is 'fwf_frac_start', and after the end of the ramp it is
! 'fwf_frac_end'. If 'fwf_ramp_ndays' is zero, the hosing fraction steps from
! 'fwf_frac_start' to 'fwf_frac_end' at 'fwf_ramp_start'.
! ------------------------------------------------------------------------------

   use mod_types, only: r8
   use mod_config, only: inst_suffix
   use mod_constants, only: L_mks2cgs
   use mod_xc
   use mod_calendar, only: date_type, daynum_diff, date_check, &
                           calendar_noerr, calendar_errstr
   use mod_time, only: date, calendar, nstep, nstep_in_day
   use mod_grid, only: scp2
   use mod_checksum, only: csdiag, chksummsk
   use netcdf
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

   implicit none

   private

   ! Variables set by namelist 'hosing'.
   logical :: &
      use_hosing      ! If true, freshwater hosing is enabled.
   character(len = 256) :: &
      fwf_file        ! Name of file containing freshwater volume flux per grid
                      ! cell, variable 'fwf_mask' [m3 s-1].
   real(r8) :: &
      fwf_scale, &    ! Scale factor applied to the freshwater flux [].
      fwf_frac_start, & ! Hosing fraction at start of ramp [].
      fwf_frac_end, & ! Hosing fraction at end of ramp [].
      fwf_ramp_ndays  ! Duration of ramp [days].
   integer :: &
      fwf_ramp_start  ! Start date of ramp [yyyymmdd].

   type(date_type) :: &
      ramp_start_date ! Start date of ramp.

   real(r8), dimension(1 - nbdy:idm + nbdy, 1 - nbdy:jdm + nbdy) :: &
      fwf_mask, &     ! Freshwater flux for hosing experiments, before applying
                      ! the hosing fraction [kg m-2 s-1].
      hosflx          ! Applied hosing freshwater flux [kg m-2 s-1].

   logical :: &
      nml_read = .false. ! True when namelist 'hosing' has been read.

   real(r8) :: &
      fwf_total       ! Global hosing volume flux with hosing fraction equal to
                      ! one, including 'fwf_scale' [m3 s-1].

   real(r8), parameter :: &
      rho_fw = 1.e3_r8, & ! Fresh water density [kg m-3].
      fwf_tol = 1.e-3_r8  ! Relative tolerance for the difference between the
                          ! applied and the prescribed global hosing flux [].

   public :: use_hosing, fwf_mask, hosflx, read_hosing_nml, init_hosing, &
             hosing_fraction, log_hosing

contains

   subroutine read_hosing_nml
   ! ---------------------------------------------------------------------------
   ! Read namelist 'hosing'. Called from 'rdlim' so that 'use_hosing' is known
   ! when the diagnostic namelist is processed.
   ! ---------------------------------------------------------------------------

      character(len = 256) :: nlfnm
      integer :: ios
      logical :: fexist

      namelist /hosing/ use_hosing, fwf_file, fwf_scale, fwf_frac_start, fwf_frac_end, &
                        fwf_ramp_start, fwf_ramp_ndays

      ! Set default values.
      use_hosing = .false.
      fwf_file = 'unset'
      fwf_scale = 1._r8
      fwf_frac_start = 0._r8
      fwf_frac_end = 1._r8
      fwf_ramp_start = -1
      fwf_ramp_ndays = 36500._r8

      ! Read namelist 'hosing'.
      if (mnproc == 1) then
         nlfnm = 'ocn_in'//trim(inst_suffix)
         inquire(file = nlfnm, exist = fexist)
         if (.not. fexist) then
            nlfnm = 'limits'//trim(inst_suffix)
            inquire(file = nlfnm, exist = fexist)
         endif
         if (fexist) then
            open (unit = nfu, file = nlfnm, status = 'old', action = 'read')
            read (unit = nfu, nml = hosing, iostat = ios)
            close (unit = nfu)
            if (ios > 0) then
               write (lp, '(a)') &
                  ' read_hosing_nml: error reading namelist hosing!'
               call xchalt('(read_hosing_nml)')
                      stop '(read_hosing_nml)'
            elseif (ios < 0) then
               write (lp, '(a)') ' read_hosing_nml: namelist hosing not found.'
               use_hosing = .false.
            endif
         endif
         fwf_file = adjustl(fwf_file)
         if (fwf_file == '') fwf_file = 'unset'
      endif
      call xcbcst(use_hosing)
      call xcbcst(fwf_file)
      call xcbcst(fwf_scale)
      call xcbcst(fwf_frac_start)
      call xcbcst(fwf_frac_end)
      call xcbcst(fwf_ramp_start)
      call xcbcst(fwf_ramp_ndays)

      nml_read = .true.

   end subroutine read_hosing_nml

   subroutine init_hosing
   ! ---------------------------------------------------------------------------
   ! Check namelist 'hosing' and, if hosing is enabled, read the freshwater
   ! flux file.
   ! ---------------------------------------------------------------------------

      integer :: errstat

      if (.not. nml_read) call read_hosing_nml

      fwf_mask(:, :) = 0._r8
      hosflx(:, :) = 0._r8

      if (.not. use_hosing) then
         if (mnproc == 1) then
            write (lp, '(a)') ' init_hosing: freshwater hosing disabled.'
         endif
         return
      endif

      ! Check namelist values.
      if (trim(fwf_file) == 'unset') then
         if (mnproc == 1) then
            write (lp, '(a)') ' init_hosing: use_hosing = .true. requires '// &
                              'fwf_file to be set!'
         endif
         call xcstop('(init_hosing)')
                stop '(init_hosing)'
      endif
      if (fwf_scale < 0._r8 .or. &
          fwf_frac_start < 0._r8 .or. fwf_frac_start > 1._r8 .or. &
          fwf_frac_end   < 0._r8 .or. fwf_frac_end   > 1._r8 .or. &
          fwf_ramp_ndays < 0._r8) then
         if (mnproc == 1) then
            write (lp, '(2a)') ' init_hosing: require fwf_scale >= 0, ', &
                               '0 <= fwf_frac_start, fwf_frac_end <= 1 and '// &
                               'fwf_ramp_ndays >= 0!'
         endif
         call xcstop('(init_hosing)')
                stop '(init_hosing)'
      endif
      ramp_start_date = date_type(fwf_ramp_start/10000, &
                                  mod(fwf_ramp_start, 10000)/100, &
                                  mod(fwf_ramp_start, 100))
      errstat = date_check(calendar, ramp_start_date)
      if (fwf_ramp_start < 0 .or. errstat /= calendar_noerr) then
         if (mnproc == 1) then
            write (lp, '(a,i0,a)') ' init_hosing: fwf_ramp_start = ', &
                                   fwf_ramp_start, &
                                   ' must be a valid date [yyyymmdd]!'
         endif
         call xcstop('(init_hosing)')
                stop '(init_hosing)'
      endif

      if (mnproc == 1) then
         write (lp, '(a)')         ' init_hosing: freshwater hosing enabled:'
         write (lp, '(2a)')        '   fwf_file       = ', trim(fwf_file)
         write (lp, '(a,es12.4)')  '   fwf_scale      = ', fwf_scale
         write (lp, '(a,f8.4)')    '   fwf_frac_start = ', fwf_frac_start
         write (lp, '(a,f8.4)')    '   fwf_frac_end   = ', fwf_frac_end
         write (lp, '(a,i8.8)')    '   fwf_ramp_start = ', fwf_ramp_start
         write (lp, '(a,f12.2)')   '   fwf_ramp_ndays = ', fwf_ramp_ndays
      endif

      call read_hosing_file

   end subroutine init_hosing

   subroutine read_hosing_file
   ! ---------------------------------------------------------------------------
   ! Read freshwater volume flux per grid cell [m3 s-1] and convert to
   ! freshwater flux per unit area [kg m-2 s-1].
   ! ---------------------------------------------------------------------------

      real(r8), dimension(itdm, jtdm) :: tmpg
      real(r8), dimension(1 - nbdy:idm + nbdy, 1 - nbdy:jdm + nbdy) :: vflx
      real(r8) :: fwftot
      integer :: i, j, errstat, ncid, varid

      if (mnproc == 1) then
         write (lp, '(2a)') ' reading freshwater flux for hosing from ', &
                            trim(fwf_file)
         call flush(lp)

         ! Open netcdf file.
         errstat = nf90_open(fwf_file, nf90_nowrite, ncid)
         if (errstat /= nf90_noerr) then
            write (lp, '(4a)') ' nf90_open: ', trim(fwf_file), ': ', &
                               nf90_strerror(errstat)
            call xchalt('(read_hosing_file)')
                   stop '(read_hosing_file)'
         endif

         ! Read freshwater volume flux.
         errstat = nf90_inq_varid(ncid, 'fwf_mask', varid)
         if (errstat /= nf90_noerr) then
            write (lp, '(2a)') ' nf90_inq_varid: fwf_mask: ', &
                               nf90_strerror(errstat)
            call xchalt('(read_hosing_file)')
                   stop '(read_hosing_file)'
         endif
         errstat = nf90_get_var(ncid, varid, tmpg)
         if (errstat /= nf90_noerr) then
            write (lp, '(2a)') ' nf90_get_var: fwf_mask: ', &
                               nf90_strerror(errstat)
            call xchalt('(read_hosing_file)')
                   stop '(read_hosing_file)'
         endif

         errstat = nf90_close(ncid)
         if (errstat /= nf90_noerr) then
            write (lp, '(4a)') ' nf90_close: ', trim(fwf_file), ': ', &
                               nf90_strerror(errstat)
            call xchalt('(read_hosing_file)')
                   stop '(read_hosing_file)'
         endif

         ! Set non-finite values (e.g. NaN fill values) to zero.
         where (.not. ieee_is_finite(tmpg)) tmpg = 0._r8

         ! Report total freshwater volume flux in file.
         fwftot = sum(tmpg)
         write (lp, '(a,es12.4,a)') ' Sum fwf_mask in file: ', fwftot, &
                                    ' m3 s-1'
      endif

      call xcaput(tmpg, fwf_mask, 1)

      ! Convert volume flux [m3 s-1] to freshwater flux per unit area
      ! [kg m-2 s-1], with grid cell area converted from model units to [m2].
      ! The scale factor 'fwf_scale' is included here.
   !$omp parallel do private(i)
      do j = 1, jj
         do i = 1, ii
            if (ip(i, j) == 1 .and. scp2(i, j) > 0._r8) then
               fwf_mask(i, j) = fwf_scale*fwf_mask(i, j)*rho_fw &
                                *L_mks2cgs**2/scp2(i, j)
            else
               fwf_mask(i, j) = 0._r8
            endif
         enddo
      enddo
   !$omp end parallel do

      call xctilr(fwf_mask, 1, 1, nbdy, nbdy, halo_ps)

      ! Global hosing volume flux [m3 s-1] applied on the model grid with
      ! hosing fraction equal to one. Differs from the sum in the file if
      ! the file has non-zero values on land points.
      vflx(:, :) = 0._r8
   !$omp parallel do private(i)
      do j = 1, jj
         do i = 1, ii
            vflx(i, j) = fwf_mask(i, j)*scp2(i, j)/(rho_fw*L_mks2cgs**2)
         enddo
      enddo
   !$omp end parallel do
      call xcsum(fwf_total, vflx, ips)

      if (mnproc == 1) then
         write (lp, '(a,es12.4,a,f8.4,a)') &
            ' Maximum hosing applied: ', fwf_total, ' m3 s-1 = ', &
            fwf_total*1.e-6_r8, ' Sv'
         if (abs(fwf_total - fwf_scale*fwftot) &
             > fwf_tol*abs(fwf_scale*fwftot)) then
            write (lp, '(a,es12.4,a)') &
               ' WARNING: read_hosing_file: applied hosing differs from '// &
               'fwf_scale*(sum in file) = ', fwf_scale*fwftot, ' m3 s-1.'
            write (lp, '(a)') &
               '          Check that fwf_file has no values on land points.'
         endif
      endif

      if (csdiag) then
         if (mnproc == 1) then
            write (lp, *) 'read_hosing_file:'
         endif
         call chksummsk(fwf_mask, ip, 1, 'fwf_mask')
      endif

   end subroutine read_hosing_file

   function hosing_fraction() result(frac)
   ! ---------------------------------------------------------------------------
   ! Return the hosing fraction at the current model time.
   ! ---------------------------------------------------------------------------

      real(r8) :: frac

      real(r8) :: elapsed, s
      integer :: errstat, ndays

      if (.not. use_hosing) then
         frac = 0._r8
         return
      endif

      ! Days elapsed since start of ramp, including fraction of current day.
      errstat = daynum_diff(calendar, ramp_start_date, date, ndays)
      if (errstat /= calendar_noerr) then
         write (lp, '(2a)') ' hosing_fraction: daynum_diff error: ', &
                            trim(calendar_errstr(errstat))
         call xchalt('(hosing_fraction)')
                stop '(hosing_fraction)'
      endif
      elapsed = real(ndays, r8) &
              + real(mod(nstep, nstep_in_day), r8)/real(nstep_in_day, r8)

      if (fwf_ramp_ndays > 0._r8) then
         s = min(max(elapsed/fwf_ramp_ndays, 0._r8), 1._r8)
      elseif (elapsed >= 0._r8) then
         s = 1._r8
      else
         s = 0._r8
      endif

      frac = fwf_frac_start + (fwf_frac_end - fwf_frac_start)*s

   end function hosing_fraction

   subroutine log_hosing(frac)
   ! ---------------------------------------------------------------------------
   ! Write the hosing fraction and the global hosing flux to the log at the
   ! first time step of each model day.
   ! ---------------------------------------------------------------------------

      real(r8), intent(in) :: frac ! Hosing fraction [].

      if (.not. use_hosing) return
      if (mnproc /= 1 .or. mod(nstep, nstep_in_day) /= 0) return

      write (lp, '(a,i4.4,2(a,i2.2),a,f10.6,a,es12.4,a,f8.4,a)') &
         ' hosing: ', date%year, '-', date%month, '-', date%day, &
         '  fraction = ', frac, &
         '  flux = ', frac*fwf_total, ' m3 s-1 = ', frac*fwf_total*1.e-6_r8, &
         ' Sv'

   end subroutine log_hosing

end module mod_hosing
