! Copyright (C) 2003  P. Wetzel 
! Copyright (C) 2021  K. Assmann, J. Tjiputra, J. Schwinger, T. Torsvik
!
! This file is part of BLOM/iHAMOCC.
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
! along with BLOM. If not, see https://www.gnu.org/licenses/.


      MODULE mo_param1_bgc
!******************************************************************************
!
! MODULE mo_param1_bgc - bgc tracer parameters.
!
!  Patrick Wetzel    *MPI-Met, HH*    01.09.03
!
!
!  Modified
!  --------
!  J.Schwinger,        *NORCE Climate, Bergen*    2020-05-26
!
!  - To facilitate easier use of 'only-lists' in use statements, make indices 
!    always defined also in case they are inside a #ifdef directive.
!
!  T.Torsvik,          *UiB, GFI, Bergen*         2021-10-05
!
!  - Allow tracer indices, other than base, to be set from namelist during run
!    time. Include new subroutine for reading namlist from file.
!
!  Purpose
!  -------
!  - definition of indices in tracer arrays
!
!******************************************************************************
      implicit none

      INTEGER, PARAMETER :: ks=12,ksp=ks+1    ! ks: nb of sediment layers

      REAL,    PARAMETER :: safediv = 1.0e-25 ! added to the denominator of isotopic ratios (avoid div. by zero)

! Tracer indices
      INTEGER, PARAMETER :: i_base=22,                                  &
     &                      isco212  =1,                                &
     &                      ialkali  =2,                                &
     &                      iphosph  =3,                                &
     &                      ioxygen  =4,                                &
     &                      igasnit  =5,                                &
     &                      iano3    =6,                                &
     &                      isilica  =7,                                &
     &                      idoc     =8,                                &
     &                      iphy     =9,                                &
     &                      izoo     =10,                               &
     &                      idet     =11,                               &
     &                      icalc    =12,                               &
     &                      iopal    =13,                               &
     &                      ian2o    =14,                               &
     &                      idms     =15,                               &
     &                      iiron    =16,                               &
     &                      ifdust   =17,                               &
     &                      iprefo2  =18,                               &
     &                      iprefpo4 =19,                               &
     &                      iprefalk =20,                               &
     &                      iprefdic =21,                               &
     &                      idicsat  =22
! C-isotope indices
      INTEGER, PROTECTED :: i_iso=0,                                    &
     &                      isco213  = -1,                              &
     &                      isco214  = -1,                              &
     &                      idoc13   = -1,                              &
     &                      idoc14   = -1,                              &
     &                      iphy13   = -1,                              &
     &                      iphy14   = -1,                              &
     &                      izoo13   = -1,                              &
     &                      izoo14   = -1,                              &
     &                      idet13   = -1,                              &
     &                      idet14   = -1,                              &
     &                      icalc13  = -1,                              &
     &                      icalc14  = -1
! CFC indices
      INTEGER, PROTECTED :: i_cfc=0,                                    &
     &                      icfc11   = -1,                              &
     &                      icfc12   = -1,                              &
     &                      isf6     = -1
! Particle sinking aggregation
      INTEGER, PROTECTED :: i_agg=0,                                    &
     &                      inos     = -1,                              &
     &                      iadust   = -1
! Natural DIC
      INTEGER, PROTECTED :: i_nat_dic=0,                                &
     &                      inatsco212 = -1,                            &
     &                      inatalkali = -1,                            &
     &                      inatcalc   = -1
#ifdef BROMO
      INTEGER, PARAMETER :: i_bromo=1,                                  &
     &                      ibromo=i_base+i_iso+i_cfc+i_agg+i_nat_dic+1
#else
      INTEGER, PARAMETER :: i_bromo=0,                                  &
     &                      ibromo=-1
#endif

! total number of advected tracers
      INTEGER, PROTECTED :: nocetra = -1


! ATMOSPHERE
      INTEGER, PARAMETER :: i_base_atm=5,                               &
     &                      iatmco2=1,                                  &
     &                      iatmo2 =2,                                  &
     &                      iatmn2 =3,                                  &
     &                      iatmn2o=4,                                  &
     &                      iatmdms=5
      INTEGER, PROTECTED :: i_iso_atm = 0,                              &
     &                      iatmc13 = -1,                               &
     &                      iatmc14 = -1
      INTEGER, PROTECTED :: i_cfc_atm = 0,                              &
     &                      iatmf11 = -1,                               &
     &                      iatmf12 = -1,                               &
     &                      iatmsf6 = -1
      INTEGER, PROTECTED :: i_ndic_atm = 0,                             &
     &                      iatmnco2 = -1
#ifdef BROMO
      INTEGER, PARAMETER :: i_bromo_atm=1,                              &
     &                      iatmbromo=i_base_atm+i_iso_atm+i_cfc_atm+   &
     &                                i_ndic_atm+1
#else
      INTEGER, PARAMETER :: i_bromo_atm=0,                              &
     &                      iatmbromo=-1
#endif

! total number of atmosphere tracers
      INTEGER, PROTECTED :: natm = -1


! sediment
      INTEGER, PROTECTED :: nsedtra=4
      INTEGER, PROTECTED :: issso12=1,                                  &
     &                      isssc12=2,                                  &
     &                      issssil=3,                                  &
     &                      issster=4,                                  &
     &                      issso13=-1,                                 &
     &                      issso14=-1,                                 &
     &                      isssc13=-1,                                 &
     &                      isssc14=-1

! pore water tracers, index should be the same as for ocetra
      INTEGER, PROTECTED :: npowtra=7
      INTEGER, PROTECTED :: ipowaic=1,                                  &
     &                      ipowaal=2,                                  &
     &                      ipowaph=3,                                  &
     &                      ipowaox=4,                                  &
     &                      ipown2 =5,                                  &
     &                      ipowno3=6,                                  &
     &                      ipowasi=7,                                  &
     &                      ipowc13=-1,                                 &
     &                      ipowc14=-1

! BGC tracer switches
      logical, protected :: with_cfc = .false.     ! Include CFC tracers
      logical, protected :: with_ciso = .false.    ! Include C-isotopes
      logical, protected :: with_natdic = .false.  ! Include natural DIC tracers
      ! Select particle sinking scheme
      !   0 = constant velocity
      !   1 = linear vertical velocity profile (default)
      !   2 = particle sinking aggregation
      integer, protected :: part_sink_scheme = 1

      namelist /bgctracs/ with_cfc, with_ciso, with_natdic,             &
     &                    part_sink_scheme

    contains

      subroutine rdnml_hamocc_tracers
      !-------------------------------------------------------------------------
      ! Read namelist to determine what hamocc tracers to activate.
      ! Set indices for tracer variables.
      !-------------------------------------------------------------------------
        use mod_xc,         only: mnproc
        use mo_control_bgc, only: bgc_namelist, get_bgc_namelist, io_stdo_bgc

        implicit none

        integer :: iounit

        ! Read namelist variables
        if(.not. allocated(bgc_namelist)) call get_bgc_namelist
        open (newunit=iounit, file=bgc_namelist, status='old', action='read')
        read (unit=iounit, nml=BGCTRACS)
        close (unit=iounit)
        IF (mnproc.eq.1) THEN
           write(io_stdo_bgc,*)
           write(io_stdo_bgc,*) 'iHAMOCC: reading namelist BGCTRACS'
           write(io_stdo_bgc,nml=BGCTRACS)
        ENDIF

        if(with_ciso) then
           ! ocean
           i_iso = 12
           isco213 = i_base+1
           isco214 = i_base+2
           idoc13  = i_base+3
           idoc14  = i_base+4
           iphy13  = i_base+5
           iphy14  = i_base+6
           izoo13  = i_base+7
           izoo14  = i_base+8
           idet13  = i_base+9
           idet14  = i_base+10
           icalc13 = i_base+11
           icalc14 = i_base+12
           ! atmosphere
           i_iso_atm = 2
           iatmc13 = i_base_atm+1
           iatmc14 = i_base_atm+2
           ! sediment
           nsedtra = 8
           issso12 = 1
           isssc12 = 2
           issssil = 3
           issster = 4
           issso13 = 5
           issso14 = 6
           isssc13 = 7
           isssc14 = 8
           ! pore water tracers, index should be the same as for ocetra
           npowtra = 9
           ipowaic = 1
           ipowaal = 2
           ipowaph = 3
           ipowaox = 4
           ipown2  = 5
           ipowno3 = 6
           ipowasi = 7
           ipowc13 = 8  ! C-isotope idices do NOT correspond to ocetra!
           ipowc14 = 9  ! C-isotope idices do NOT correspond to ocetra!
        endif

        if(with_cfc) then
           ! ocean
           i_cfc = 3
           icfc11  = i_base+i_iso+1
           icfc12  = i_base+i_iso+2
           isf6    = i_base+i_iso+3
           ! atmosphere
           i_cfc_atm = 3
           iatmf11 = i_base_atm+i_iso_atm+1
           iatmf12 = i_base_atm+i_iso_atm+2
           iatmsf6 = i_base_atm+i_iso_atm+3
        endif

        if(part_sink_scheme == 2) then
           i_agg = 2
           inos   = i_base+i_iso+i_cfc+1
           iadust = i_base+i_iso+i_cfc+2
        endif

        if(with_natdic) then
           ! ocean
           i_nat_dic = 3
           inatsco212 = i_base+i_iso+i_cfc+i_agg+1
           inatalkali = i_base+i_iso+i_cfc+i_agg+2
           inatcalc   = i_base+i_iso+i_cfc+i_agg+3
           ! atmosphere
           i_ndic_atm = 1
           iatmnco2   = i_base_atm+i_iso_atm+i_cfc_atm+1
        endif

        ! total number of advected tracers
        nocetra = i_base + i_iso + i_cfc + i_agg + i_nat_dic + i_bromo

        ! total number of atmosphere tracers
        natm = i_base_atm + i_iso_atm + i_cfc_atm + i_ndic_atm + i_bromo_atm

      end subroutine rdnml_hamocc_tracers

!******************************************************************************
      END MODULE mo_param1_bgc
