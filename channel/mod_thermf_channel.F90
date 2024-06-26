! ------------------------------------------------------------------------------
! Copyright (C) 2021-2024 Aleksi Nummelin, Mats Bentsen, Mariana Vertenstein
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

module mod_thermf_channel

  use mod_xc
  use mod_types,     only: r8
  use mod_ben02,     only: ntda
  use mod_constants, only: spcifh, t0deg, epsilt, onem
  use mod_time,      only: nday_in_year, nday_of_year, nstep, &
                           nstep_in_day, baclin, &
                           xmi, l1mi, l2mi, l3mi, l4mi, l5mi
  use mod_xc
  use mod_grid,      only: scp2, area
  use mod_state,     only: dp, temp, saln
  use mod_swtfrz,    only: swtfrz
  use mod_ben02,     only: nrfets
  use mod_forcing,   only: sref, tflxap, sflxap, tflxdi, sflxdi, &
                           nflxdi, aptflx, apsflx, ditflx, disflx, &
                           sstclm, ricclm, sssclm, trxday, srxday, &
                           trxdpt, srxdpt, trxlim, srxlim, srxbal, &
                           swa, nsf, hmltfz, lip, sop, eva, rnf, rfi, &
                           fmltfz, sfl, ustarw, surflx, surrlx, &
                           sswflx, salflx, brnflx, salrlx, ustar
  use mod_utility,   only: util1, util2, util3, util4
  use mod_checksum,  only: csdiag, chksummsk
  use mod_tracers,   only: ntr, itrtke, itrgls, trc, trflx
  use mod_diffusion, only: difdia
  use mod_tke,       only: gls_cmu0, zos, gls_p, gls_m, gls_n, vonKar
  use mod_intp1d,    only: intp1d
  use mod_ifdefs,    only: use_TRC, use_TKE, use_GLS

  implicit none
  private

  public :: thermf_channel

contains

  subroutine thermf_channel(m,n,mm,nn,k1m,k1n)
    !
    ! thermf for the channel config
    !
    integer, intent(in) :: m,n,mm,nn,k1m,k1n
    !
    real(r8), dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) :: vrtsfl
    integer  :: i,j,k,l,m1,m2,m3,m4,m5
    real(r8) :: dt,cpsw,rnf_fac,y,dpotl,hotl,totl,sotl
    real(r8) :: dpmxl,hmxl,tmxl,smxl,q,fwflx,sstc,rice
    real(r8) :: trxflx,sssc,srxflx,totsfl,totwfl,sflxc,totsrp,totsrn
    integer  :: nt
    real(r8), dimension(ntr,1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) :: ttrsf
    real(r8), dimension(ntr,1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) :: ttrav
    real(r8) :: trflxc
    !
    ! Set various constants
    dt = baclin                         ! Time step
    cpsw = spcifh*1.e3_r8               ! Specific heat of seawater
    rnf_fac = baclin/real(nrfets*86400) ! Runoff reservoar detrainment rate
    !
    ! Set parameters for time interpolation when applying diagnosed heat
    ! and salt relaxation fluxes
    y = (nday_of_year-1+mod(nstep,nstep_in_day)/real(nstep_in_day))*48. &
         /real(nday_in_year)
    m3 = int(y)+1
    y = y-real(m3-1)
    m1 = mod(m3+45,48)+1
    m2 = mod(m3+46,48)+1
    m4 = mod(m3   ,48)+1
    m5 = mod(m3+ 1,48)+1

    ! Time level for diagnosing heat and salt relaxation fluxes
    k = m3

    if (ditflx.or.disflx) nflxdi(k) = nflxdi(k)+1

    !$omp parallel do private( &
    !$omp l,i,dpotl,hotl,totl,sotl,dpmxl,hmxl,tmxl,smxl,fwflx,sstc,rice, &
    !$omp trxflx,sssc,srxflx,nt) shared(xmi,y)
    do j = 1,jj
      do l = 1,isp(j)
        do i = max(1,ifp(j,l)),min(ii,ilp(j,l))

          ! Initialize variables describing the state of the ocean top
          ! layer, the mixed layer and ice/snow fraction
          dpotl = dp(i,j,k1n)
          hotl = dpotl/onem
          totl = temp(i,j,k1n)+t0deg
          sotl = saln(i,j,k1n)

          dpmxl = dp(i,j,1+nn)+dp(i,j,2+nn)
          hmxl = dpmxl/onem
          tmxl = (temp(i,j,1+nn)*dp(i,j,1+nn) &
                 +temp(i,j,2+nn)*dp(i,j,2+nn))/dpmxl+t0deg
          smxl = (saln(i,j,1+nn)*dp(i,j,1+nn) &
                 +saln(i,j,2+nn)*dp(i,j,2+nn))/dpmxl


          ! Accumulate the runoff in a reservoar to delay the discharge
          ! into the ocean (by nrfets days approximately 1/e of runoff
          ! added will by discharged).
          !       rnfres(i,j)=rnfres(i,j)+rnf(i,j)
          !       rnfflx(i,j)=rnfres(i,j)*rnf_fac
          !       rnfres(i,j)=rnfres(i,j)*(1.-rnf_fac)
          !       rfiflx(i,j)=rfi(i,j)

          ! Fresh water flux due to melting/freezing [kg m-2 s-1]
          ! (positive downwards)
          fmltfz(i,j) = 0._r8 !-(dvi*rhoice+dvs*rhosnw)/dt

          ! Fresh water flux [kg m-2 s-1] (positive downwards)
          fwflx = eva(i,j)+lip(i,j)+sop(i,j)+rnf(i,j)+rfi(i,j)+fmltfz(i,j)

          ! Salt flux [kg m-2 s-1] (positive downwards)
          sfl(i,j) = 0._r8 !-sice*dvi*rhoice/dt*1.e-3

          ! Salt flux due to brine rejection of freezing sea
          ! ice [kg m-2 m-1] (positive downwards)
          brnflx(i,j) = 0._r8 !max(0.,-sotl*fmltfz(i,j)*1.e-3+sfl(i,j))

          ! Virtual salt flux [kg m-2 s-1] (positive downwards)
          vrtsfl(i,j) = -sotl*fwflx*1.e-3

          ! Store area weighted virtual salt flux and fresh water flux
          util1(i,j) = vrtsfl(i,j)*scp2(i,j)
          util2(i,j) = fwflx*scp2(i,j)

          ! Heat flux due to melting/freezing [W m-2] (positive downwards)
          hmltfz(i,j) = 0._r8 !(dvi*fusi+dvs*fuss)/dt

          ! Total heat flux in MICOM units [W cm-2] (positive upwards)
          surflx(i,j) = -(swa(i,j)+nsf(i,j)+hmltfz(i,j))*1.e-4_r8

          ! Short-wave heat flux in MICOM units [W cm-2] (positive
          ! upwards)
          sswflx(i,j) = 0._r8 !-qsww*(1.-fice0)*1.e-4

          !---------------------------------------------------------------
          ! Tracer fluxes (positive downwards)
          !---------------------------------------------------------------

          if (use_TRC) then
            do nt = 1,ntr
              if (use_TKE) then
                if (nt == itrtke) then
                  trflx(nt,i,j) = 0._r8
                  ttrsf(nt,i,j) = 0._r8
                  ttrav(nt,i,j) = 0._r8
                  cycle
                end if
                if (use_GLS) then
                  if (nt == itrgls) then
                    trflx(nt,i,j) = -gls_n*difdia(i,j,1)*(gls_cmu0**gls_p) &
                         *(trc(i,j,k1n,itrtke)**gls_m) &
                         *(vonKar**gls_n)*Zos**(gls_n-1.)
                    ttrsf(nt,i,j) = 0._r8
                    ttrav(nt,i,j) = 0._r8
                    cycle
                  end if
                else
                  if (nt == itrgls) then
                    trflx(nt,i,j) = 0._r8
                    ttrsf(nt,i,j) = 0._r8
                    ttrav(nt,i,j) = 0._r8
                    cycle
                  end if
                end if
              end if
              trflx(nt,i,j) = -trc(i,j,k1n,nt)*fwflx*1.e-3
              ttrsf(nt,i,j) = trflx(nt,i,j)*scp2(i,j)
              ttrav(nt,i,j) = trc(i,j,k1n,nt)*scp2(i,j)
            end do
          end if

          !---------------------------------------------------------------
          ! Relaxation fluxes
          !---------------------------------------------------------------

          surrlx(i,j) = 0._r8

          ! If  trxday>0 , apply relaxation towards observed sst
          if (trxday > epsilt) then
            sstc = intp1d(sstclm(i,j,l1mi),sstclm(i,j,l2mi), &
                          sstclm(i,j,l3mi),sstclm(i,j,l4mi), &
                          sstclm(i,j,l5mi),xmi)
            rice = intp1d(ricclm(i,j,l1mi),ricclm(i,j,l2mi), &
                          ricclm(i,j,l3mi),ricclm(i,j,l4mi), &
                          ricclm(i,j,l5mi),xmi)
            sstc = (1._r8-rice)*sstc !max(sstc,tice_f)+rice*tice_f
            trxflx = spcifh*100._r8*min(hmxl,trxdpt)/(trxday*86400.) &
                                   *min(trxlim,max(-trxlim,sstc-tmxl))
            surrlx(i,j) = -trxflx
          else
            trxflx = 0._r8
          end if

          ! If aptflx=.true., apply diagnosed relaxation flux
          if (aptflx) then
            surrlx(i,j) = surrlx(i,j) &
                 -intp1d(tflxap(i,j,m1),tflxap(i,j,m2),tflxap(i,j,m3), &
                 tflxap(i,j,m4),tflxap(i,j,m5),y)
          end if

          ! If ditflx=.true., diagnose relaxation flux by accumulating the
          ! relaxation flux
          if (ditflx) then
            tflxdi(i,j,k) = tflxdi(i,j,k)+trxflx
          end if

          salrlx(i,j) = 0._r8

          ! if  srxday>0 , apply relaxation towards observed sss
          if (srxday > epsilt) then
            sssc = intp1d(sssclm(i,j,l1mi),sssclm(i,j,l2mi), &
                 sssclm(i,j,l3mi),sssclm(i,j,l4mi), &
                 sssclm(i,j,l5mi),xmi)
            srxflx = 100._r8*min(hmxl,srxdpt)/(srxday*86400.) &
                            *min(srxlim,max(-srxlim,sssc-smxl))
            salrlx(i,j) = -srxflx
            util3(i,j) = max(0._r8,salrlx(i,j))*scp2(i,j)
            util4(i,j) = min(0._r8,salrlx(i,j))*scp2(i,j)
          else
            srxflx = 0.
          end if

          ! If apsflx=.true., apply diagnosed relaxation flux
          if (apsflx) then
            salrlx(i,j) = salrlx(i,j) &
                 -intp1d(sflxap(i,j,m1),sflxap(i,j,m2),sflxap(i,j,m3), &
                         sflxap(i,j,m4),sflxap(i,j,m5),y)
          end if

          ! If disflx=.true., diagnose relaxation flux by accumulating the
          ! relaxation flux
          if (disflx) then
            sflxdi(i,j,k) = sflxdi(i,j,k)+srxflx
          end if

          !----------------------------------------------------------------
          ! Compute friction velocity (cm/s)
          !----------------------------------------------------------------

          ustar(i,j) = ustarw(i,j)*1.e2_r8
          !  ustar(i,j)=(min(ustari(i,j),.8e-2)*fice0 &
          !                 +ustarw(i,j)*(1.-fice0))*1.e2

        end do
      end do
    end do
    !$omp end parallel do

    ! ------------------------------------------------------------------
    ! Compute correction to the virtual salt flux so it is globally
    ! consistent with a salt flux based on some reference salinity.
    ! Also combine virtual and true salt flux and convert salt fluxes
    ! used later to unit [10e-3 g cm-2 s-1] and positive upwards.
    ! ------------------------------------------------------------------

    call xcsum(totsfl,util1,ips)
    call xcsum(totwfl,util2,ips)

    ! Correction for the virtual salt flux [kg m-2 s-1]
    sflxc = (-sref*totwfl*1.e-3_r8-totsfl)/area
    if (mnproc == 1) then
      write (lp,*) 'thermf: totsfl/area,sflxc',totsfl/area,sflxc
    end if

    ! Apply the virtual salt flux correction and the compute the total
    ! salt flux by combining the virtual and true salt flux
    !$omp parallel do private(l,i)
    do j = 1,jj
      do l = 1,isp(j)
        do i = max(1,ifp(j,l)),min(ii,ilp(j,l))
          salflx(i,j) = -(vrtsfl(i,j)+sflxc+sfl(i,j))*1.e2_r8
          brnflx(i,j) = -brnflx(i,j)*1.e2_r8
        end do
      end do
    end do
    !$omp end parallel do

    ! if  srxday>0  and  srxbal=.true. , balance the sss relaxation flux
    ! so the net input of salt in grid cells connected to the world
    ! ocean is zero
    if (srxday > epsilt.and.srxbal) then
      call xcsum(totsrp,util3,ipwocn)
      call xcsum(totsrn,util4,ipwocn)
      if (abs(totsrp) > abs(totsrn)) then
        q = -totsrn/totsrp
        !$omp parallel do private(l,i)
        do j = 1,jj
          do l = 1,isp(j)
            do i = max(1,ifp(j,l)),min(ii,ilp(j,l))
              if (salrlx(i,j) > 0._r8.and.ipwocn(i,j) == 1) then
                salrlx(i,j) = q*salrlx(i,j)
              end if
            end do
          end do
        end do
        !$omp end parallel do
      else
        q = -totsrp/totsrn
        !$omp parallel do private(l,i)
        do j = 1,jj
          do l = 1,isp(j)
            do i = max(1,ifp(j,l)),min(ii,ilp(j,l))
              if (salrlx(i,j) < 0._r8.and.ipwocn(i,j) == 1) then
                salrlx(i,j) = q*salrlx(i,j)
              end if
            end do
          end do
        end do
        !$omp end parallel do
      end if
    end if

    if (use_TRC) then
      do nt = 1,ntr
        if (use_TKE) then
          if (nt == itrtke.or.nt == itrgls) cycle
        end if
        trflxc = 0.
        !$omp parallel do private(l,i)
        do j = 1,jj
          do l = 1,isp(j)
            do i = max(1,ifp(j,l)),min(ii,ilp(j,l))
              trflx(nt,i,j) = -(trflx(nt,i,j)+trflxc)*1.e2
            end do
          end do
        end do
        !$omp end parallel do
      end do
    end if

    ! ------------------------------------------------------------------
    ! number of accumulated fields for flux calculations
    ! ------------------------------------------------------------------

    ntda = ntda+1

    if (csdiag) then
      if (mnproc == 1) then
        write (lp,*) 'thermf_channel:'
      end if
      ! call chksummsk(rnfres,ip,1,'rnfres')
      call chksummsk(surflx,ip,1,'surflx')
      call chksummsk(sswflx,ip,1,'sswflx')
      call chksummsk(salflx,ip,1,'salflx')
      call chksummsk(surrlx,ip,1,'surrlx')
      call chksummsk(salrlx,ip,1,'salrlx')
      call chksummsk(ustar,ip,1,'ustar')
    end if

  end subroutine thermf_channel

end module mod_thermf_channel
