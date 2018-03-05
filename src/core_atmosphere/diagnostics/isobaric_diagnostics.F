! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module isobaric_diagnostics

    use mpas_dmpar
    use mpas_kind_types
    use mpas_derived_types
    use mpas_pool_routines
    use mpas_constants

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag

    type (MPAS_clock_type), pointer :: clock

    public :: isobaric_diagnostics_setup, &
              isobaric_diagnostics_compute

    private

    logical :: need_mslp, &
               need_relhum_200, need_relhum_250, need_relhum_500, need_relhum_700, need_relhum_850, need_relhum_925, need_relhum_1000, &
               need_dewpoint_200, need_dewpoint_250, need_dewpoint_500, need_dewpoint_700, need_dewpoint_850, need_dewpoint_925, need_dewpoint_1000, &
               need_temp_200, need_temp_250, need_temp_500, need_temp_700, need_temp_850, need_temp_925, need_temp_1000, &
               need_height_200, need_height_250, need_height_500, need_height_700, need_height_850, need_height_925, need_height_1000, &
               need_uzonal_200, need_uzonal_250, need_uzonal_500, need_uzonal_700, need_uzonal_850, need_uzonal_925, need_uzonal_1000, &
               need_umeridional_200, need_umeridional_250, need_umeridional_500, need_umeridional_700, need_umeridional_850, need_umeridional_925, need_umeridional_1000, &
               need_w_200, need_w_250, need_w_500, need_w_700, need_w_850, need_w_925, need_w_1000, &
               need_vorticity_200, need_vorticity_250, need_vorticity_500, need_vorticity_700, need_vorticity_850, need_vorticity_925, need_vorticity_1000, &
               need_t_isobaric, need_z_isobaric, need_meanT_500_300
    logical :: need_temp, need_relhum, need_dewpoint, need_w, need_uzonal, need_umeridional, need_vorticity, need_height


    contains


    !-----------------------------------------------------------------------
    !  routine isobaric_diagnostics_setup
    !
    !> \brief Set up the isobaric diagnostics module
    !> \author Michael Duda
    !> \date   21 October 2016
    !> \details
    !>  This routine sets up the isobaric diagnostics module, principally by
    !>  saving pointers to pools that are used in the computation of diagnostics.
    !
    !-----------------------------------------------------------------------
    subroutine isobaric_diagnostics_setup(all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        clock => simulation_clock

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
   
    end subroutine isobaric_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine isobaric_diagnostics_compute
    !
    !> \brief Compute isobaric diagnostic before model output is written
    !> \author Michael Duda
    !> \date   21 October 2016
    !> \details
    !>  Compute isobaric diagnostic before model output is written. Code called
    !>  from here was previously in mpas_atm_interp_diagnostics.F.
    !
    !-----------------------------------------------------------------------
    subroutine isobaric_diagnostics_compute()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        logical :: need_any_diags

        need_any_diags = .false.

        need_temp = .false.
        need_dewpoint = .false.
        need_relhum = .false.
        need_w = .false.
        need_uzonal = .false.
        need_umeridional = .false.
        need_vorticity = .false.
        need_height = .false.

        need_mslp = MPAS_field_will_be_written('mslp')
        need_any_diags = need_any_diags .or. need_mslp
        need_relhum_200 = MPAS_field_will_be_written('relhum_200hPa')
        need_relhum = need_relhum .or. need_relhum_200
        need_any_diags = need_any_diags .or. need_relhum_200
        need_relhum_250 = MPAS_field_will_be_written('relhum_250hPa')
        need_relhum = need_relhum .or. need_relhum_250
        need_any_diags = need_any_diags .or. need_relhum_250
        need_relhum_500 = MPAS_field_will_be_written('relhum_500hPa')
        need_relhum = need_relhum .or. need_relhum_500
        need_any_diags = need_any_diags .or. need_relhum_500
        need_relhum_700 = MPAS_field_will_be_written('relhum_700hPa')
        need_relhum = need_relhum .or. need_relhum_700
        need_any_diags = need_any_diags .or. need_relhum_700
        need_relhum_850 = MPAS_field_will_be_written('relhum_850hPa')
        need_relhum = need_relhum .or. need_relhum_850
        need_any_diags = need_any_diags .or. need_relhum_850
        need_relhum_925 = MPAS_field_will_be_written('relhum_925hPa')
        need_relhum = need_relhum .or. need_relhum_925
        need_any_diags = need_any_diags .or. need_relhum_925
        need_relhum_1000 = MPAS_field_will_be_written('relhum_1000hPa')
        need_relhum = need_relhum .or. need_relhum_1000
        need_any_diags = need_any_diags .or. need_relhum_1000
        need_dewpoint_200 = MPAS_field_will_be_written('dewpoint_200hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_200
        need_any_diags = need_any_diags .or. need_dewpoint_200
        need_dewpoint_250 = MPAS_field_will_be_written('dewpoint_250hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_250
        need_any_diags = need_any_diags .or. need_dewpoint_250
        need_dewpoint_500 = MPAS_field_will_be_written('dewpoint_500hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_500
        need_any_diags = need_any_diags .or. need_dewpoint_500
        need_dewpoint_700 = MPAS_field_will_be_written('dewpoint_700hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_700
        need_any_diags = need_any_diags .or. need_dewpoint_700
        need_dewpoint_850 = MPAS_field_will_be_written('dewpoint_850hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_850
        need_any_diags = need_any_diags .or. need_dewpoint_850
        need_dewpoint_925 = MPAS_field_will_be_written('dewpoint_925hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_925
        need_any_diags = need_any_diags .or. need_dewpoint_925
        need_dewpoint_1000 = MPAS_field_will_be_written('dewpoint_1000hPa')
        need_dewpoint = need_dewpoint .or. need_dewpoint_1000
        need_any_diags = need_any_diags .or. need_dewpoint_1000
        need_temp_200 = MPAS_field_will_be_written('temperature_200hPa')
        need_temp = need_temp .or. need_temp_200
        need_any_diags = need_any_diags .or. need_temp_200
        need_temp_250 = MPAS_field_will_be_written('temperature_250hPa')
        need_temp = need_temp .or. need_temp_250
        need_any_diags = need_any_diags .or. need_temp_250
        need_temp_500 = MPAS_field_will_be_written('temperature_500hPa')
        need_temp = need_temp .or. need_temp_500
        need_any_diags = need_any_diags .or. need_temp_500
        need_temp_700 = MPAS_field_will_be_written('temperature_700hPa')
        need_temp = need_temp .or. need_temp_700
        need_any_diags = need_any_diags .or. need_temp_700
        need_temp_850 = MPAS_field_will_be_written('temperature_850hPa')
        need_temp = need_temp .or. need_temp_850
        need_any_diags = need_any_diags .or. need_temp_850
        need_temp_925 = MPAS_field_will_be_written('temperature_925hPa')
        need_temp = need_temp .or. need_temp_925
        need_any_diags = need_any_diags .or. need_temp_925
        need_temp_1000 = MPAS_field_will_be_written('temperature_1000hPa')
        need_temp = need_temp .or. need_temp_1000
        need_any_diags = need_any_diags .or. need_temp_1000
        need_height_200 = MPAS_field_will_be_written('height_200hPa')
        need_height = need_height .or. need_height_200
        need_any_diags = need_any_diags .or. need_height_200
        need_height_250 = MPAS_field_will_be_written('height_250hPa')
        need_height = need_height .or. need_height_250
        need_any_diags = need_any_diags .or. need_height_250
        need_height_500 = MPAS_field_will_be_written('height_500hPa')
        need_height = need_height .or. need_height_500
        need_any_diags = need_any_diags .or. need_height_500
        need_height_700 = MPAS_field_will_be_written('height_700hPa')
        need_height = need_height .or. need_height_700
        need_any_diags = need_any_diags .or. need_height_700
        need_height_850 = MPAS_field_will_be_written('height_850hPa')
        need_height = need_height .or. need_height_850
        need_any_diags = need_any_diags .or. need_height_850
        need_height_925 = MPAS_field_will_be_written('height_925hPa')
        need_height = need_height .or. need_height_925
        need_any_diags = need_any_diags .or. need_height_925
        need_height_1000 = MPAS_field_will_be_written('height_1000hPa')
        need_height = need_height .or. need_height_1000
        need_any_diags = need_any_diags .or. need_height_1000
        need_uzonal_200 = MPAS_field_will_be_written('uzonal_200hPa')
        need_uzonal = need_uzonal .or. need_uzonal_200
        need_any_diags = need_any_diags .or. need_uzonal_200
        need_uzonal_250 = MPAS_field_will_be_written('uzonal_250hPa')
        need_uzonal = need_uzonal .or. need_uzonal_250
        need_any_diags = need_any_diags .or. need_uzonal_250
        need_uzonal_500 = MPAS_field_will_be_written('uzonal_500hPa')
        need_uzonal = need_uzonal .or. need_uzonal_500
        need_any_diags = need_any_diags .or. need_uzonal_500
        need_uzonal_700 = MPAS_field_will_be_written('uzonal_700hPa')
        need_uzonal = need_uzonal .or. need_uzonal_700
        need_any_diags = need_any_diags .or. need_uzonal_700
        need_uzonal_850 = MPAS_field_will_be_written('uzonal_850hPa')
        need_uzonal = need_uzonal .or. need_uzonal_850
        need_any_diags = need_any_diags .or. need_uzonal_850
        need_uzonal_925 = MPAS_field_will_be_written('uzonal_925hPa')
        need_uzonal = need_uzonal .or. need_uzonal_925
        need_any_diags = need_any_diags .or. need_uzonal_925
        need_uzonal_1000 = MPAS_field_will_be_written('uzonal_1000hPa')
        need_uzonal = need_uzonal .or. need_uzonal_1000
        need_any_diags = need_any_diags .or. need_uzonal_1000
        need_umeridional_200 = MPAS_field_will_be_written('umeridional_200hPa')
        need_umeridional = need_umeridional .or. need_umeridional_200
        need_any_diags = need_any_diags .or. need_umeridional_200
        need_umeridional_250 = MPAS_field_will_be_written('umeridional_250hPa')
        need_umeridional = need_umeridional .or. need_umeridional_250
        need_any_diags = need_any_diags .or. need_umeridional_250
        need_umeridional_500 = MPAS_field_will_be_written('umeridional_500hPa')
        need_umeridional = need_umeridional .or. need_umeridional_500
        need_any_diags = need_any_diags .or. need_umeridional_500
        need_umeridional_700 = MPAS_field_will_be_written('umeridional_700hPa')
        need_umeridional = need_umeridional .or. need_umeridional_700
        need_any_diags = need_any_diags .or. need_umeridional_700
        need_umeridional_850 = MPAS_field_will_be_written('umeridional_850hPa')
        need_umeridional = need_umeridional .or. need_umeridional_850
        need_any_diags = need_any_diags .or. need_umeridional_850
        need_umeridional_925 = MPAS_field_will_be_written('umeridional_925hPa')
        need_umeridional = need_umeridional .or. need_umeridional_925
        need_any_diags = need_any_diags .or. need_umeridional_925
        need_umeridional_1000 = MPAS_field_will_be_written('umeridional_1000hPa')
        need_umeridional = need_umeridional .or. need_umeridional_1000
        need_any_diags = need_any_diags .or. need_umeridional_1000
        need_w_200 = MPAS_field_will_be_written('w_200hPa')
        need_w = need_w .or. need_w_200
        need_any_diags = need_any_diags .or. need_w_200
        need_w_250 = MPAS_field_will_be_written('w_250hPa')
        need_w = need_w .or. need_w_250
        need_any_diags = need_any_diags .or. need_w_250
        need_w_500 = MPAS_field_will_be_written('w_500hPa')
        need_w = need_w .or. need_w_500
        need_any_diags = need_any_diags .or. need_w_500
        need_w_700 = MPAS_field_will_be_written('w_700hPa')
        need_w = need_w .or. need_w_700
        need_any_diags = need_any_diags .or. need_w_700
        need_w_850 = MPAS_field_will_be_written('w_850hPa')
        need_w = need_w .or. need_w_850
        need_any_diags = need_any_diags .or. need_w_850
        need_w_925 = MPAS_field_will_be_written('w_925hPa')
        need_w = need_w .or. need_w_925
        need_any_diags = need_any_diags .or. need_w_925
        need_w_1000 = MPAS_field_will_be_written('w_1000hPa')
        need_w = need_w .or. need_w_1000
        need_any_diags = need_any_diags .or. need_w_1000
        need_vorticity_200 = MPAS_field_will_be_written('vorticity_200hPa')
        need_vorticity = need_vorticity .or. need_vorticity_200
        need_any_diags = need_any_diags .or. need_vorticity_200
        need_vorticity_250 = MPAS_field_will_be_written('vorticity_250hPa')
        need_vorticity = need_vorticity .or. need_vorticity_250
        need_any_diags = need_any_diags .or. need_vorticity_250
        need_vorticity_500 = MPAS_field_will_be_written('vorticity_500hPa')
        need_vorticity = need_vorticity .or. need_vorticity_500
        need_any_diags = need_any_diags .or. need_vorticity_500
        need_vorticity_700 = MPAS_field_will_be_written('vorticity_700hPa')
        need_vorticity = need_vorticity .or. need_vorticity_700
        need_any_diags = need_any_diags .or. need_vorticity_700
        need_vorticity_850 = MPAS_field_will_be_written('vorticity_850hPa')
        need_vorticity = need_vorticity .or. need_vorticity_850
        need_any_diags = need_any_diags .or. need_vorticity_850
        need_vorticity_925 = MPAS_field_will_be_written('vorticity_925hPa')
        need_vorticity = need_vorticity .or. need_vorticity_925
        need_any_diags = need_any_diags .or. need_vorticity_925
        need_vorticity_1000 = MPAS_field_will_be_written('vorticity_1000hPa')
        need_vorticity = need_vorticity .or. need_vorticity_1000
        need_any_diags = need_any_diags .or. need_vorticity_1000
        need_t_isobaric = MPAS_field_will_be_written('t_isobaric')
        need_any_diags = need_any_diags .or. need_t_isobaric
        need_z_isobaric = MPAS_field_will_be_written('z_isobaric')
        need_any_diags = need_any_diags .or. need_z_isobaric
        need_meanT_500_300 = MPAS_field_will_be_written('meanT_500_300')
        need_any_diags = need_any_diags .or. need_meanT_500_300

        if (need_any_diags) then
            call interp_diagnostics(1)
        end if
   
    end subroutine isobaric_diagnostics_compute


   !==================================================================================================
    subroutine interp_diagnostics(time_lev)
   !==================================================================================================

       !input arguments:
        integer, intent(in) :: time_lev              ! which time level to use from state
       
       !local variables:
        integer :: iCell
        integer, pointer :: nCells, nVertLevels, nVertices
        integer, pointer :: index_qv, num_scalars

        type (field2DReal), pointer:: pressure_p_field
        
        real (kind=RKIND), dimension(:,:), pointer :: exner, height
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p 
        real (kind=RKIND), dimension(:,:), pointer :: relhum, theta_m
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
       
        real (kind=RKIND), dimension(:), pointer :: t_iso_levels
        real (kind=RKIND), dimension(:), pointer :: z_iso_levels
        real (kind=RKIND), dimension(:,:), pointer :: t_isobaric
        real (kind=RKIND), dimension(:,:), pointer :: z_isobaric
        real (kind=RKIND), dimension(:), pointer :: meanT_500_300
       
        real (kind=RKIND) :: evp
       
       !--------------------
       
        real (kind=RKIND), dimension(:), pointer :: mslp
       
        real (kind=RKIND), dimension(:,:), allocatable :: pressure, pressure2, pressure_v, temperature
        real (kind=RKIND), dimension(:,:), allocatable :: dewpoint

        real (kind=RKIND), dimension(7) :: pOut
        
       !--------------------------------------------------------------------------------------------------
       
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)

        call mpas_pool_get_array(mesh, 'zgrid', height)
        call mpas_pool_get_array(state, 'theta_m', theta_m, time_lev)
        call mpas_pool_get_array(state, 'scalars', scalars, time_lev)

        call mpas_pool_get_field(diag, 'pressure_p', pressure_p_field)
        call mpas_dmpar_exch_halo_field(pressure_p_field)
       
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)
       
        ! calculation of total pressure at cell centers (at mass points):
        allocate(pressure(nVertLevels,nCells))
        pressure(:,:) = (pressure_p(:,:) + pressure_b(:,:)) / 100._RKIND

        ! Compute temperature from theta
        if (need_temp .or. need_mslp .or. need_t_isobaric) then
            allocate(temperature(nVertLevels,nCells))
            temperature(:,:) = (theta_m(:,:)/(1._RKIND+rvord*scalars(index_qv,:,:)))*exner(:,:)
        end if

        ! Compute dewpoint
        if ( need_dewpoint .or. need_mslp) then
            allocate(dewpoint(nVertLevels,nCells))
            ! calculation of temperature at cell centers:
            do iCell = 1,nCells
            do k = 1,nVertLevels
                ! Vapor pressure (NB: pressure here is already in hPa)
                evp = pressure(k,iCell) * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
                evp = max(evp, 1.0e-8_RKIND)

                ! Dewpoint temperature following Bolton (1980)
                dewpoint(k,iCell) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
                dewpoint(k,iCell) = dewpoint(k,iCell) + 273.15
            enddo
            enddo
        end if

        ! Compute staggered pressure, if needed
        if (need_height .or. need_w .or. need_z_isobaric) then
            allocate(pressure2(nVertLevels+1,nCells))
            call stagger_pressure(pressure, pressure2, height, nVertLevels, nCells)
        end if

        ! Compute pressure at vertices, if needed
        if (need_vorticity) then
            allocate(pressure_v(nVertLevels,nVertices))
            call vertex_pressure(pressure, pressure_p, pressure_b, pressure_v, nVertLevels, nCells, nVertices)
        end if


        ! Target pressure levels - must be decreasing
        pOut(1:7) = (/ 1000.0, 925.0, 850.0, 700.0, 500.0, 250.0, 200.0 /)

        ! interpolation for fields at cell centers and mass levels
        call interp_cell_center_diagnostics(pressure, temperature, dewpoint, pOut, 7, nCells, nVertLevels)
       
        ! interpolation for fields at cell centers and w levels
        call interp_cell_staggered_diagnostics(pressure2, height, pOut, 7, nCells, nVertLevels+1)

        ! interpolation for fields at vertices and mass levels
        call interp_vertex_center_diagnostics(pressure_v, pOut, 7, nVertices, nVertLevels)

        !!!!!!!!!!! Calculate MSLP  !!!!!!!!!!!
        if (need_mslp) then
            call mpas_pool_get_array(diag, 'mslp', mslp)
            !... compute SLP (requires temp, height, pressure, qvapor)
             call compute_slp(nCells, nVertLevels, num_scalars, temperature, height, pressure, index_qv, scalars, mslp)
             mslp(:) = mslp(:) * 100.0   ! Convert from hPa to Pa
            !... alternative way
            !do iCell = 1, nCells
            !   mslp(iCell) = diag % surface_pressure % array(iCell) + 11.38*height(1,iCell)
            !   mslp(iCell) = mslp(iCell)/100.
            !enddo
        end if
    
    
        !!!!!!!!!!! Calculate mean temperature in 500 hPa - 300 hPa layer  !!!!!!!!!!!
        if (need_meanT_500_300) then
            call mpas_pool_get_array(diag, 'meanT_500_300', meanT_500_300)
            do iCell=1,nCells
                meanT_500_300(iCell) = compute_layer_mean(500.0, 300.0, temperature(:,iCell), pressure(:,iCell), nVertLevels)
            end do
        end if

        !!!!!!!!!!! Additional temperature levels for vortex tracking !!!!!!!!!!!
        if (need_t_isobaric) then
            call mpas_pool_get_array(diag, 't_iso_levels', t_iso_levels)
            call mpas_pool_get_array(diag, 't_isobaric', t_isobaric)
            t_iso_levels(1:5) = (/ 500., 450., 400., 350., 300. /)
            do iCell=1,nCells
                t_isobaric(1:5,iCell) = interp_to_p(temperature(:,iCell), pressure(:,iCell), nVertLevels, t_iso_levels, 5)
            end do
            t_iso_levels(:) = t_iso_levels(:) * 100.
        end if
     
        !!!!!!!!!!! Additional height levels for vortex tracking !!!!!!!!!!!
        if (need_z_isobaric) then
            call mpas_pool_get_array(diag, 'z_iso_levels', z_iso_levels)
            call mpas_pool_get_array(diag, 'z_isobaric', z_isobaric)
            z_iso_levels(1:13) = (/ 900., 850., 800., 750., 700., 650., 600., 550., 500., 450., 400., 350., 300. /)
            do iCell=1,nCells
                z_isobaric(1:13,iCell) = interp_to_p(height(:,iCell), pressure2(:,iCell), nVertLevels+1, z_iso_levels, 13)
            end do
            z_iso_levels(:) = z_iso_levels(:) * 100.
        end if
    
        if(allocated(temperature) ) deallocate(temperature )
        if(allocated(pressure)    ) deallocate(pressure    )
        if(allocated(dewpoint)    ) deallocate(dewpoint    )

        if(allocated(pressure2)   ) deallocate(pressure2   )
        if(allocated(pressure_v)  ) deallocate(pressure_v  )

       !formats:
       ! 201 format(i5,4(1x,e15.8))
   
    end subroutine interp_diagnostics


    subroutine compute_slp(ncol,nlev_in,nscalars,t,height,p,index_qv,scalars,slp)
   
       implicit none
   
      !input arguments:
       integer, intent(in) :: ncol, nlev_in, nscalars
      
      !p: in mb
      !t: in K
      !scalars: in kg/kg
      !height: in m
       real(kind=RKIND), intent(in), dimension(nlev_in,ncol) :: p,t
       real(kind=RKIND), intent(in), dimension(nlev_in+1,ncol) :: height
       integer, intent(in) :: index_qv
       real(kind=RKIND), intent(in), dimension(nscalars,nlev_in,ncol) :: scalars
      
      !output arguments:
       real(kind=RKIND), intent(out), dimension(ncol) :: slp
      
      !local variables:
       integer :: icol, k, kcount
       integer :: klo, khi
      
       real(kind=RKIND) :: gamma, rr, grav
       parameter (rr=287.0, grav=9.80616, gamma=0.0065)
      
       real(kind=RKIND) :: tc, pconst
       parameter (tc=273.16+17.5, pconst=100.)
      
       logical mm5_test
       parameter (mm5_test=.true.)
      
       integer, dimension(:), allocatable :: level
       real(kind=RKIND), dimension(:), allocatable :: t_surf, t_msl
       real(kind=RKIND) :: plo , phi , tlo, thi , zlo , zhi
       real(kind=RKIND) :: p_at_pconst , t_at_pconst , z_at_pconst, z_half_lowest
      
       logical :: l1, l2, l3, found
      
      ! Find least zeta level that is PCONST Pa above the surface.  We later use this
      ! level to extrapolate a surface pressure and temperature, which is supposed
      ! to reduce the effect of the diurnal heating cycle in the pressure field.
      
       if (.not.allocated(level))  allocate(level(ncol))
       if (.not.allocated(t_surf)) allocate(t_surf(ncol))
       if (.not.allocated(t_msl))  allocate(t_msl(ncol))
      
       do icol = 1 , ncol
          level(icol) = -1
      
          k = 1
          found = .false.
          do while ( (.not. found) .and. (k.le.nlev_in))
                if ( p(k,icol) .lt. p(1,icol)-pconst ) then
                   level(icol) = k
                   found = .true.
                end if
                k = k+1
          end do
      
          if ( level(icol) .eq. -1 ) then
             write(0,*) 'Troubles finding level ', pconst,' above ground.'
             write(0,*) 'Problems first occur at (',icol,')'
             write(0,*) 'Surface pressure = ',p(1,icol),' hPa.'
             write(0,*) '*** MSLP field will not be computed'
             slp(:) = 0.0
             return
          end if
      
       end do
      
      ! Get temperature PCONST hPa above surface.  Use this to extrapolate
      ! the temperature at the surface and down to sea level.
      
       do icol = 1 , ncol
      
          klo = max ( level(icol) - 1 , 1      )
          khi = min ( klo + 1        , nlev_in - 1 )
      
          if ( klo .eq. khi ) then
             write(0,*) 'Trapping levels are weird.'
             write(0,*) 'icol = ',icol
             write(0,*) 'klo = ',klo,', khi = ',khi, ': and they should not be equal.'
             write(0,*) '*** MSLP field will not be computed'
             slp(:) = 0.0
             return
          end if
      
          plo = p(klo,icol)
          phi = p(khi,icol)
          tlo = t(klo,icol) * (1. + 0.608 * scalars(index_qv,klo,icol))
          thi = t(khi,icol) * (1. + 0.608 * scalars(index_qv,khi,icol))
          zlo = 0.5*(height(klo,icol)+height(klo+1,icol))
          zhi = 0.5*(height(khi,icol)+height(khi+1,icol))
      
          p_at_pconst = p(1,icol) - pconst
          t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi)
          z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi)
      
          t_surf(icol) = t_at_pconst*(p(1,icol)/p_at_pconst)**(gamma*rr/grav)
          t_msl(icol) = t_at_pconst+gamma*z_at_pconst
      !   if (icol.eq.500) then
      !      write(0,*) plo,phi,tlo,thi,zlo,zhi,p_at_pconst,t_at_pconst,z_at_pconst
      !      write(0,*) t_surf(icol),t_msl(icol),level(icol),klo,khi
      !      write(0,*) height(klo,icol),height(khi,icol),height(khi+1,icol)
      !   endif
      
       end do
      
      ! If we follow a traditional computation, there is a correction to the sea level
      ! temperature if both the surface and sea level temnperatures are *too* hot.
      
       if ( mm5_test ) then
          kcount = 0
          do icol = 1 , ncol
                l1 = t_msl(icol) .lt. tc
                l2 = t_surf(icol) .le. tc
                l3 = .not. l1
                if ( l2 .and. l3 ) then
                   t_msl(icol) = tc
                else
                   t_msl(icol) = tc - 0.005*(t_surf(icol)-tc)**2
                   kcount = kcount+1
                end if
          end do
      !   write(0,*) 'These number of points had t_msl adjusted ', kcount
       end if
      
       do icol = 1 , ncol
          z_half_lowest=0.5*(height(1,icol)+height(2,icol))
          slp(icol) = p(1,icol) * exp((2.*grav*z_half_lowest)/ &
                                    (rr*(t_msl(icol)+t_surf(icol))))
       end do
      
       if (allocated(level))  deallocate(level)
       if (allocated(t_surf)) deallocate(t_surf)
       if (allocated(t_msl))  deallocate(t_msl)
   
    end subroutine compute_slp


   !***********************************************************************
   !
   !  function compute_layer_mean
   !
   !> \brief   Computes the mean of a field in the specified layer.
   !> \author  Michael Duda
   !> \date    Mar 2018
   !> \details
   !>  Follows general formulation from Michael Duda's implementation but
   !>  operates in 1D and decreasing pressure.
   !>  Given a 1d pressure field, press_in(nVertLevels), with pressure
   !>  decreasing with vertical index, and a 1d field,
   !>  field_in(nVertLevels) with levels in the same order, this routine
   !>  will compute the mean of the field for each column between pressures
   !>  p1 and p2.
   !
   !----------------------------------------------------------------------- 
    function compute_layer_mean(p1, p2, field_in, press_in, nVertLevels)
   
       implicit none
   
       real(kind=RKIND) :: compute_layer_mean
       real(kind=RKIND), intent(in) :: p1, p2
       real(kind=RKIND), dimension(:), intent(in) :: field_in
       real(kind=RKIND), dimension(:), intent(in) :: press_in
   
       integer, intent(in) :: nVertLevels

       integer :: k
       integer :: k_bot, k_top
       real(kind=RKIND) :: p_bot, p_top
       real(kind=RKIND) :: wtop_p, wtop_m
       real(kind=RKIND) :: wbot_p, wbot_m
       real(kind=RKIND) :: wtotal, w
       real(kind=RKIND) :: temp, layerMean
       
       !
       ! Set the pressure at the top and bottom of the layer
       !
       if (p1 < p2) then
          p_top = p1
          p_bot = p2
       else
          p_top = p2
          p_bot = p1
       end if

        k_bot = -1
        k_top = -1

        ! Search for trapping levels: k_top is the index just above (or equal to)
        ! p_top, and k_bot is the index just above (or equal to) p_bot.
        vertloop: do k=1,nVertLevels-1
            if (press_in(k+1) <= p_top .and. press_in(k) > p_top) then
                k_top = k
                wtop_p = (press_in(k) - p_top)/(press_in(k) - press_in(k+1))
                wtop_m = (p_top - press_in(k+1))/(press_in(k) - press_in(k+1))
            end if
            if (press_in(k+1) <= p_top .and. press_in(k) > p_bot) then
                k_bot = k
                wbot_m = (press_in(k) - p_bot) / (press_in(k) - press_in(k+1))
                wbot_p = (p_bot - press_in(k+1)) / (press_in(k) - press_in(k+1))
            end if
        end do vertloop
   
        if (k_top == -1 .or. k_bot == -1) then      ! Layer intersects top or bottom boundary
            layerMean = 0.0
        else if (k_top == k_bot) then               ! Layer lies entirely within a single model layer
            layerMean = wtop_m * field_in(k_top) + wtop_p * field_in(k_top+1)
            layerMean = layerMean + wbot_m * field_in(k_bot+1) + wbot_p * field_in(k_bot)
            layerMean = 0.5 * layerMean
        else

            ! First layer: p_bot/k_bot
            wtotal = p_bot - press_in(k_bot+1)
            temp = wbot_m * field_in(k_bot+1) + wbot_p * field_in(k_bot)
            layerMean = wtotal * 0.5 * (field_in(k_bot+1) + temp)

            ! Middle layers
            do k=k_bot+1,k_top-1
                w = press_in(k) - press_in(k+1)
                wtotal = wtotal + w
                layerMean = layerMean + w * 0.5 * (field_in(k) + field_in(k+1))
            end do

            ! Last layer: p_top/k_top
            w = press_in(k_top) - p_top
            wtotal = wtotal + w
            temp = wbot_m * field_in(k_top) + wbot_p * field_in(k_top+1)
            layerMean = layerMean + w * 0.5 * (field_in(k_top) + temp)

            layerMean = layerMean / wtotal
        end if

        compute_layer_mean = layerMean

    end function compute_layer_mean

    !===============================================================================================
    ! Interpolate pressure to w levels
    subroutine stagger_pressure(pressure, pressure2, height, nVertLevels, nCells)
        real (kind=RKIND), dimension(:,:), intent(in) :: pressure, height
        integer, intent(in) :: nVertLevels, nCells

        real (kind=RKIND), dimension(:,:), intent(out) :: pressure2

        real (kind=RKIND) :: z0, z1, z2, w1, w2
        integer :: iCell, k

        k = nVertLevels + 1
        do iCell = 1, nCells
            z0 = height(k,iCell)
            z1 = 0.5*(height(k,iCell)+height(k-1,iCell))
            z2 = 0.5*(height(k-1,iCell)+height(k-2,iCell))
            w1 = (z0-z2)/(z1-z2)
            w2 = 1.-w1
            !use log of pressure to avoid occurrences of negative top-of-the-model pressure.
            pressure2(k,iCell) = exp(w1*log(pressure(k-1,iCell))+w2*log(pressure(k-2,iCell)))
        enddo
        do k = 2, nVertLevels
            do iCell = 1, nCells
                w1 = (height(k,iCell)-height(k-1,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
                w2 = (height(k+1,iCell)-height(k,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
                pressure2(k,iCell) = w1*pressure(k,iCell) + w2*pressure(k-1,iCell)
            enddo
        enddo
        k = 1
        do iCell = 1, nCells
            z0 = height(k,iCell)
            z1 = 0.5*(height(k,iCell)+height(k+1,iCell))
            z2 = 0.5*(height(k+1,iCell)+height(k+2,iCell))
            w1 = (z0-z2)/(z1-z2)
            w2 = 1.-w1
            pressure2(k,iCell) = w1*pressure(k,iCell)+w2*pressure(k+1,iCell)
        enddo

    end subroutine

    !===============================================================================================
    ! Interpolate pressure from cells to vertices
    subroutine vertex_pressure(pressure, pPerturb, pBase, pVertex, nVertLevels, nCells, nVertices)
        real (kind=RKIND), dimension(:,:), intent(in) :: pressure
        real (kind=RKIND), dimension(:,:), intent(in) :: pPerturb, pBase
        integer, intent(in) :: nVertLevels, nCells, nVertices

        real (kind=RKIND), dimension(:,:), intent(out) :: pVertex

        real (kind=RKIND), dimension(nVertLevels, nCells+1) :: pressureCp1
        real (kind=RKIND), dimension(:,:), pointer :: kiteAreasOnVertex
        real (kind=RKIND), dimension(:), pointer :: areaTriangle
        integer, dimension(:,:), pointer :: cellsOnVertex
        integer, pointer :: vertexDegree

        integer :: iVert, k, iVertD

        pressureCp1(:,:) = pressure(:,:)
        pressureCp1(:,nCells+1)   = (pPerturb(:,nCells+1) + pBase(:,nCells+1)) / 100._RKIND

        call mpas_pool_get_dimension(mesh, 'vertexDegree', vertexDegree)
        call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
        call mpas_pool_get_array(mesh, 'areaTriangle', areaTriangle)
        call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)

        !calculation of total pressure at cell vertices (at mass points):
        do iVert = 1, nVertices
            pVertex(:,iVert) = 0._RKIND

            do k = 1, nVertLevels
                do iVertD = 1, vertexDegree
                    pVertex(k,iVert) = pVertex(k,iVert) &
                        + kiteAreasOnVertex(iVertD,iVert)*pressureCp1(k,cellsOnVertex(iVertD,iVert))
                enddo
                pVertex(k,iVert) = pVertex(k,iVert) / areaTriangle(iVert)
            enddo
        enddo
    end subroutine

    !===============================================================================================
    ! Includes relhum, dewpoint, temperature, uzonal, umeridional
    ! pOut is assumed sorted in descending order
    subroutine interp_cell_center_diagnostics(pressure, temperature, dewpoint, pOut, nPOut, nCells, nVertLevels)

        real (kind=RKIND), dimension(:,:,:), allocatable :: intw  ! interp weights
        integer, dimension(:,:,:), allocatable :: inti            ! interp indices

        real (kind=RKIND), dimension(nVertLevels,nCells), intent(in) :: pressure, temperature, dewpoint
        real (kind=RKIND), dimension(nPOut), intent(in) :: pOut

        integer, intent(in) :: nPOut, nCells, nVertLevels

        real (kind=RKIND), dimension(:,:), pointer :: field_in

        ! Preparation
        if (need_temp .or. need_dewpoint .or. need_relhum .or. need_uzonal .or. need_umeridional) then
            allocate(intw(2,nPOut,nCells))
            allocate(inti(2,nPOut,nCells))
            call calculate_interp_weights(pressure, pOut, nPOut, nCells, nVertLevels, intw, inti)
        end if

        ! Interpolation
        if (need_temp) then
            call apply_weights(temperature, intw, inti, 'temperature_1000hPa', 1)
            call apply_weights(temperature, intw, inti, 'temperature_925hPa' , 2)
            call apply_weights(temperature, intw, inti, 'temperature_850hPa' , 3)
            call apply_weights(temperature, intw, inti, 'temperature_700hPa' , 4)
            call apply_weights(temperature, intw, inti, 'temperature_500hPa' , 5)
            call apply_weights(temperature, intw, inti, 'temperature_250hPa' , 6)
            call apply_weights(temperature, intw, inti, 'temperature_200hPa' , 7)
        end if

        if (need_dewpoint) then
            call apply_weights(dewpoint, intw, inti, 'dewpoint_1000hPa', 1)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_925hPa' , 2)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_850hPa' , 3)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_700hPa' , 4)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_500hPa' , 5)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_250hPa' , 6)
            call apply_weights(dewpoint, intw, inti, 'dewpoint_200hPa' , 7)
        end if

        if (need_relhum) then
            call mpas_pool_get_array(diag, 'relhum', field_in)
            call apply_weights(field_in, intw, inti, 'relhum_1000hPa', 1)
            call apply_weights(field_in, intw, inti, 'relhum_925hPa' , 2)
            call apply_weights(field_in, intw, inti, 'relhum_850hPa' , 3)
            call apply_weights(field_in, intw, inti, 'relhum_700hPa' , 4)
            call apply_weights(field_in, intw, inti, 'relhum_500hPa' , 5)
            call apply_weights(field_in, intw, inti, 'relhum_250hPa' , 6)
            call apply_weights(field_in, intw, inti, 'relhum_200hPa' , 7)
        end if

        if (need_uzonal) then
            call mpas_pool_get_array(diag, 'uReconstructZonal', field_in)
            call apply_weights(field_in, intw, inti, 'uzonal_1000hPa', 1)
            call apply_weights(field_in, intw, inti, 'uzonal_925hPa' , 2)
            call apply_weights(field_in, intw, inti, 'uzonal_850hPa' , 3)
            call apply_weights(field_in, intw, inti, 'uzonal_700hPa' , 4)
            call apply_weights(field_in, intw, inti, 'uzonal_500hPa' , 5)
            call apply_weights(field_in, intw, inti, 'uzonal_250hPa' , 6)
            call apply_weights(field_in, intw, inti, 'uzonal_200hPa' , 7)
        end if

        if (need_umeridional) then
            call mpas_pool_get_array(diag, 'uReconstructMeridional', field_in)
            call apply_weights(field_in, intw, inti, 'umeridional_1000hPa', 1)
            call apply_weights(field_in, intw, inti, 'umeridional_925hPa' , 2)
            call apply_weights(field_in, intw, inti, 'umeridional_850hPa' , 3)
            call apply_weights(field_in, intw, inti, 'umeridional_700hPa' , 4)
            call apply_weights(field_in, intw, inti, 'umeridional_500hPa' , 5)
            call apply_weights(field_in, intw, inti, 'umeridional_250hPa' , 6)
            call apply_weights(field_in, intw, inti, 'umeridional_200hPa' , 7)
        end if

        ! Clean up
        if (allocated(intw)) deallocate(intw)
        if (allocated(inti)) deallocate(inti)

    end subroutine interp_cell_center_diagnostics

    !===============================================================================================
    ! Includes height, w
    ! pOut is assumed sorted in descending order
    subroutine interp_cell_staggered_diagnostics(pressure, height, pOut, nPOut, nCells, nVertLevels)

        real (kind=RKIND), dimension(:,:,:), allocatable :: intw  ! interp weights
        integer, dimension(:,:,:), allocatable :: inti            ! interp indices

        real (kind=RKIND), dimension(nVertLevels,nCells), intent(in) :: pressure, height
        real (kind=RKIND), dimension(nPOut), intent(in) :: pOut

        integer, intent(in) :: nPOut, nCells, nVertLevels

        real (kind=RKIND), dimension(:,:), pointer :: field_in

        ! Preparation
        if (need_height .or. need_w) then
            allocate(intw(2,nPOut,nCells))
            allocate(inti(2,nPOut,nCells))
            call calculate_interp_weights(pressure, pOut, nPOut, nCells, nVertLevels, intw, inti)
        end if

        ! Interpolation
        if (need_height) then
            call apply_weights(height, intw, inti, 'height_1000hPa', 1)
            call apply_weights(height, intw, inti, 'height_925hPa' , 2)
            call apply_weights(height, intw, inti, 'height_850hPa' , 3)
            call apply_weights(height, intw, inti, 'height_700hPa' , 4)
            call apply_weights(height, intw, inti, 'height_500hPa' , 5)
            call apply_weights(height, intw, inti, 'height_250hPa' , 6)
            call apply_weights(height, intw, inti, 'height_200hPa' , 7)
        end if

        if (need_w) then
            call mpas_pool_get_array(state, 'w', field_in, 1)
            call apply_weights(field_in, intw, inti, 'w_1000hPa', 1)
            call apply_weights(field_in, intw, inti, 'w_925hPa' , 2)
            call apply_weights(field_in, intw, inti, 'w_850hPa' , 3)
            call apply_weights(field_in, intw, inti, 'w_700hPa' , 4)
            call apply_weights(field_in, intw, inti, 'w_500hPa' , 5)
            call apply_weights(field_in, intw, inti, 'w_250hPa' , 6)
            call apply_weights(field_in, intw, inti, 'w_200hPa' , 7)
        end if

        ! Clean up
        if (allocated(intw)) deallocate(intw)
        if (allocated(inti)) deallocate(inti)

    end subroutine

    !===============================================================================================
    ! Includes vorticity
    ! pOut is assumed sorted in descending order
    subroutine interp_vertex_center_diagnostics(pressure, pOut, nPOut, nVertices, nVertLevels)
        real (kind=RKIND), dimension(:,:,:), allocatable :: intw  ! interp weights
        integer, dimension(:,:,:), allocatable :: inti            ! interp indices

        real (kind=RKIND), dimension(nVertLevels,nVertices), intent(in) :: pressure
        real (kind=RKIND), dimension(nPOut), intent(in) :: pOut

        integer, intent(in) :: nPOut, nVertices, nVertLevels

        real (kind=RKIND), dimension(:,:), pointer :: field_in

        ! Preparation
        if (need_vorticity) then
            allocate(intw(2,nPOut,nVertices))
            allocate(inti(2,nPOut,nVertices))
            call calculate_interp_weights(pressure, pOut, nPOut, nVertices, nVertLevels, intw, inti)
        end if

        ! Interpolation
        if (need_vorticity) then
            call mpas_pool_get_array(diag, 'vorticity', field_in, 1)
            call apply_weights(field_in, intw, inti, 'vorticity_1000hPa', 1)
            call apply_weights(field_in, intw, inti, 'vorticity_925hPa' , 2)
            call apply_weights(field_in, intw, inti, 'vorticity_850hPa' , 3)
            call apply_weights(field_in, intw, inti, 'vorticity_700hPa' , 4)
            call apply_weights(field_in, intw, inti, 'vorticity_500hPa' , 5)
            call apply_weights(field_in, intw, inti, 'vorticity_250hPa' , 6)
            call apply_weights(field_in, intw, inti, 'vorticity_200hPa' , 7)
        end if

        ! Clean up
        if (allocated(intw)) deallocate(intw)
        if (allocated(inti)) deallocate(inti)

    end subroutine



    !===============================================================================================
    subroutine apply_weights(field, interpw, interpi, field_out, p_idx, needed)
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        real (kind=RKIND), dimension(:,:), intent(in) :: field
        real (kind=RKIND), dimension(:,:,:), intent(in) :: interpw
        integer,           dimension(:,:,:), intent(in) :: interpi
        CHARACTER(len=*), intent(in) :: field_out
        integer, intent(in) :: p_idx
        logical, optional, intent(in) :: needed

        real (kind=RKIND), dimension(:), pointer :: field_out_p
        integer :: iElm

        if (.not.present(needed) .or. needed) then
            call mpas_pool_get_array(diag, field_out, field_out_p)
            do iElm=1,nElements
                field_out_p(iElm) = field(interpi(1,iElm,p_idx),iElm) * interpw(1,iElm,p_idx) + &
                                    field(interpi(2,iElm,p_idx),iElm) * interpw(2,iElm,p_idx)
            end do
        end if
    end subroutine apply_weights

    !===============================================================================================
    ! Calculate weights for interpolation where same pressure levels and target pressures are used
    ! multiple times. Otherwise, use interp_to_p instead
    subroutine calculate_interp_weights(pressure, pOut, nPOut, nCells, nVertLevels, interpw, interpi)

        real (kind=RKIND), dimension(2,nCells,nPOut), intent(out) :: interpw  ! interp weights
        integer,           dimension(2,nCells,nPOut), intent(out) :: interpi  ! interp indices

        real (kind=RKIND), dimension(nVertLevels,nCells), intent(in) :: pressure
        real (kind=RKIND), dimension(nPOut), intent(in) :: pOut

        integer, intent(in) :: nPOut, nCells, nVertLevels
        integer :: iPOut, iCell, iVertLevel, iVertLevel0
        real (kind=RKIND) :: dpu, dpl
        integer, dimension(nCells) :: iVertLevels0

        iVertLevels0(:) = 1

        loopP: do iPOut=1,nPOut
            loopCell: do iCell=1,nCells
                iVertLevel0 = iVertLevels0(iCell)

                if (pOut(iPOut) > pressure(iVertLevel0,iCell)) then
                    ! Below lowest level
                    interpw(:,iCell,iPOut) = (/1.0_RKIND, 0.0_RKIND/)
                    interpi(:,iCell,iPOut) = (/iVertLevel0, iVertLevel0/)
                else if (pOut(iPOut) <= pressure(nVertLevels,iCell)) then
                    ! Above top level
                    iVertLevel0 = nVertLevels
                    interpw(:,iCell,iPOut) = (/(pOut(iPOut)/pressure(nVertLevels,iCell)), 0.0_RKIND/)
                    interpi(:,iCell,iPOut) = (/iVertLevel0, iVertLevel0/)
                else
                    loopVert: do iVertLevel=iVertLevel0,nVertLevels-1
                        iVertLevel0 = iVertLevel
                        if (pOut(iPOut) > pressure(iVertLevel+1,iCell)) then
                            dpu = pOut(iPOut) - pressure(iVertLevel,iCell)
                            dpl = pressure(iVertLevel+1,iCell) - pOut(iPOut)
                            interpw(:,iCell,iPOut) = (/ (dpl/(dpl+dpu)), (dpu/(dpl+dpu)) /)
                            interpi(:,iCell,iPOut) = (/ iVertLevel     , iVertLevel+1    /)

                            exit loopVert
                        end if
                    end do loopVert
                end if

                iVertLevels0(iCell) = iVertLevel0
            end do loopCell
        end do loopP
    end subroutine calculate_interp_weights
    !===============================================================================================

    !===============================================================================================
    ! Interpolate directly to target 1D array
    function interp_to_p(field, pressure, nVertLevels, pOut, nPOut)
        real (kind=RKIND), dimension(nVertLevels), intent(in) :: field, pressure
        real (kind=RKIND), dimension(nPOut),       intent(in) :: pOut
        integer, intent(in) :: nVertLevels, nPOut

        real (kind=RKIND), dimension(nPOut) :: interp_to_p

        real (kind=RKIND) :: dpu, dpl
        integer :: iPOut, iVertLevel0, iVertLevel

        loopP: do iPOut=1,nPOut
            iVertLevel0 = 1

            if (pOut(iPOut) > pressure(iVertLevel0)) then
                ! Below bottom level
                interp_to_p(iPOut) = field(iVertLevel0)
            else if (pOut(iPOut) <= pressure(nVertLevels)) then
                ! Above top level
                iVertLevel0 = nVertLevels
                interp_to_p(iPOut) = field(nVertLevels) * (pOut(iPOut)/pressure(nVertLevels))
            else
                loopVert: do iVertLevel=iVertLevel0,nVertLevels-1
                    iVertLevel0 = iVertLevel
                    if (pOut(iPOut) > pressure(iVertLevel+1)) then
                        dpu = pOut(iPOut) - pressure(iVertLevel)
                        dpl = pressure(iVertLevel+1) - pOut(iPOut)
                        interp_to_p(iPOut) = (field(iVertLevel) * dpl + field(iVertLevel+1) * dpu)/(dpl+dpu)
                        exit loopVert
                    end if
                end do loopVert
            end if

        end do loopP
    end function

end module isobaric_diagnostics
