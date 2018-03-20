!  turbulence_diagnostics.F
!
!  Created by John Wong, March / 2018
!  Copyright (c) 2018 The Weather Company. All rights reserved.
!
!  Experimental turbulence diagnostics from MPAS
!
!----------------------------------------------------------------------->

module turbulence_diagnostics

    use mpas_derived_types
    use mpas_kind_types
    use mpas_pool_routines
    use mpas_constants

    type (MPAS_pool_type), pointer :: mesh, state, diag, turb

    integer, pointer :: nCells, nCellsSolve, maxEdges, nVertLevels, nEdges, nVertices
    integer, dimension(:,:), pointer :: cellsOnCell
    real (kind=RKIND), dimension(:,:), pointer :: zgrid, z_unstag
    integer, dimension(:), pointer :: nEdgesOnCell

    real (kind=RKIND), dimension(:,:), allocatable :: deriv_rdzu ! 1/dz used by 3-point linear deriv

    public :: turbulence_diagnostics_setup, &
              turbulence_diagnostics_update, &
              turbulence_diagnostics_compute, &
              turbulence_diagnostics_reset, &
              turbulence_diagnostics_cleanup

    private

    contains


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_setup
    !
    !> \brief   Initialize the turbulence_diagnostics module
    !> \author  John Wong
    !> \date    19 Mar 2018
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_setup(all_pools)

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        integer :: iCell

        ! Caching all necessary pointers for mesh description
        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'turb', turb)

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
        call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
        call mpas_pool_get_dimension(mesh, 'maxEdges', maxEdges)
        call mpas_pool_get_array(mesh, 'cellsOnCell', cellsOnCell)
        call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)

        call mpas_pool_get_array(mesh, 'zgrid', zgrid)
        call mpas_pool_get_array(diag, 'zgrid_unstag', z_unstag)

        ! Compute deriv_rdzu
        call compute_deriv_rdzu()

    end subroutine turbulence_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_compute
    !
    !> \brief  Update turbulence diagnostics every time step
    !> \author John Wong
    !> \date
    !> \details
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_update()
        implicit none
    end subroutine turbulence_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_compute
    !
    !> \brief  Compute deformation for model output
    !> \author John Wong
    !> \date
    !> \details
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_compute()
        implicit none
    end subroutine turbulence_diagnostics_compute


    subroutine turbulence_diagnostics_reset()
        implicit none
    end subroutine turbulence_diagnostics_reset


    subroutine turbulence_diagnostics_cleanup()
        implicit none

        deallocate(deriv_rdzu)
    end subroutine turbulence_diagnostics_cleanup



    !-----------------------------------------------------------------------
    ! Vertical shear (squared) from reconstructed winds at cell centers
    !-----------------------------------------------------------------------
    subroutine shearsq_cell(rec_U, rec_V)
        implicit none

        real (kind=RKIND), dimension(nVertLevels, nCells), intent(in) :: rec_U, rec_V
        real (kind=RKIND), dimension(nVertLevels) :: dudz, dvdz

        real (kind=RKIND), dimension(:,:), pointer :: shearsq

        integer :: iCell

        call mpas_pool_get_array(turb, 'shearsq', shearsq)

        do iCell=1:nCellsSolve
            dudz(:) = dfdz(rec_U(:,iCell), deriv_rdzu(:,iCell))
            dvdz(:) = dfdz(rec_V(:,iCell), deriv_rdzu(:,iCell))

            shearsq(:, iCell) = dudz(:) * dudz(:) + dvdz(:) * dvdz(:)
        end do

    end subroutine

    !-----------------------------------------------------------------------
    ! Richardson number calculation
    !-----------------------------------------------------------------------
    subroutine richardson(theta, shearsq)
        implicit none

        real (kind=RKIND), dimension(nVertLevels, nCells), intent(in) :: theta, shearsq
        real (kind=RKIND), dimension(:,:), pointer :: richno

        real (kind=RKIND), dimension(nVertLevels) :: dthdz, nsq
        integer :: iCell

        call mpas_pool_get_array(turb, 'richno', richno)

        do iCell=1:nCellsSolve
            dthdz(:) = dfdz(theta(:,iCell), deriv_rdzu(:,iCell))
            nsq(:) = gravity/theta(:,iCell) * dthdz(:)

            richno(:,iCell) = nsq(:)/shearsq(:,iCell)
        end do
    end subroutine

    !-----------------------------------------------------------------------
    ! Precompute 1/dz for dfdz
    !-----------------------------------------------------------------------
    subroutine compute_deriv_rdzu()
        implicit none

        integer :: iCell
        real (kind=RKIND), dimension(:), pointer :: z

        allocate(deriv_rdzu(nVertLevels, nCells))

        do iCell=1:nCells
            z => z_unstag(:,iCell)
            deriv_rdzu(1, iCell) = z(2) - z(1)
            deriv_rdzu(2:nVertLevels-1, iCell) = z(3:nVertLevels) - z(1:nVertLevels-2)
            deriv_rdzu(nVertLevels, iCell) = z(nVertLevels) - z(nVertLevels-1)
        end do
        deriv_rdzu(:,:) = 1._RKIND/deriv_rdzu(:,:)
    end subroutine


    !-----------------------------------------------------------------------
    ! Vertical derivatives
    !
    ! Computes the 3-point linear vertical derivatives at level centers
    !-----------------------------------------------------------------------
    function dfdz(field, rdz)

        real (kind=RKIND), dimension(nVertLevels), intent(in) :: field, rdz
        real (kind=RKIND), dimension(nVertLevels) :: df
        real (kind=RKIND), dimension(nVertLevels) :: dfdz ! output

        df(1) = field(2) - field(1)
        df(2:nVertLevels-1) = field(3:nVertLevels) - field(1:nVertLevels-2)
        df(nVertLevels) = field(nVertLevels) - field(nVertLevels-1)

        dfdz(:) = df(:) * rdz(:)
    end function


end module turbulence_diagnostics

