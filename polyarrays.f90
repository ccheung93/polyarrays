module polyarrays
    use, intrinsic :: iso_fortran_env, only : dp => real64, int64

    implicit none

    private 

    public :: base_array_type, sp_array, dp_array

    ! abstract base type for arrays
    type, abstract :: base_array_type
    contains
        procedure(allocate_array_i), deferred :: allocate_array
        procedure(print_array_i), deferred :: print_array
    end type base_array_type

    ! interface for deferred subroutines
    abstract interface
        subroutine allocate_array_i(self, n, m)
            use, intrinsic :: iso_fortran_env, only : int64
            import base_array_type
            class(base_array_type), intent(inout) :: self
            integer(kind=int64), intent(in) :: n, m
        end subroutine allocate_array_i

        subroutine print_array_i(self)
            use, intrinsic :: iso_fortran_env, only : int64
            import base_array_type
            class(base_array_type), intent(in) :: self
        end subroutine print_array_i
    end interface

    ! derived type for single precision array 
    type, extends(base_array_type) :: sp_array
        real, allocatable :: array_1d(:), array_2d(:,:)
    contains
        procedure :: allocate_array => allocate_sp_array
        procedure :: print_array => print_sp_array
    end type sp_array

    ! derived type for double precision array  
    type, extends(base_array_type) :: dp_array
        real(dp), allocatable :: array_1d(:), array_2d(:,:)
    contains
        procedure :: allocate_array => allocate_dp_array
        procedure :: print_array => print_dp_array
    end type dp_array

contains

    subroutine allocate_sp_array(self, n, m)
        class(sp_array), intent(inout) :: self
        integer(kind=int64), intent(in) :: n, m
        if (m == 0) then    
            allocate(self%array_1d(n))
        else
            allocate(self%array_2d(n,m))
        end if
    end subroutine allocate_sp_array

    subroutine allocate_dp_array(self, n, m)
        class(dp_array), intent(inout) :: self
        integer(kind=int64), intent(in) :: n, m
        if (m == 0) then    
            allocate(self%array_1d(n))
        else
            allocate(self%array_2d(n,m))
        end if
    end subroutine allocate_dp_array

    subroutine print_sp_array(self)
        class(sp_array), intent(in) :: self
        if (allocated(self%array_1d)) then
            print *, 'Single Precision 1D Array: '
            print*, self%array_1d
        end if
        if (allocated(self%array_2d)) then
            print *, 'Single Precision 2D Array: '
            print *, self%array_2d
        end if
    end subroutine print_sp_array

    subroutine print_dp_array(self)
        class(dp_array), intent(in) :: self
        if (allocated(self%array_1d)) then
            print *, 'Double Precision 1D Array: '
            print*, self%array_1d
        end if
        if (allocated(self%array_2d)) then
            print *, 'Double Precision 2D Array: '
            print *, self%array_2d
        end if
    end subroutine print_dp_array

end module polyarrays