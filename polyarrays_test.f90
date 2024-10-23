module test_polyarrays
    use, intrinsic :: iso_fortran_env, only : dp => real64, int64
    use polyarrays
    implicit none

contains

    subroutine run_tests()
        print*, 'running tests for polyarrays module...'

        call test_allocate_sp_array()
        call test_allocate_dp_array()
        call test_allocate_2d_array()

        print*, 'all tests passed!'

    end subroutine run_tests

    subroutine test_allocate_sp_array()

        class(base_array_type), allocatable :: array
        integer(kind=int64) :: n, m

        ! dimension of 1D array
        n = 10
        m = 0

        ! test single precision array allocation
        allocate(sp_array::array)
        call array%allocate_array(n, m)

        ! verify that array is of type(sp_array) and array is correctly allocated
        select type (array)
        type is (sp_array)
            if (.not. allocated(array%array_1d)) then
                print*, 'test failed: single precision 1D array is not allocated'
                stop
            end if
            print*, 'test passed: single precision 1D array is allocated'
        class default
            print*, 'test failed: array is not of type(sp_array)'
            stop
        end select

        deallocate(array)

    end subroutine test_allocate_sp_array

    subroutine test_allocate_dp_array()
        class(base_array_type), allocatable :: array
        integer(kind=int64) :: n, m

        ! dimension of 1D array
        n = 10
        m = 0

        ! test double precision array allocation
        allocate(dp_array::array)
        call array%allocate_array(n, m)

        ! verify that array is of type(dp_array) and array is correctly allocated
        select type (array)
        type is (dp_array)
            if (.not. allocated(array%array_1d)) then
                print*, 'test failed: double precision 1D array is not allocated'
                stop
            end if
            print*, 'test passed: double precision 1D array is allocated'
        class default
            print*, 'test failed: array is not of type(dp_array)'
            stop
        end select

        deallocate(array)

    end subroutine test_allocate_dp_array

    subroutine test_allocate_2d_array()
        class(base_array_type), allocatable :: array
        integer(kind=int64) :: n, m

        ! dimension of 2D array
        n = 10
        m = 2
        
        ! test single precision 2D array allocation
        allocate(sp_array::array)
        call array%allocate_array(n, m)

        ! verify that the array was correctly allocated
        select type(array)
        type is (sp_array)
            if (.not. allocated(array%array_2d)) then
                print*, 'test failed: single precision 2D array is not allocated'
                stop
            end if
            print*, 'test passed: single precision 2D array is allocated'
        class default
            print*, 'test failed: array is not of type(sp_array)'
            stop
        end select

        deallocate(array)

        ! test double precision 2D array allocation
        allocate(dp_array::array)
        call array%allocate_array(n, m)

        ! verify that the array was correctly allocated
        select type(array)
        type is (dp_array)
            if (.not. allocated(array%array_2d)) then
                print*, 'test failed: double precision 2D array is not allocated'
                stop
            end if
            print*, 'test passed: double precision 2D array is allocated'
        class default
            print*, 'test failed: array is not of type(dp_array)'
            stop
        end select

        deallocate(array)

    end subroutine test_allocate_2d_array

end module test_polyarrays

program polyarrays_test
    use, intrinsic :: iso_fortran_env, only : dp => real64, int64
    use test_polyarrays
    implicit none
    Class(base_array_type), Allocatable :: Rint2
    integer(kind=int64) :: i, j, n1, n2

    ! run tests
    call run_tests()

end program polyarrays_test