module fzz
implicit none
public

    interface ensure_allocation
        module procedure ensure_allocation_int_1
        module procedure ensure_allocation_int_2
        module procedure ensure_allocation_int_3
        module procedure ensure_allocation_int_4

        module procedure ensure_allocation_real_1
        module procedure ensure_allocation_real_2
        module procedure ensure_allocation_real_3
        module procedure ensure_allocation_real_4

        module procedure ensure_allocation_cmplx_1
        module procedure ensure_allocation_cmplx_2
        module procedure ensure_allocation_cmplx_3
        module procedure ensure_allocation_cmplx_4

        module procedure ensure_allocation_bool_1
        module procedure ensure_allocation_bool_2
        module procedure ensure_allocation_bool_3
        module procedure ensure_allocation_bool_4
    end interface ensure_allocation

    private :: ensure_allocation_int_1
    private :: ensure_allocation_int_2
    private :: ensure_allocation_int_3
    private :: ensure_allocation_int_4
    private :: ensure_allocation_real_1
    private :: ensure_allocation_real_2
    private :: ensure_allocation_real_3
    private :: ensure_allocation_real_4
    private :: ensure_allocation_cmplx_1
    private :: ensure_allocation_cmplx_2
    private :: ensure_allocation_cmplx_3
    private :: ensure_allocation_cmplx_4
    private :: ensure_allocation_bool_1
    private :: ensure_allocation_bool_2
    private :: ensure_allocation_bool_3
    private :: ensure_allocation_bool_4


!   interface h5tget_file
!       module procedure h5tget_file_int
!       module procedure h5tget_file_real
!       module procedure h5tget_file_char
!   end interface h5tget_file

!   interface h5tget_mem
!       module procedure h5tget_mem_int
!       module procedure h5tget_mem_real
!       module procedure h5tget_mem_char
!   end interface h5tget_mem

!   private :: h5tget_file_int, h5tget_file_real, h5tget_file_char
!   private :: h5tget_mem_int, h5tget_mem_real, h5tget_mem_char


    interface resizearray
        module procedure resizearray_mat, resizearray_vec
    end interface resizearray

    interface resizearray_ptr
        module procedure resizearray_vecptr, resizearray_matptr
    end interface resizearray_ptr

    private :: resizearray_mat, resizearray_vec
    private :: resizearray_matptr, resizearray_vecptr


    interface sort
        module procedure sortr, sortrr, sortri, sorti, sortii, sortir
    end interface sort

    private :: sortr, sortrr, sortri, sorti, sortii, sortir


    interface interp1ez
        module procedure interp1ez_s, interp1ez_arr
    end interface interp1ez

    private :: interp1ez_s, interp1ez_arr


    interface quantile
        module procedure quantile, quantile_arr
    end interface quantile

    private :: quantile_arr

    interface random_int
        module procedure random_int, random_int_arr
    end interface random_int

    private :: random_int_arr

contains

    include "datetime.f90"
    include "ensure_allocation.f90"
    include "find.f90"
    include "fmin_brent.f90"
    include "fzero_brent.f90"
    include "ieee.f90"
    include "interp1ez.f90"
    include "integration.f90"
    include "la.f90"
    include "mnbrak.f90"
    include "psort.f90"
    include "random.f90"
    include "resizearray.f90"
    include "sort.f90"
    include "spareunit.f90"
    include "spatial.f90"
    include "stats.f90"
    include "string.f90"

end module fzz
