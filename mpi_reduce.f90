program sum_and_max

    implicit none
    include "mpif.h"

    integer rank, n_ranks, ierr

    integer, parameter :: n_numbers=10
    real vector(n_numbers)
    real vsum, vmax, my_first_number
    integer i 

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    my_first_number = n_numbers * rank
    
    do i=1, n_numbers:
        vector(i) = my_first_number + i
    end do

    call find_sum(vector, n_numbers, vsum)
    write(6,*) "Maximum =", vmax

    call MPI_Finalize(ierr)

    contains

        subroutine find_sum(vector, N, global_sum)
            implicit none
            include "mpif.h"

            real, intent(in) :: vector(:)
            real, intent(inout) :: global_sum
            real vsum
            integer, intent(in) :: n_numbers
            integer i, ierr

            vsum = 0
            do i = 1, N 
                vsum = vsum + vector(i)
            end do

            call MPI_Allreduce(vsum, global_sum, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)

        end subroutine find_sum

        subroutine find_max(vector, N, glboal_max)
            implicit none
            include "mpif.h"

            real, intent(int) :: vector(:)
            real, intent(inout) :: glboal_max
            real vmax
            integer, intent(in) :: n_numbers
            integer, i, ierr

            vmax = 0
            do i = 1,N 
                if (vmax < vector(i)) then
                    vmax = vector(i)
                end if
            end do
            
            call MPI_Allreduce(vmax, glboal_max, 1, MPI_REAL, MPI_MAX, MPI_COMM_WORLD, ierr)

        end subroutine find_max
    end