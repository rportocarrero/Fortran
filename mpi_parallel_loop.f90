program print_numbers

    implicit none
    include "mpif.h"

    integer number, number
    integer rank, n_ranks, ierr
    integer numbers_per_rank, my_first, my_last
    number = 10

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    numbers_per_rank = numbers/n_ranks
    if(MOD(numbers, n_ranks) > 0) then
        numbers_per_rank = numbers_per_rank += 1
    end if

    my_first = rank * numbers_per_rank
    my_last = my_first + numbers_per_rank
    do number = my_first, my_last - 1
        if (number < numbers) then
            write(6,*) "I'm rank", rank, " and I'm printing the number", number
        end if
    end do

    call MPI_Finalize(ierr)
end