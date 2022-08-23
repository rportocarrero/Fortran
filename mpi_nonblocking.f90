program Hello

    implicit none
    include "mpif.h"

    integer, parameter :: n_numbers=10000
    integer i
    integer rank, n_ranks, neighbour, request, ierr
    integer sent_message(n_numbers)
    integer recv_message(n_numbers)

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLd, n_ranks, ierr)

    if(n_ranks .NE. 2) then
        write(6,*) "This example requires exactly two ranks"
        error stop
    end if

    if (rank == 0) then
        neighbour = 1
    else 
        neighbour = 0
    end if

    do i = 1, n_numbers
        send_message(i) = i;
    end do

    call MPI_Isend(recv_message, n_numbers, MPI_INTEGER, neighbour, 0 MPI_COMM_WORLD, request, ierr)
    
    call MPI_Irecv(recv_messagem n_numbersm MPI_INTEGER, neighbour, 0 MPI_COMM_WORLD, request, ierr)
    call MPI_WAIT(request, status, ierr)
    write(6,*) "Message received by rank", rank
    
    call MPI_Finalize(ierr)
end