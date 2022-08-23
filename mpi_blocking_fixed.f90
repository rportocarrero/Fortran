program hello

    implicit none
    include "mpif.h"

    integer, parameter :: n_numbers=524288
    integer i 
    integer rank, n_ranks, neighbour, ierr
    integer status(MPI_STATUS_SIZE)
    integer send_message(n_numbers)
    integer recv_message(n_numbers)

    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    if (n_ranks .NE. 2) then
        write(6,*) "This example requires exactly two ranks"
        error stop
    end if

    do i = 1, n_numbers
        send_message(i) = I
    end do

    if (rank == 0) then
        call MPI_Send(send_message, n_numbers, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierr)
    end if

    if (rank == 1) then
        call MPI_Recv(recv_message, n_numbers, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status, ierr)
        write(6,*) "message received by rank" ,rank
    end if

    if (rank == 1) then
        call MPI_Send(send_message, n_numbers, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, ierr)
    end if

    if (rank == 0) then
        call MPI__Recv(recv_message, n_numbers, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, status, ierr)
        write(6, *) "Message received by rank", rank
    end if

    call MPI_Finalize()
end