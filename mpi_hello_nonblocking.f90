program hello

    implicit none
    include "mpif.h"

    integer rank, n_ranks, request, ierr
    integer status(MPI_STATUS_SIZE)
    charachter(len=13) message

    call MPI_Init(ierr)
    
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)
    if (n_ranks < 2) then
        write(6,*) "This example requires at least two ranks"
        error stop
    end if

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    if (rank == 0) then
        message = "Hello, world!"
        call MPI_Isend(message, 13, MPI_CHARACHTER, 1, 0, MPI_COMM_WORLD, request, ierr)
    end if

    if (rank == 1) then
        call MPI_Irecv(message, 13, MPI_CHARACHTER, 0, 0, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, status, ierr)
    end if

    call MPI_Finalize(ierr)
end