program hello

    implicit none
    include "mpif.h"
    
    integer rank, n_ranks, my_pair, ierr
    integer status(MPI_STATUS_SIZE)
    character(len=13)  message
    
    call MPI_Init(ierr)

    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    if ( MOD(rank,2) == 1 ) then
       my_pair = rank-1;
    else
       my_pair = rank+1;
    end if

    if( my_pair < n_ranks ) then

         if ( MOD(rank,2) == 0 ) then
              message = "Hello, world!"
              call MPI_Send( message, 13, MPI_CHARACTER, my_pair, 0, MPI_COMM_WORLD, ierr)
         end if

         if ( MOD(rank,2) == 1 ) then
              call MPI_Recv( message, 13, MPI_CHARACTER, my_pair, 0, MPI_COMM_WORLD, status, ierr)
              write(6,*) message
         end if
    end if

    call MPI_Finalize(ierr)
end