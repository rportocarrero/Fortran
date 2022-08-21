! This requires the installation of the gfortran compiler
program hello

    implicit none
    include "mpif.h"

    integer rank, n_ranks, ierr

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks, ierr)
    write(6, *) "Hello World! I'm rank ", rank
    write(6, *) "Total no. of ranks = ", n_ranks
    call MPI_Finalize(ierr)

end