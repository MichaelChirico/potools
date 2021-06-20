Ignoring invalid %s==\"%s\". Not an integer >= 1. Please remove any characters that are not a digit [0-9]. See ?data.table::setDTthreads.
Ignoring invalid R_DATATABLE_NUM_PROCS_PERCENT==%d. If used it must be an integer between 2 and 100. Default is 50. See ?setDTtheads.
'verbose' must be TRUE or FALSE
This installation of data.table has not been compiled with OpenMP support.\n
  OpenMP version (_OPENMP)       %d\n
  omp_get_num_procs()            %d\n
  R_DATATABLE_NUM_PROCS_PERCENT  %s\n
  R_DATATABLE_NUM_THREADS        %s\n
  R_DATATABLE_THROTTLE           %s\n
  omp_get_thread_limit()         %d\n
  omp_get_max_threads()          %d\n
  OMP_THREAD_LIMIT               %s\n
  OMP_NUM_THREADS                %s\n
  RestoreAfterFork               %s\n
  data.table is using %d threads with throttle==%d. See ?setDTthreads.\n
restore_after_fork= must be TRUE, FALSE, or NULL (default). getDTthreads(verbose=TRUE) reports the current setting.\n
'throttle' must be a single number, non-NA, and >=1
threads= must be either NULL or a single number >= 0. See ?setDTthreads.
Internal error: percent= must be TRUE or FALSE at C level
Internal error: threads==%d should be between 2 and 100 (percent=TRUE at C level).
