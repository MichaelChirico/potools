Internal error in line %d of fread.c, please report on data.table GitHub:  
System error %d unmapping view of file\n
System errno %d unmapping file: %s\n
Internal error: NUMTYPE(%d) > nLetters(%d)
%<PRIu64>%cB (%<PRIu64> bytes)
%.*f%cB (%<PRIu64> bytes)
%<PRIu64> bytes
Unable to allocate %s of contiguous virtual RAM. %s allocation.
Avoidable %.3f seconds. %s time to copy.\n
  File copy in RAM took %.3f seconds.\n
Previous fread() session was not cleaned up properly. Cleaned up ok at the beginning of this fread() call.\n
[01] Check arguments\n
  Using %d threads (omp_get_max_threads()=%d, nth=%d)\n
Internal error: NAstrings is itself NULL. When empty it should be pointer to NULL.
freadMain: NAstring <<%s>> has whitespace at the beginning or end
freadMain: NAstring <<%s>> is recognized as type boolean, this is not permitted.
  No NAstrings provided.\n
  NAstrings = [
]\n
  One or more of the NAstrings looks like a number.\n
  None of the NAstrings look like numbers.\n
  skip num lines = %<PRId64>\n
  skip to string = <<%s>>\n
  show progress = %d\n
  0/1 column will be read as %s\n
sep == quote ('%c') is not allowed
dec='' not allowed. Should be '.' or ','
sep == dec ('%c') is not allowed
quote == dec ('%c') is not allowed
[02] Opening the file\n
  `input` argument is provided rather than a file name, interpreting as raw text to read\n
Internal error: last byte of character input isn't \\0
  Opening file %s\n
file not found: %s
Opened file ok but couldn't obtain its size: %s
File is empty: %s
  File opened, size = %s.\n
File not found: %s
Unable to open file after %d attempts (error %d): %s
GetFileSizeEx failed (returned 0) on file: %s
File is empty: %s
  File opened, size = %s.\n
This is Windows, CreateFileMapping returned error %d for file %s
Opened %s file ok but could not memory map it. This is a %dbit process. %s.
Please upgrade to 64bit
There is probably not enough contiguous virtual memory available
  Memory mapped ok\n
Internal error: Neither `input` nor `filename` are given, nothing to read.
[03] Detect and skip BOM\n
  UTF-8 byte order mark EF BB BF found at the start of the file and skipped.\n
GB-18030 encoding detected, however fread() is unable to decode it. Some character fields may be garbled.\n
File is encoded in UTF-16, this encoding is not supported by fread(). Please recode the file to UTF-8.
  Last byte(s) of input found to be %s and removed.\n
Input is empty or only contains BOM or terminal control characters
[04] Arrange mmap to be \\0 terminated\n
  No \\n exists in the file at all, so single \\r (if any) will be taken as one line ending. This is unusual but will happen normally when there is no \\r either; e.g. a single line missing its end of line.\n
  \\n has been found in the input and different lines can end with different line endings (e.g. mixed \\n and \\r\\n in one file). This is common and ideal.\n
  File ends abruptly with '%c'. Final end-of-line is missing. Using cow page to write 0 to the last byte.\n
This file is very unusual: it ends abruptly without a final newline, and also its size is a multiple of 4096 bytes. Please properly end the last row with a newline using for example 'echo >> file' to avoid this 
  File ends abruptly with '%c'. Copying file in RAM. %s copy.\n
[05] Skipping initial rows if needed\n
skip='%s' not found in input (it is case sensitive and literal; i.e., no patterns, wildcards or regex)
Found skip='%s' on line %<PRIu64>. Taking this to be header row or first row of data.\n
  Skipped to line %<PRIu64> in the file
skip=%<PRIu64> but the input only has %<PRIu64> line%s
Input is either empty, fully whitespace, or skip has been set after the last non-whitespace.
  Moved forward to first non-blank line (%d)\n
  Positioned on line %d starting: <<%s>>\n
[06] Detect separator, quoting rule, and ncolumns\n
  sep='\\n' passed in meaning read lines as single character column\n
  Detecting sep automatically ...\n
  Using supplied sep '%s'\n
  with %d fields using quote rule %d\n
  with %d lines of %d fields using quote rule %d\n
  No sep and quote rule found a block of 2x2 or greater. Single column input.\n
Single column input contains invalid quotes. Self healing only effective when ncol>1
Found and resolved improper quoting in first %d rows. If the fields are not quoted (e.g. field separator does not appear within any field), try quote=\"\" to avoid this warning.
Internal error: ncol==%d line==%d after detecting sep, ncol and first line
Internal error: first line has field count %d but expecting %d
  Detected %d columns on line %d. This line is either column names or first data row. Line starts as: <<%s>>\n
  Quote rule picked = %d\n
  fill=%s and the most number of columns found is %d\n
This file is very unusual: it's one single column, ends with 2 or more end-of-line (representing several NA at the end), and is a multiple of 4096, too.
  Copying file in RAM. %s\n
  1-column file ends with 2 or more end-of-line. Restoring last eol using extra byte in cow page.\n
[07] Detect column types, good nrow estimate and whether first row is column names\n
  'header' changed by user from 'auto' to %s\n
Failed to allocate 2 x %d bytes for type and tmpType: %s
  Number of sampling jump points = %d because 
nrow limit (%<PRIu64>) supplied\n
jump0size==0\n
(%<PRIu64> bytes from row 1 to eof) / (2 * %<PRIu64> jump0size) == %<PRIu64>\n
  A line with too-%s fields (%d/%d) was found on line %d of sample jump %d. %s\n
few
many
Most likely this jump landed awkwardly so type bumps here will be skipped.
  Type codes (jump %03d)    : %s  Quote rule %d\n
  'header' determined to be true due to column %d containing a string on row 1 and a lower type (%s) in the rest of the %d sample rows\n
Internal error: row before first data row has the same number of fields but we're not using it.
Internal error: ch!=pos after counting fields in the line before the first data row.
Types in 1st data row match types in 2nd data row but previous row has %d fields. Taking previous row as column names.
Detected %d column names but the data has %d columns (i.e. invalid file). Added %d extra default column name%s\n
 for the first column which is guessed to be row names or an index. Use setnames() afterwards if this guess is not correct, or fix the file write command that created the file to create a valid file.
s at the end.
Internal error: fill=true but there is a previous row which should already have been filled.
Detected %d column names but the data has %d columns. Filling rows automatically. Set fill=TRUE explicitly to avoid this warning.\n
Failed to realloc 2 x %d bytes for type and tmpType: %s
  'header' determined to be %s because there are%s number fields in the first and only row\n
 no
  'header' determined to be true because all columns are type string and a better guess is not possible\n
  'header' determined to be false because there are some number columns and those columns do not have a string field at the top of them\n
  Type codes (first row)   : %s  Quote rule %d\n
  All rows were sampled since file is small so we know nrow=%<PRIu64> exactly\n
  =====\n
  Sampled %<PRIu64> rows (handled \\n inside quoted fields) at %d jump points\n
  Bytes from first data row on line %d to the end of last row: %<PRIu64>\n
  Line length: mean=%.2f sd=%.2f min=%d max=%d\n
  Estimated number of rows: %<PRIu64> / %.2f = %<PRIu64>\n
  Initial alloc = %<PRIu64> rows (%<PRIu64> + %d%%) using bytes/max(mean-2*sd,min) clamped between [1.1*estn, 2.0*estn]\n
  =====\n
Internal error: sampleLines(%<PRIu64>) > allocnrow(%<PRIu64>)
  Alloc limited to lower nrows=%<PRIu64> passed in.\n
[08] Assign column names\n
Unable to allocate %d*%d bytes for column name pointers: %s
Internal error: reading colnames ending on '%c'
[09] Apply user overrides on column types\n
  Cancelled by user: userOverride() returned false.
Failed to allocate %d bytes for size array: %s
Attempt to override column %d%s%.*s%s of inherent type '%s' down to '%s' ignored. Only overrides to a higher type are currently supported. If this was intended, please coerce to the lower type afterwards.
  After %d type and %d drop user overrides : %s\n
[10] Allocate memory for the datatable\n
  Allocating %d column slots (%d - %d dropped) with %<PRIu64> rows\n
Buffer size %<PRId64> is too large\n
[11] Read the data\n
  jumps=[%d..%d), chunk_size=%<PRIu64>, total_size=%<PRIu64>\n
Internal error: Master thread is not thread 0 but thread %d.\n
Column %d (\"%.*s\") bumped from '%s' to '%s' due to <<%.*s>> on row %<PRIu64>\n
Internal error: invalid head position. jump=%d, headPos=%p, thisJumpStart=%p, sof=%p
  Too few rows allocated. Allocating additional %<PRIu64> rows (now nrows=%<PRIu64>) and continue reading from jump %d\n
  Restarting team from jump %d. nSwept==%d quoteRule==%d\n
  %d out-of-sample type bumps: %s\n
Read %<PRIu64> rows x %d columns from %s file in %02d:%06.3f wall clock time\n
[12] Finalizing the datatable\n
  Type counts:\n
%10d : %-9s '%c'\n
Discarded single-line footer: <<%s>>
Stopped early on line %<PRIu64>. Expected %d fields but found %d. Consider fill=TRUE and comment.char=. First discarded non-empty line: <<%s>>
Found and resolved improper quoting out-of-sample. First healed line %<PRIu64>: <<%s>>. If the fields are not quoted (e.g. field separator does not appear within any field), try quote=\"\" to avoid this warning.
=============================\n
%8.3fs (%3.0f%%) Memory map %.3fGB file\n
%8.3fs (%3.0f%%) sep=
 ncol=%d and header detection\n
%8.3fs (%3.0f%%) Column type detection using %<PRIu64> sample rows\n
%8.3fs (%3.0f%%) Allocation of %<PRIu64> rows x %d cols (%.3fGB) of which %<PRIu64> (%3.0f%%) rows used\n
%8.3fs (%3.0f%%) Reading %d chunks (%d swept) of %.3fMB (each chunk %d rows) using %d threads\n
   + %8.3fs (%3.0f%%) Parse to row-major thread buffers (grown %d times)\n
   + %8.3fs (%3.0f%%) Transpose\n
   + %8.3fs (%3.0f%%) Waiting\n
%8.3fs (%3.0f%%) Rereading %d columns due to out-of-sample type exceptions\n
%8.3fs        Total\n
