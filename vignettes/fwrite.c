buffMB=%d outside [1,1024]
eol must be 1 or more bytes (usually either \\n or \\r\\n) but is length %d
Column writers: 
%d 
%d 
... 
%d 
\nargs.doRowNames=%d args.rowNames=%d doQuote=%d args.nrow=%<PRId64> args.ncol=%d eolLen=%d\n
Internal error: type %d has no max length method implemented
maxLineLen=%<PRIu64>. Found in %.3fs\n
%s: '%s'. Failed to open existing file for writing. Do you have write permission to it? Is this Windows and does another process such as Excel have it open?
%s: '%s'. Unable to create new file for writing (it does not exist already). Do you have permission to write here, is there space on the disk and does the path exist?
Compression in fwrite uses zlib library. Its header files were not found at the time data.table was compiled. To enable fwrite compression, please reinstall data.table and study the output for further guidance.
Writing bom (%s), yaml (%d characters) and column names (%s) ... 
\n
Unable to allocate %d MiB for header: %s
Can't allocate gzip stream structure
Unable to allocate %d MiB for zbuffer: %s
Compress gzip error: %d
%s: '%s'
done in %.3fs\n
No data rows present (nrow==0)\n
%s: '%s'
Writing %<PRId64> rows in %d batches of %d rows (each buffer size %dMB, showProgress=%d, nth=%d)\n
Can't allocate gzip stream structure
Unable to allocate %d MB * %d thread buffers; '%d: %s'. Please read ?fwrite for nThread, buffMB and verbose options.
Unable to allocate %d MB * %d thread compressed buffers; '%d: %s'. Please read ?fwrite for nThread, buffMB and verbose options.
zlib %s (zlib.h %s) deflate() returned error %d with z_stream->msg==\"%s\" Z_FINISH=%d Z_BLOCK=%d. %s
Please include the full output above and below this message in your data.table bug report.
Please retry fwrite() with verbose=TRUE and include the full output with your data.table bug report.
