###############################################################################
# fileutil.tcl --
#
#    Package for basic operation with files
#
# version 1.0:   initial implementation, autumn 2010
###############################################################################

package provide nettcl::fileutil 1.0.0

namespace eval nettcl::fileutil {

###############################################################################
# Copies n first lines from source file to destination file
# Parameters:
#   n - number of lines to copy
#   srcFile - name of source file
#   destFile - name of destination file, if not specified, lines are copied to stdout
###############################################################################
proc head { n srcFile { destFile "" } } {

  if { [catch {open $srcFile r} src] } {
      puts stderr "Could not open $srcFile for reading\n$src"
      exit 1
  }

  if { [string length $destFile] > 0 } {
    if { [catch {open $destFile w} dest] } {
        puts stderr "Could not open $destFile for reading\n$dest"
        close $src
        exit 1
    }
    set ownDest 1
  } else {
    set dest stdout
    set ownDest 0
  }

  while { [ gets $src line ] >= 0 && $n > 0 } {
    puts $dest $line
    incr n -1
  }
  
  close $src
  if { $ownDest } {
    close $dest
  }
  
}

###############################################################################
# Counts number of lines in a file
# Parameters:
#   fileName - name of file to test
# Return:
#   number of lines in file
###############################################################################
proc wcl { fileName } {

  if { [catch {open $fileName r} src] } {
      puts stderr "Could not open $fileName for reading\n$src"
      exit 1
  }

  set n 0
  while { [ gets $src line ] >= 0 } {
    incr n
  }
  
  close $src
  return $n
}

# end of package
}

