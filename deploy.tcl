# deploy --
#
#    Package for experiment deploying
#
# version 1.0:   initial implementation, winter 2011
# Copyright (c) 2008-2010 by Andrey Nakin

package provide nettcl::deploy 1.0.0

namespace eval ::nettcl::deploy {
  namespace export \
    matchVersion
}

# Constants

set ::nettcl::deploy::SERIES_DIR_NAME_FORMAT "series%03d"
set ::nettcl::deploy::START_FILE_NAME "start.sh"
set ::nettcl::deploy::README_FILE_NAME "readme.txt"
set ::nettcl::deploy::MAX_SERIES_IN_RUN 10
set ::nettcl::deploy::EXECUTABLE "/home/student/native_bin/nettcl.condor"
set ::nettcl::deploy::RUN_SCRIPT_FILE_NAME "../run.nettcl"
set ::nettcl::deploy::SEPARATOR "/"
set ::nettcl::deploy::UNIVERSE "standard"

set ::nettcl::deploy::OUT_FILE_NAME "tmp.out"
set ::nettcl::deploy::ERROR_FILE_NAME "tmp.err"
set ::nettcl::deploy::LOG_FILE_NAME "tmp.log"

set ::nettcl::deploy::SERIES_SUBMIT_EXECUTABLE "condor_submit"
set ::nettcl::deploy::SERIES_SUBMIT_BACKGROUNG ""
set ::nettcl::deploy::CONDOR_SUBMIT_FILE_NAME "calc.cmd"
set ::nettcl::deploy::CONDOR_RANK "Speed/(1.+LoadAvg)/((49.+VirtualMachineId)/50)/((49.+TotalLoadAvg)/50)"

set ::nettcl::deploy::CONDOR_SUBMIT_FILE_TEMPLATE {
Universe     = $UNIVERSE
Executable   = $EXECUTABLE
Arguments    = $arguments
Rank         = $CONDOR_RANK
Output       = $OUT_FILE_NAME
Error        = $ERROR_FILE_NAME
Log          = $LOG_FILE_NAME
Notification = Never
Initialdir   = $seriesDir
Getenv       = true
Queue
}

set ::nettcl::deploy::CLEAN_FILE_NAME "clean.sh"
set ::nettcl::deploy::CLEAN_FILE_TEMPLATE {
\#!/bin/sh
rm -rf */tmp.*
}

# Private variables
set ::nettcl::deploy::numOfSeries 0
array set ::nettcl::deploy::params {}
set ::nettcl::deploy::arguments ""
set ::nettcl::deploy::readmeSeries "auto"
set ::nettcl::deploy::readmeRun "auto"
set ::nettcl::deploy::startFileContent ""

proc ::nettcl::deploy::parameter { paramName paramValue } {
  global ::nettcl::deploy::numOfSeries
  global ::nettcl::deploy::params

  set l [llength $paramValue]
  if { $l > $numOfSeries } {
    set numOfSeries $l
  }
  
  set params($paramName) $paramValue
}

proc ::nettcl::deploy::output { outFileName } {
  global ::nettcl::deploy::OUT_FILE_NAME
  set OUT_FILE_NAME $outFileName
}

proc ::nettcl::deploy::executable { execFileName } {
  global ::nettcl::deploy::EXECUTABLE
  set EXECUTABLE $execFileName
}

proc ::nettcl::deploy::runScript { scriptFileName } {
  global ::nettcl::deploy::RUN_SCRIPT_FILE_NAME
  set RUN_SCRIPT_FILE_NAME $scriptFileName
}

proc ::nettcl::deploy::noRunScript { } {
  global ::nettcl::deploy::RUN_SCRIPT_FILE_NAME
  set RUN_SCRIPT_FILE_NAME ""
}

proc ::nettcl::deploy::arguments { s } {
  global ::nettcl::deploy::arguments
  set arguments $s
}

proc ::nettcl::deploy::auto-arguments { } {
  global ::nettcl::deploy::arguments
  global ::nettcl::deploy::params

  set s ""
  foreach p [array names params] {
    append s -
    append s $p
    append s " "
    append s {$}
    append s $p
    append s " "
  }
  set arguments $s
}

proc ::nettcl::deploy::readme { scope s } {
  global ::nettcl::deploy::readmeSeries
  global ::nettcl::deploy::readmeRun

  if { $scope == "series" } {
    set readmeSeries $s
  } elseif { $scope == "run" } {
    set readmeRun $s
  } else {
    error "Unknown scope: $scope"
  }
}

proc ::nettcl::deploy::make { } {
  global ::nettcl::deploy::numOfSeries

  # create series directories  
  set runNum [retrieveRunNum]
  for { set seriesNum 0 } { $seriesNum < $numOfSeries } { incr seriesNum } {
    set seriesDir [makeSeriesDir $runNum $seriesNum]
    makeCondorSubmitFile $seriesNum $seriesDir
    makeSeriesReadmeFile $seriesNum $seriesDir
  }
  
  # create run-wide files
  makeRunReadmeFile
  makeCleanFile
  makeStartFile
}

proc ::nettcl::deploy::retrieveRunNum {} {
  global ::nettcl::deploy::SEPARATOR
  
  set parts [split [pwd] $SEPARATOR]
  set s [lindex $parts [expr [llength $parts] - 1]]
  if { [regexp -nocase {^run[0]*([0-9]+)$} $s matchresult num] } {
    return [expr int(1 * $num)]
  }

  return 0
}

proc ::nettcl::deploy::makeSeriesDir { runNum seriesNum } {
  global ::nettcl::deploy::SERIES_DIR_NAME_FORMAT
  global ::nettcl::deploy::MAX_SERIES_IN_RUN
  
  set totalSeriesNum [expr int($runNum * $MAX_SERIES_IN_RUN + $seriesNum)]
  set dirName [format $SERIES_DIR_NAME_FORMAT $totalSeriesNum]
  file mkdir $dirName
  return $dirName 
}

proc ::nettcl::deploy::writeFile { dirName fileName content { setPerms 0 } } {
  global ::nettcl::deploy::SEPARATOR
  
  set fileName "${dirName}${SEPARATOR}${fileName}"
  set f [open $fileName w]
  puts $f $content
  close $f
  
  if { $setPerms != 0 } {
    set p [file attributes $fileName -permissions]
    file attributes $fileName -permissions [expr int($p | $setPerms)]
  }
}

proc ::nettcl::deploy::makeCondorSubmitFile { seriesNum seriesDir } {
  global ::nettcl::deploy::EXECUTABLE
  global ::nettcl::deploy::CONDOR_RANK
  global ::nettcl::deploy::OUT_FILE_NAME
  global ::nettcl::deploy::ERROR_FILE_NAME
  global ::nettcl::deploy::LOG_FILE_NAME
  global ::nettcl::deploy::CONDOR_SUBMIT_FILE_TEMPLATE
  global ::nettcl::deploy::CONDOR_SUBMIT_FILE_NAME
  global ::nettcl::deploy::startFileContent
  global ::nettcl::deploy::RUN_SCRIPT_FILE_NAME
  global ::nettcl::deploy::SEPARATOR
  global ::nettcl::deploy::SERIES_SUBMIT_EXECUTABLE
  global ::nettcl::deploy::SERIES_SUBMIT_BACKGROUNG
  global ::nettcl::deploy::UNIVERSE

  set arguments "${RUN_SCRIPT_FILE_NAME} [makeArguments $seriesNum]"
  eval "set __s \"[string trim $CONDOR_SUBMIT_FILE_TEMPLATE]\""
  
  writeFile $seriesDir $CONDOR_SUBMIT_FILE_NAME $__s
  
  if { [string length $startFileContent] == 0} {
    set startFileContent "#!/bin/sh\n"
  }
  set startFileContent "${startFileContent}${SERIES_SUBMIT_EXECUTABLE} ${seriesDir}${SEPARATOR}${CONDOR_SUBMIT_FILE_NAME} ${SERIES_SUBMIT_BACKGROUNG}\n"
}

proc ::nettcl::deploy::makeSeriesReadmeFile { seriesNum seriesDir } {
  global ::nettcl::deploy::README_FILE_NAME
  global ::nettcl::deploy::params
  global ::nettcl::deploy::readmeSeries ""
  
  if { $readmeSeries == "auto" } {
    set __s ""
    
    foreach paramName [array names params] {
      set v $params($paramName)
      set l [llength $v]
      if { $l > 1 } {
        set idx $seriesNum
        if { $idx >= $l } {
          set idx [expr int($l - 1)]
        }
        set v [lindex $v $idx]
      }
      if { [string length $__s] > 0 } {
        set __s "$__s\n"
      }
      set v [string trim $v]
      set __s "${__s}${paramName}=${v}"
    }
  } else {
    foreach paramName [array names params] {
      set v $params($paramName)
      set l [llength $v]
      if { $l > 1 } {
        set idx $seriesNum
        if { $idx >= $l } {
          set idx [expr int($l - 1)]
        }
        set v [lindex $v $idx]
      }
      set v [string trim $v]
      set $paramName $v
    }
    
    eval "set __s \"[string trim $readmeSeries]\""
  }
  
  writeFile $seriesDir $README_FILE_NAME $__s
}

proc ::nettcl::deploy::makeRunReadmeFile { } {
  global ::nettcl::deploy::README_FILE_NAME
  global ::nettcl::deploy::params
  global ::nettcl::deploy::readmeRun ""
  
  if { $readmeRun == "auto" } {
    set __s ""
    
    foreach paramName [array names params] {
      set v $params($paramName)
      if { [llength $v] == 1 } {
        if { [string length $__s] > 0 } {
          set __s "$__s\n"
        }
        set v [string trim $v]
        set __s "${__s}${paramName}=${v}"
      }
    }
  } else {
    foreach paramName [array names params] {
      set v $params($paramName)
      if { [llength $v] == 1 } {
        set v [string trim $v]
        set $paramName $v
      }
    }
    
    eval "set __s \"[string trim $readmeRun]\""
  }
  
  writeFile "." $README_FILE_NAME $__s
}

proc ::nettcl::deploy::makeArguments { _seriesNum } {
  global ::nettcl::deploy::arguments
  global ::nettcl::deploy::params

  foreach _paramName [array names params] {
    set _v $params($_paramName)
    set _l [llength $_v]
    if { $_l > 1 } {
      set _idx $_seriesNum
      if { $_idx >= $_l } {
        set _idx [expr int($_l - 1)]
      }
      set _v [lindex $_v $_idx]
    }
    set $_paramName [string trim $_v]
  }

  eval "set __s \"[string trim $arguments]\""
  return $__s
}

proc ::nettcl::deploy::makeCleanFile { } {
  global ::nettcl::deploy::CLEAN_FILE_NAME
  global ::nettcl::deploy::CLEAN_FILE_TEMPLATE

  eval "set __s \"[string trim $CLEAN_FILE_TEMPLATE]\""
  writeFile "." $CLEAN_FILE_NAME $__s 0111
}

proc ::nettcl::deploy::makeStartFile { } {
  global ::nettcl::deploy::startFileContent
  global ::nettcl::deploy::START_FILE_NAME

  if { [string length $startFileContent] > 0 } {
    writeFile "." $START_FILE_NAME $startFileContent 0111
  }
}

