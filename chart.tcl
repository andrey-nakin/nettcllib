###############################################################################
# chart.nettcl --
#
#    Package with GNUPLOT supporting procedures
#
# version 1.0:   initial implementation, autumn 2009
###############################################################################

package require fileutil 1.13
package require math::statistics 0.5
package require math::fourier 1.0.2
package require crc16 1.1.1
package require nettcl 1.0.0
package provide nettcl::chart 1.0.0

namespace eval nettcl::chart {

###############################################################################
# Global constants
###############################################################################

# Rules for PDF terminal
set TERMINAL_PDF "pdf enhanced"
# GNUPLOT executable command
set GNUPLOT "gnuplot"
# Current GNUPLOT terminal
set TERMINAL $TERMINAL_PDF
# Directory descrption file name
set README_FILE_NAME "readme.txt" 
# Parameters to add to chart title
set TITLE_PARAM_NAMES {N m <J> dJ A zc+/zc- dh}
# Min number of avalanches to include line into profile chart
set PROFILE_CHART_MIN_AVAL_NUM 100
# Network definition file name
set NETWORK_FILE_NAME "network.net" 

# Parameter name substitutions
array set PARAM_NAME_SUBSTITUTION {}
set PARAM_NAME_SUBSTITUTION(J) "<J>"  
set PARAM_NAME_SUBSTITUTION(dJ) "{/Symbol D}J"  
set PARAM_NAME_SUBSTITUTION(dh) "{/Symbol D}h"  
set PARAM_NAME_SUBSTITUTION(dH) "{/Symbol D}H"  
set PARAM_NAME_SUBSTITUTION(threshold) "P_t"  
set PARAM_NAME_SUBSTITUTION(zc+/zc-) "z@^+_c/z@^-_c"  
set PARAM_NAME_SUBSTITUTION(nb+/nb-) "n@^+_b/n@^-_b"  
set PARAM_NAME_SUBSTITUTION(nb-/nb+) "n@^-_b/n@^+_b"  
set PARAM_NAME_SUBSTITUTION(na) "n_a"  
set PARAM_NAME_SUBSTITUTION("S(na)") "S(n_a)"
set PARAM_NAME_SUBSTITUTION(gilbert_p) "Gilbert P"
set PARAM_NAME_SUBSTITUTION(grid2d_rows) "rows"
set n "perturbation method"
set PARAM_NAME_SUBSTITUTION($n) "pert"
set PARAM_NAME_SUBSTITUTION(potential) "potential"
set PARAM_NAME_SUBSTITUTION(node_weight_method) "NWM"
set PARAM_NAME_SUBSTITUTION(avgh) "<z>"
set PARAM_NAME_SUBSTITUTION(xi) "{/Symbol x}"
set PARAM_NAME_SUBSTITUTION(avgz) "<z>"
set PARAM_NAME_SUBSTITUTION(davgz) "{/Symbol D}<z>"
set PARAM_NAME_SUBSTITUTION(V) "<V>"
set PARAM_NAME_SUBSTITUTION(v) "<V>"
set PARAM_NAME_SUBSTITUTION(dV) "{/Symbol D}V"
set PARAM_NAME_SUBSTITUTION(dt) "{/Symbol D}t"
set PARAM_NAME_SUBSTITUTION(grid3d_planes) "planes"
set PARAM_NAME_SUBSTITUTION(grid3d_rows) "rows"
set PARAM_NAME_SUBSTITUTION(alpha) "<{/Symbol a}>"
set PARAM_NAME_SUBSTITUTION(tau) "<{/Symbol t}>"
set PARAM_NAME_SUBSTITUTION(beta) "<{/Symbol b}>"
set PARAM_NAME_SUBSTITUTION(dtau) "{/Symbol Dt}"
set PARAM_NAME_SUBSTITUTION(dbeta) "{/Symbol Db}"
set PARAM_NAME_SUBSTITUTION(delta) "{/Symbol d}"
set PARAM_NAME_SUBSTITUTION(zx) "z_x"
set PARAM_NAME_SUBSTITUTION(zy) "z_y"
set PARAM_NAME_SUBSTITUTION(xyDelta) "{/Symbol d}_{xy}"
set PARAM_NAME_SUBSTITUTION(vx) "<V_x>"
set PARAM_NAME_SUBSTITUTION(dvx) "{/Symbol D}V_x"
set PARAM_NAME_SUBSTITUTION(vy) "<V_y>"
set PARAM_NAME_SUBSTITUTION(dvy) "{/Symbol D}V_y"
set PARAM_NAME_SUBSTITUTION(columns) "L_x"
set PARAM_NAME_SUBSTITUTION(rows) "L_y"
set PARAM_NAME_SUBSTITUTION(epsilon) "{/Symbol e}"
set PARAM_NAME_SUBSTITUTION(xSize) "L_x"
set PARAM_NAME_SUBSTITUTION(ySize) "L_y"
set PARAM_NAME_SUBSTITUTION(zSize) "L_z"

###############################################################################
# internal variables
###############################################################################

set TEMP_FILES {}
set CHANNEL ""

###############################################################################
# Creates temporary file with unique name and returns its name
# Return: full path to file name
###############################################################################
proc temp_file {} {
  global ::nettcl::chart::TEMP_FILES

  set fn [::fileutil::tempfile]
  lappend TEMP_FILES $fn  
  return $fn
}

###############################################################################
# Clears all temporary files previously created by temp_file
###############################################################################
proc clear_temp {} {
  global ::nettcl::chart::TEMP_FILES
  
  for {set i 0} {$i < [llength $TEMP_FILES]} {incr i} {
    file delete [lindex $TEMP_FILES $i]
  }
  
  set TEMP_FILES {}
}

###############################################################################
# Checks whether directory name given is series directory
# Parameters:
#   name - name of the directory to test
# Return:
#   true - name is the name of data series directory 
###############################################################################
proc is_series_dir { name } {
  return [expr [string match "series*" $name] && [file isdirectory $name]] 
}

###############################################################################
# Returns list of names of data series directories within given run
# Series are sorted by their numbers
# Parameters:
#   rundir - name of the experiment run directory
# Return:
#   list of the paths to series directories 
###############################################################################
proc find_series {rundir } {
  return [lsort [::fileutil::find $rundir is_series_dir]]
}

###############################################################################
# Extract series number from series directory
# Parameters:
#   series - name of series directory
# Return:
#   series number 
###############################################################################
proc get_series_num { series } {
  set series [file split $series]
  set series [lindex $series [expr [llength $series] - 1]]
  set l [string length $series]
  return [string range $series [expr $l - 3] $l]
}

###############################################################################
# Creates distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
#   datafile - name of the file within series directory with distribution data
#   using - using expression for data file
#   legendParamNames - names of series parameters to place into legend
#   options - optional GNUPLOT commands
###############################################################################
proc run_distr_chart { rundir datafile using xlabel ylabel title legendParamNames {options ""} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
    
  set seriesDirs [find_series $rundir]
  set tracenum 0
  set plot ""
  
  foreach series $seriesDirs {
    set df "$series[file separator]$datafile"
    if {[file isfile $df] && [file size $df]} {
      if {$tracenum > 0} {
        set plot "$plot ,"
      }
      set plot "$plot \"$df\" using $using title \"[description_to_string $series $legendParamNames]\" with lines"
      incr tracenum
    }
  }
  
  if {[string length $plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"$xlabel\""
    puts $CHANNEL "set ylabel \"$ylabel\""
    puts $CHANNEL "set title \"$title, [description_to_string $rundir $TITLE_PARAM_NAMES]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $plot"
  }
}

###############################################################################
# Creates avalanche size distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
###############################################################################
proc run_aval_distr_chart { rundir legendParamNames {options ""} } {
  return [run_distr_chart $rundir "aval.distr" "1:2" "size" "P(size)" "Avalanche Size Distr." $legendParamNames "set logscale xy\n$options"]
}

###############################################################################
# Creates avalanche length distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
###############################################################################
proc run_length_distr_chart { rundir legendParamNames {options ""} } {
  return [run_distr_chart $rundir "l.distr" "1:2" "L" "P(L)" "Avalanche Length Distr." $legendParamNames "set logscale xy\n$options"]
}

###############################################################################
# Creates avalanche area distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
###############################################################################
proc run_area_distr_chart { rundir legendParamNames {options ""} } {
  return [run_distr_chart $rundir "area.distr" "1:2" "Area" "P(Area)" "Avalanche Area Distr." $legendParamNames "set logscale xy\n$options"]
}

###############################################################################
# Creates Z distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
###############################################################################
proc run_z_distr_chart { rundir legendParamNames {options ""}} {
  return [run_distr_chart $rundir "z.distr" "1:2" "z" "P(z)" "Z Distr." $legendParamNames $options]
}

###############################################################################
# Creates avegare profile chart
# Chatrt displays average profiles of the same length for each series
# Parameters:
#   rundir - directory name with experiment run to build chart for
#   L - required avalanche length
#   legendParamNames - names of series parameters to put into legend
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
###############################################################################
proc run_avg_profile_by_L_chart { rundir L legendParamNames {isValueProfile 0} {options ""} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM
    
  set seriesDirs [find_series $rundir]
  set tracenum 0
  set plot ""
  
  foreach series $seriesDirs {
    set profileFileName [make_avg_profile_file_name $series $isValueProfile] 
    if {![file readable $profileFileName] || ![file size $profileFileName]} {
      continue;
    } 
    set profile [extract_avg_profile $profileFileName $L]
    if {[llength $profile]} {
      # try to retrieve number of avalanches in profile
      set lt ""
      set src [make_profile_file_name $series $L $isValueProfile]
      if {![file readable $src]} {
        set src [make_profile_file_name $series $L $isValueProfile 1]
      }
      if {[file readable $src]} {
        set avalNum [calc_profile_length $src]
        if {$avalNum >= 0} {
          if {$avalNum < $PROFILE_CHART_MIN_AVAL_NUM} {
            continue
          }
          set lt " ($avalNum avals.)"
        }
      }
      
      set df [make_profile_data_file $profile] 
      if {$tracenum > 0} {
        set plot "$plot ,\\\n"
      }
      set plot "$plot \"$df\" title \"[description_to_string $series $legendParamNames]$lt\" with linespoints"
      incr tracenum
    }    
  }

  if {[string length $plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 1"
    puts $CHANNEL "set mytics 10"
    if {$isValueProfile} {
      puts $CHANNEL "set xlabel \"v_a\""
      puts $CHANNEL "set ylabel \"S(v_a)\""
      set pt "Value"
    } else {
      puts $CHANNEL "set xlabel \"n_a\""
      puts $CHANNEL "set ylabel \"S(n_a)\""
      set pt "Count"
    }
    puts $CHANNEL "set title \"Average $pt Profiles, L=$L, [description_to_string $rundir $TITLE_PARAM_NAMES]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $plot"
  }
}

###############################################################################
# Creates avegare profile chart
# Chart displays several profiles of the same series with different avalanche lengths
# Parameters:
#   series - series directory
#   minL - min avalanche length
#   maxL - max avalanche length
#   stepL - length increment value
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
###############################################################################
proc series_avg_profile_by_L_chart { series minL maxL stepL titleParamNames {isValueProfile 0} {options ""} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM

  set tracenum 0
  set plot ""
  
  for {set L $minL} {$L <= $maxL} {incr L [expr int($stepL)]} {
    set profileFileName [make_avg_profile_file_name $series $isValueProfile]
    if {![file readable $profileFileName] || ![file size $profileFileName]} {
      continue;
    } 
    set profile [extract_avg_profile $profileFileName $L]
    if {[llength $profile]} {
      # try to retrieve number of avalanches in profile
      set lt ""
      set src [make_profile_file_name $series $L $isValueProfile 0]
      if {![file readable $src]} {
        set src [make_profile_file_name $series $L $isValueProfile 1]
      }
      if {[file readable $src]} {
        set avalNum [calc_profile_length $src]
        if {$avalNum >= 0} {
          if {$avalNum < $PROFILE_CHART_MIN_AVAL_NUM} {
            continue
          }
          set lt " ($avalNum avals.)"
        }
      }

      set df [make_profile_data_file $profile] 
      if {$tracenum > 0} {
        set plot "$plot ,\\\n"
      }
      set plot "$plot \"$df\" title \"L = $L$lt\" with linespoints"
      incr tracenum
    }    
  }

  if {[string length $plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 1"
    puts $CHANNEL "set mytics 10"
    if {$isValueProfile} {
      puts $CHANNEL "set xlabel \"v_a\""
      puts $CHANNEL "set ylabel \"S(v_a)\""
      set pt "Value"
    } else {
      puts $CHANNEL "set xlabel \"n_a\""
      puts $CHANNEL "set ylabel \"S(n_a)\""
      set pt "Count"
    }
    set t [description_to_string $series $titleParamNames]
    if {[string length $t]} {
      set t "$t, "
    }
    puts $CHANNEL "set title \"Average $pt Profiles, $t[description_to_string $series $TITLE_PARAM_NAMES]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $plot"
  }
}

###############################################################################
# Repeats series_avg_profile_by_L_chart for all series in run
# Parameters:
#   rundir - experiment run directory
#   minL - min avalanche length
#   maxL - max avalanche length
#   stepL - length increment value
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
###############################################################################
proc run_series_avg_profile_by_L_chart { rundir minL maxL stepL titleParamNames {isValueProfile 0} {options ""} } {
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    series_avg_profile_by_L_chart $series $minL $maxL $stepL $titleParamNames $isValueProfile $options
  }
}

###############################################################################
# Returns number of lines in file
# Parameters:
#   fileName - full path to file
# Return:
#   number of lines or -1 if not available
###############################################################################
proc count_lines { fileName } {
  set s {}
  set l [string length $fileName]
  set suffix [string range $fileName [expr $l - 4] $l]
  if {$suffix == ".zip"} {
    set s [split [exec unzip -p $fileName | wc -l] " \t"]
  } else {
    set s [split [exec wc -l $fileName] " \t"]
  }

  if {[llength $s]} {
    return [expr 1 * [lindex $s 0]]
  } else {
    return -1
  }
}

###############################################################################
# Returns number of avalanches in profile data file
# Parameters:
#   profileFileName - full path to profile file
# Return:
#   number of avalanches or -1 if not available
###############################################################################
proc calc_profile_length { profileFileName } {
  return [count_lines $profileFileName]
}

###############################################################################
# Makes name of the profile file name from series direcrory name
# Parameters:
#   seriesdir - series directory
#   L - profile length
#   isValueProfile - true - display value profiles, count profiles otherwise
#   isZipped - true - file is zipped
# Return:
#   data file name
###############################################################################
proc make_profile_file_name { seriesdir L {isValueProfile 0} {isZipped 0} } {
  if {$isValueProfile} {
    set fn "value-profile" 
  } else {
    set fn "count-profile" 
  }
  if {$isZipped} {
    set sf "zip"
  } else {
    set sf "txt"
  }
  set ls $L
  for {set nl 3} {$nl <= 5} {incr nl} {
    while {[string length $ls] < $nl} {
      set ls "0$ls"
    }
    set res "$seriesdir[file separator]$fn.$ls.$sf"
    if { [file exists $res] } {
      return $res
    }
  }
  return $res; 
}

###############################################################################
# Makes name of the average profile file name from series direcrory name
# Parameters:
#   seriesdir - series directory
#   isValueProfile - true - display value profiles, count profiles otherwise
# Return:
#   data file name
###############################################################################
proc make_avg_profile_file_name { seriesdir {isValueProfile 0} } {
  set serNum [get_series_num $seriesdir]
  if {$isValueProfile} {
    set fn "value-profile" 
  } else {
    set fn "count-profile" 
  }
  set parts [file split $seriesdir]
  return "[join [lrange $parts 0 [expr [llength $parts] - 2]] [file separator]][file separator]avg.$fn.$serNum.txt"
}

###############################################################################
# Creates several avegare profile charts
# Charts display average profiles of the same length for each series
# Parameters:
#   rundir - directory name with experiment run to build chart for
#   Ls - list of required avalanche lengths
#   legendParamNames - names of series parameters to put into legend
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
###############################################################################
proc run_avg_profile_by_Ls_chart { rundir Ls legendParamNames {isValueProfile 0} {options ""} } {

  foreach L $Ls {
    run_avg_profile_by_L_chart $rundir $L $legendParamNames $isValueProfile $options
  }

}

###############################################################################
# Creates <L>(Pt) chart
# One chart per experiment run
# Parameters:
#   rundir - directory name with experiment run to build chart for
#   options - optional GNUPLOT commands
###############################################################################
proc run_avgL_by_Pt_chart { rundir {options ""} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
    
  set seriesDirs [find_series $rundir]
  set tracenum 0
  set plot ""
  set data ""
  
  foreach series $seriesDirs {
    set serNum [get_series_num $series]
    set fileName "$series[file separator]l-stat.txt"
    if {![file readable $fileName] || ![file size $fileName]} {
      continue;
    } 
    set f [open $fileName r+]
    if {[string length $data]} {
      set data "$data\n"
    }
    set data "$data[string trim [read $f]]"
    close $f
  }
  if {[string length $data]} {
    set df [temp_file]
    set f [open $df w]
    puts $f $data
    close $f

    # try to retrieve common <m> parameter    
    array set runparams {}
    read_description $rundir runparams
    if {[array size runparams] && [info exists runparams(m)]} {
      set m $runparams(m)
    } else {
      set m 0
    }
    
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 1"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set ylabel \"[refine_param_name <L>]\""
    puts $CHANNEL "set title \"[refine_param_name <L>]([refine_param_name P_t]), [description_to_string $rundir $TITLE_PARAM_NAMES]\""
    if {$m} {
      puts $CHANNEL "set xlabel \"1/2m[refine_param_name P_t]\""
      puts $CHANNEL $options
      puts $CHANNEL "plot \"$df\" using (1 / ([expr 2 * $m] * \$1)):2:3 title \"\" with errorlines"
    } else {
      puts $CHANNEL "set xlabel \"1/[refine_param_name P_t]\""
      puts $CHANNEL $options
      puts $CHANNEL "plot \"$df\" using (1 / \$1):2:3 title \"\" with errorlines"
    }
  }
}

###############################################################################
# Extract line with profile data of given length
# Parameters:
#   fileName - average profile file name
#   L - profile length required
# Return:
#   list with profile values
###############################################################################
proc extract_avg_profile { fileName L } {
  set res {}
  
  if {[file readable $fileName]} {
    set f [open $fileName r+]
    foreach line [split [read $f] "\n"] {
      if { ![string match {\s\#} $line] } {
        set nums [split $line " \t"]
        if {[llength $nums] == $L} {
          set res $nums
          break
        }
      }
    }
    close $f
  }
  
  return $res
}

###############################################################################
# Checks whether given file is zipped
# Parameters:
#   fileName - full path to file
# Return:
#   true - file is zipped
###############################################################################
proc isFileZipped { fileName } {
  return [regexp -nocase "^.+\\.zip$" $fileName]
}

###############################################################################
# Extract line with profile data
# Parameters:
#   fileName - profile file name
#   index - zero-based profile number
# Return:
#   list with profile values
###############################################################################
proc extract_profile { fileName index } {
  set res {}
  
  if {[file readable $fileName]} {
    if { [isFileZipped $fileName] } {
      set f [open "|unzip -p $fileName" r+]
      set i -1
      while { [gets $f line] >= 0 } {
        # we have to read all lines from pipe to avoid "child killed" error
        if { ![regexp "^\\s*#" $line] } {
          set data [split $line]
          if { [llength $data] } {
            # data line detected
            incr i
            if { $i == $index } {
              set res $data
            }
          }
        }
      }
      close $f
    } else {
      set f [open $fileName r+]
      for {set i -1} { $i < $index } {} {
        set line [gets $f]
        if { ![regexp "^\\s*#" $line] } {
          set data [split $line]
          if { [llength $data] } {
            # data line detected
            incr i
            if { $i == $index } {
              set res $data
              break
            }
          }
        }
      }
      close $f
    }
  }
  
  return $res
}

###############################################################################
# Creates temporary file with profile data
# Parameters:
#   profile - list with profile values
# Return:
#   data file name
###############################################################################
proc make_profile_data_file { profile } {
  set fn [temp_file]
  set f [open $fn w]
  set n 1
  foreach v $profile {
    puts $f "$n\t$v"
    incr n
  }
  close $f
  return $fn
}

###############################################################################
# Reads and parses directory description file
# Parameters:
#   dir - directory to find description in
#   desc - name of array variable to place parameters in
###############################################################################
proc read_description { dir desc } {
  global ::nettcl::chart::README_FILE_NAME;
  set fileName "$dir[file separator]$README_FILE_NAME"
  if {[file readable $fileName]} {
    upvar $desc dest
    
    set channel [open $fileName r]
    foreach line [split [read $channel] "\n"] {
      if { [string match {*=*} $line] } {
        ::set line [split $line =]
        ::set key [string trim [lindex $line 0]]
        if { $key == "" } { continue }
        ::set value [string trim [join [lrange $line 1 end] =]]
        if { $value == "" } { continue }
        ::set dest($key) $value
      }
    }
    close $channel
  }
}

###############################################################################
# Extracts parameter value
# Parameters:
#   dir - directory to find readme.txt in
#   name - parameter name
#   defValue - default parameter value
# Returns:
#   parameter value or empty string if not found
###############################################################################

proc parameter { dir name { defValue "" }  } {
    array set runparams {}
    read_description $dir runparams
	if { [info exists runparams($name)] } {
		set res $runparams($name)
	} else {
		set res $defValue
	}
	return $res
}

###############################################################################
# Prepares name of parameter for displaying depending on current terminal type
# Parameters:
#   pn - parameter name
# Return:
#   refined parameter name
###############################################################################
proc refine_param_name { pn } {
  global ::nettcl::chart::TERMINAL
  global ::nettcl::chart::PARAM_NAME_SUBSTITUTION
  
  set tt [lindex [split $TERMINAL " "] 0]
  if { $tt == "pdf" || $tt == "postscript"} {
    if {[info exists PARAM_NAME_SUBSTITUTION($pn)]} {
      return $PARAM_NAME_SUBSTITUTION($pn)
    }
  }
  
  return $pn
}

###############################################################################
# Reads description file and puts all or given parameters to single string
# Parameters:
#   dir - directory to find description in
#   paramNames - list of parameter names to add. If omitted, all parameters are added
#   omittedParamNames - list of parameter names to omit.
###############################################################################
proc description_to_string { dir {paramNames {}} {omittedParamNames {}} } {
  set res ""
  array set params {}
  read_description $dir params
  if {[array size params]} {
    if {[llength $paramNames]} {
      foreach pn $paramNames {
        if {[info exists params($pn)] && [lsearch $omittedParamNames $pn] < 0} {
          if {[string length $res]} {
            set res "$res, "
          }
          set v $params($pn)
          set res "$res[refine_param_name $pn]=$v"
        }
      }
    } else {
      set names [array names params]
      foreach pn $names {
        if {[lsearch $omittedParamNames $pn] < 0} {
          if {[string length $res]} {
            set res "$res, "
          }
          set v $params($pn)
          set res "$res[refine_param_name $pn]=$v"
        }
      }
    }
  }

  return $res
}

###############################################################################
# Creates name of the chart file depending on name of the specified directory
# Parameters:
#   dirpath - path to the directory
# Return:
#   recommended chart file name
###############################################################################
proc auto_chart_name { dirpath } {
  global ::nettcl::chart::TERMINAL

  set curdir [pwd]
  cd $dirpath
  set dirname [pwd]
  cd $curdir

  set res ""
  set parts [split $dirname [file separator]]
  set i [expr [llength $parts] - 1]
  
  while {$i >= 0} {
    set s [lindex $parts $i]
    if {[string match "series*" $s]} {
      set res "[string range $s 6 8]"
    } elseif {[string match "run*" $s]} {
      if {[string length $res]} {
        set res ".$res" 
      }
      set res "[string range $s 3 4]$res"
    } elseif {[string match "exp*" $s]} {
      if {[string length $res]} {
        set res ".$res" 
      }
      set res "[string range $s 3 5]$res"
      break;
    }
    incr i -1
  } 

  set tts [split $TERMINAL " "]
  set tt [lindex $tts 0]
  if {$tt == "postscript"} {
    if {[lindex $tts 1] == "enhanced"} {
      set tt "eps"
    } else {
      set tt "ps"
    }
  }

  return "charts$res.$tt"
}

###############################################################################
# Returns number of avalanches in profile
# Parameters:
#   seriesDir - path to series directory
#   L - profile length
#   isValueProfile - true - display value profiles, count profiles otherwise
# Return:
#   number of avalanches or -1 if not available
###############################################################################
proc get_profile_aval_count { seriesDir L {isValueProfile 0} }  {
  set src [make_profile_file_name $seriesDir $L $isValueProfile 0]
  if {![file readable $src]} {
    set src [make_profile_file_name $seriesDir $L $isValueProfile 1]
  }
  if {[file readable $src]} {
    set avalNum [calc_profile_length $src]
    if {$avalNum >= 0} {
      return $avalNum
    }
  }
  return -1
}

###############################################################################
# Writes list to temporary file
# Parameters
#   src - list to write
#   numberLines - true - write line number prior to data
#   firstNumber - number of first line (used when numberLines is true)
#   maxPoints - max number of points to write, 0 - unlimited (default is 0)
# Return:
#   temporary file name
###############################################################################
proc write_list_to_tmp_file { src {numberLines 0} {firstNumber 0} {maxPoints 0} } {
  set filename [temp_file]
  set fileid [open $filename w]
  set num 0
  foreach v $src {
    if { $numberLines } {
      puts -nonewline $fileid "$firstNumber\t"
      incr firstNumber
    }
    if { [catch {llength $v} ] } {
      foreach vv $v {      
        puts $fileid -nonewline "$vv\t"
      }
      puts ""
    } else {
      puts $fileid $v
    }
    
    incr num
    if { $maxPoints > 0 && $maxPoints <= $num } {
      break
    }
  }
  close $fileid
  return $filename
}

###############################################################################
# Makes chart title. Puts specified parameters first, then puts default parameters
# Parameters:
#   dirname - path to directory with readme.txt
#   titleParamNames - list of parameters
###############################################################################
proc make_chart_title { dirname {titleParamNames {} } } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  
  set res ""
  if { [llength $titleParamNames] } {
    set res [description_to_string $dirname $titleParamNames]
    if {[string length $res]} {
      set res "$res, "
    }
  }
  return "$res[description_to_string $dirname $TITLE_PARAM_NAMES $titleParamNames]"
}

###############################################################################
# Returns string describing profile type
###############################################################################
proc describe_profile_type {isValueProfile} {
  if {$isValueProfile} {
    return "Value"
  } else {
    return "Count"
  }
}

###############################################################################
# Creates avegare profile autocorrelation and fourier charts
# Chart displays several profiles of the same series with different avalanche lengths
# Parameters:
#   series - series directory
#   Ls - list of required avalanche lengths
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
#   maxPoints - max points to display on chart
###############################################################################
proc series_avg_profile_analysis { series Ls titleParamNames {isValueProfile 0} {options ""} {maxPoints 0} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM

  set tracenum 0
  set profile_plot ""
  set acf_plot ""
  set dft_plot ""
  
  foreach L $Ls {
    set profileFileName [make_avg_profile_file_name $series $isValueProfile]
    if {![file readable $profileFileName] || ![file size $profileFileName]} {
      continue;
    } 
    set profile [extract_avg_profile $profileFileName $L]
    if {[llength $profile]} {
      # try to retrieve number of avalanches in profile
      set lt ""
      set avalNum [get_profile_aval_count $series $L $isValueProfile]
      if {$avalNum >= 0} {
        if {$avalNum < $PROFILE_CHART_MIN_AVAL_NUM} {
          continue
        }
        set lt " ($avalNum avals.)"
      }

      set acf [::math::statistics::autocorr $profile]
      set dft [::math::fourier::dft $acf]

      if {$tracenum > 0} {
        set profile_plot "$profile_plot,\\\n"
        set acf_plot "$acf_plot,\\\n"
        set dft_plot "$dft_plot,\\\n"
      }
      set profile_plot "$profile_plot \"[write_list_to_tmp_file $profile 1 1 $maxPoints]\" using 1:2 title \"L = $L$lt\" with lines"
      set acf_plot "$acf_plot \"[write_list_to_tmp_file $acf 1 0 $maxPoints]\" using 1:2 title \"L = $L$lt\" with lines"
      set dftFileName [write_list_to_tmp_file $dft 1 0]
      set dft_plot "$dft_plot \"$dftFileName\" using 1:2 title \"RE, L = $L$lt\" with lines"
      set dft_plot "$dft_plot, \"$dftFileName\" using 1:3 title \"IM, L = $L$lt\" with lines"
      incr tracenum
    }    
  }

  if { $maxPoints > 0 } {
    set maxPointsHdr " ($maxPoints points)"
  } else {
    set maxPointsHdr ""
  }
    
  if {[string length $profile_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 1"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"[refine_param_name na]\""
    puts $CHANNEL "set ylabel \"S([refine_param_name na])\""
    puts $CHANNEL "set title \"Average [describe_profile_type $isValueProfile] Profile, [make_chart_title $series $titleParamNames]$maxPointsHdr\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $profile_plot"
  }
  if {[string length $acf_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"r\""
    puts $CHANNEL "set ylabel \"ACF(r)\""
    puts $CHANNEL "set title \"Autocorrelation of Average [describe_profile_type $isValueProfile] Profile, [make_chart_title $series $titleParamNames]$maxPointsHdr\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $acf_plot"
  }
  if {[string length $dft_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
#    puts $CHANNEL "set xlabel \"r\""
#    puts $CHANNEL "set ylabel \"ACF(r)\""
    puts $CHANNEL "set title \"DFT of Autocorrelation of Average [describe_profile_type $isValueProfile] Profile, [make_chart_title $series $titleParamNames]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $dft_plot"
  }
}

###############################################################################
# Repeats series_avg_profile_analysis for each series in run 
# Parameters:
#   rundir - run directory
#   Ls - list of required avalanche lengths
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
#   maxPoints - max points to display on chart
###############################################################################
proc run_series_avg_profile_analysis { rundir Ls titleParamNames {isValueProfile 0} {options ""} {maxPoints 0} } {
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    series_avg_profile_analysis $series $Ls $titleParamNames $isValueProfile $options $maxPoints
  }
}

###############################################################################
# Creates sample profile autocorrelation and fourier charts
# Each chart displays first <sampleNum> profiles of given length from specified series
# Parameters:
#   series - series directory
#   Ls - list of required avalanche lengths
#   sampleNum - number of samples
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
#   maxPoints - max points to display on chart
###############################################################################
proc series_sample_profile_analysis { series Ls {sampleNum 3} {titleParamNames {}} {isValueProfile 0} {options ""} {maxPoints 0} } {
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM

  foreach L $Ls {
    set tracenum 0
    set profile_plot ""
    set acf_plot ""
    set dft_plot ""
    
    # access profile file
    set profileFileName [make_profile_file_name $series $L $isValueProfile 0]
    if { ![file exists $profileFileName] || ![file readable $profileFileName] || ![file size $profileFileName]} {
      set profileFileName [make_profile_file_name $series $L $isValueProfile 1]
      if { ![file exists $profileFileName] || ![file readable $profileFileName] || ![file size $profileFileName]} {
        continue
      }
    }
    
    for {set i 0} {$i < $sampleNum} {incr i} {
      set profile [extract_profile $profileFileName $i]
      if {[llength $profile]} {
        set acf [::math::statistics::autocorr $profile]
        set dft [::math::fourier::dft $acf]

        if {$tracenum > 0} {
          set profile_plot "$profile_plot,\\\n"
          set acf_plot "$acf_plot,\\\n"
          set dft_plot "$dft_plot,\\\n"
        }
        set legendTitle "Sample #$i"
        set profile_plot "$profile_plot \"[write_list_to_tmp_file $profile 1 1 $maxPoints]\" using 1:2 title \"$legendTitle\" with lines"
        set acf_plot "$acf_plot \"[write_list_to_tmp_file $acf 1 0 $maxPoints]\" using 1:2 title \"$legendTitle\" with lines"
        set dftFileName [write_list_to_tmp_file $dft 1 0]
        set dft_plot "$dft_plot \"$dftFileName\" using 1:2 title \"$legendTitle\" with lines"
        incr tracenum
      }    
    }
    
    if { $maxPoints > 0 } {
      set maxPointsHdr " ($maxPoints points)"
    } else {
      set maxPointsHdr ""
    }
      
    if {[string length $profile_plot]} {
      puts $CHANNEL "reset"
      puts $CHANNEL "set mxtics 1"
      puts $CHANNEL "set mytics 10"
      puts $CHANNEL "set xlabel \"[refine_param_name na]\""
      puts $CHANNEL "set ylabel \"S([refine_param_name na])\""
      puts $CHANNEL "set title \"[describe_profile_type $isValueProfile] Profile, L=$L, [make_chart_title $series $titleParamNames]$maxPointsHdr\""
      puts $CHANNEL $options
      puts $CHANNEL "plot $profile_plot"
    }
    if {[string length $acf_plot]} {
      puts $CHANNEL "reset"
      puts $CHANNEL "set mxtics 10"
      puts $CHANNEL "set mytics 10"
      puts $CHANNEL "set xlabel \"r\""
      puts $CHANNEL "set ylabel \"ACF(r)\""
      puts $CHANNEL "set title \"Autocorrelation of [describe_profile_type $isValueProfile] Profile, L=$L, [make_chart_title $series $titleParamNames]$maxPointsHdr\""
      puts $CHANNEL $options
      puts $CHANNEL "plot $acf_plot"
    }
    if {[string length $dft_plot]} {
      puts $CHANNEL "reset"
      puts $CHANNEL "set mxtics 10"
      puts $CHANNEL "set mytics 10"
  #    puts $CHANNEL "set xlabel \"r\""
  #    puts $CHANNEL "set ylabel \"ACF(r)\""
      puts $CHANNEL "set title \"DFT of Autocorrelation of [describe_profile_type $isValueProfile] Profile, L=$L, [make_chart_title $series $titleParamNames]\""
      puts $CHANNEL $options
      puts $CHANNEL "plot $dft_plot"
    }
  }
}

###############################################################################
# Repeats series_sample_profile_analysis for each series in run 
# Parameters:
#   rundir - run directory
#   Ls - list of required avalanche lengths
#   sampleNum - number of samples
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
#   maxPoints - max points to display on chart
###############################################################################
proc run_series_sample_profile_analysis { rundir Ls {sampleNum 3} {titleParamNames {}} {isValueProfile 0} {options ""} {maxPoints 0} } {
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    series_sample_profile_analysis $series $Ls $sampleNum $titleParamNames $isValueProfile $options $maxPoints
  }
}

###############################################################################
# Creates k-distribution chart code
# Parameters:
#   rundir - directory name with experiment run to build chart for
#   legendParamNames - names of series parameters to place into legend
#   options - optional GNUPLOT commands
###############################################################################
proc run_k_distr_chart { rundir legendParamNames {options ""} } {
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::NETWORK_FILE_NAME
    
  set seriesDirs [find_series $rundir]
  set tracenum 0
  set plot ""
  
  foreach series $seriesDirs {
    # load network
    set nf "$series[file separator]$NETWORK_FILE_NAME"
    if { ![file readable $nf] || ![file size $nf] } { continue }
    set net [network $nf]
    set limits [::nettcl::makeHistogramLimits2 1 [network_get $net kmin] [network_get $net kmax]]
    set df [temp_file]
    ::nettcl::writeHistogram [::math::statistics::histogram $limits [network_get $net klist]] $limits $df
    if {[file isfile $df] && [file size $df]} {
      if {$tracenum > 0} {
        set plot "$plot ,"
      }
      set plot "$plot \"$df\" using 1:2 title \"[description_to_string $series $legendParamNames]\" with lines"
      incr tracenum
    }
  }
  
  if {[string length $plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set logscale xy"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"k\""
    puts $CHANNEL "set ylabel \"p(k)\""
    puts $CHANNEL "set title \"K-distribution, [description_to_string $rundir $TITLE_PARAM_NAMES]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $plot"
  }
}

###############################################################################
# Returns given string zero-left-padded
#   s - source string
#   length - min length required
###############################################################################
proc zeroPad { s length } {
  while { [string length $s] < $length } {
    set s "0$s"
  }
  return $s
}

###############################################################################
# Constructs name of avalanche snapshot file
###############################################################################
proc makeSnapshotFileName { series L index } {

  set MAX_DIGITS_IN_LENGTH 5
  set MAX_DIGITS_IN_INDEX 5

  for { set ln 1 } { $ln <= $MAX_DIGITS_IN_LENGTH } { incr ln } {
    for { set in 1 } { $in <= $MAX_DIGITS_IN_INDEX } { incr in } {
      set fn "$series/snapshot.[zeroPad $L $ln].[zeroPad $index $in].txt"
      if { [file exists $fn] } {
        return $fn
      }
    }
  }

  return ""
}

###############################################################################
# Reads snapshot file line by line, calculates CRC of each line, 
# and puts into list
# Parameters:
#   fileName - snapshot file name
# Returns:
#   list of CRC's
###############################################################################
proc makeSnapshotCRC { fileName } {
  set result {}
  
  set f [open $fileName r+]
  while { [gets $f s] > 0 } {
    lappend result [::crc::crc16 $s]
  }
  close $f
  
  return $result
}

###############################################################################
# Creates snapshot autocorrelation chart
# Each chart displays first <sampleNum> profiles of given length from specified series
# Parameters:
#   series - series directory
#   Ls - list of required avalanche lengths
#   sampleNum - number of samples
#   maxPoints - max points to display on chart (0 - umlimited)
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   options - optional GNUPLOT commands
###############################################################################
proc series_snapshot_analysis { series Ls {sampleNum 3} {maxPoints 0} {titleParamNames {}} {options ""} } {
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM

  foreach L $Ls {
    set tracenum 0
    set acf_plot ""
    
    for {set i 0} {$i < $sampleNum} {incr i} {
      set snapshotFileName [makeSnapshotFileName $series $L $i]
      if { [file exists $snapshotFileName] && [file readable $snapshotFileName] && [file size $snapshotFileName] > 0 } {
        set crcData [makeSnapshotCRC $snapshotFileName]
        if {[llength $crcData]} {
          set acf [::math::statistics::autocorr $crcData]

          if {$tracenum > 0} {
            set acf_plot "$acf_plot,\\\n"
          }
          set legendTitle "Sample #$i"
          set acf_plot "$acf_plot \"[write_list_to_tmp_file $acf 1 0 $maxPoints]\" using 1:2 title \"$legendTitle\" with lines"
          incr tracenum
        }    
      }
    }
    
    if {[string length $acf_plot]} {
      if { $maxPoints > 0 } {
        set maxPointsHdr " ($maxPoints points)"
      } else {
        set maxPointsHdr ""
      }
      
      puts $CHANNEL "reset"
      puts $CHANNEL "set mxtics 10"
      puts $CHANNEL "set mytics 10"
      puts $CHANNEL "set xlabel \"r\""
      puts $CHANNEL "set ylabel \"ACF(r)\""
      puts $CHANNEL "set title \"Autocorrelation of Snapshot CRC, L=$L, [make_chart_title $series $titleParamNames]$maxPointsHdr\""
      puts $CHANNEL $options
      puts $CHANNEL "plot $acf_plot"
    }
  }
}

###############################################################################
# Repeats series_snapshot_analysis for each series in run 
# Parameters:
#   rundir - run directory
#   Ls - list of required avalanche lengths
#   sampleNum - number of samples
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   options - optional GNUPLOT commands
###############################################################################
proc run_series_snapshot_analysis { rundir Ls {sampleNum 3} {maxPoints 0} {titleParamNames {}} {options ""} } {
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    series_snapshot_analysis $series $Ls $sampleNum $maxPoints $titleParamNames $options
  }
}

###############################################################################
# Creates grid2d snapshot chart
# Parameters:
#   series - series directory
#   Ls - list of required avalanche lengths
#   sampleNum - number of sample avalanches
#   startIteration - starting avalanche iteration to display, zero-based (default is 0)
#   maxIterations - max iterations to display, 0 - unlimited (default is 0)
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   options - optional GNUPLOT commands
###############################################################################
proc series_grid2d_snapshot { series Ls {sampleNum 3} {startIteration 0} {maxIterations 0} {titleParamNames {}} {options ""} } {
  global ::nettcl::chart::CHANNEL

  foreach L $Ls {
    for {set i 0} {$i < $sampleNum} {incr i} {
      set snapshotFileName [makeSnapshotFileName $series $L $i]
      if { [file exists $snapshotFileName] && [file readable $snapshotFileName] && [file size $snapshotFileName] > 0 } {
        array set sparams {}
        read_description $series sparams
        if { [array size sparams] == 0 || ![info exists sparams(N)] || ![info exists sparams(grid2d_rows)] } {
          continue;
        }
        set N $sparams(N)
        set rows $sparams(grid2d_rows)
        if { $N <= 0 || $rows <= 0 } {
          continue;
        }
        
        set columns [expr int($N / $rows)]
        
        set f [open "$snapshotFileName" r+]
        set iterationCounter 1
        set cnt 0
        while { [gets $f line] >= 0 } {
          if { $cnt >= $startIteration } {
            set data {}
            
            foreach node [split $line] {
              if {[string range $node 0 1] == "-"} {
                set v [expr -$iterationCounter]
                set node [expr -$node]
              } else {
                set v $iterationCounter
              }
              set datapoint [list [expr int($node % $columns)] [expr int($node / $columns)] $v]
              lappend data $datapoint
            }
            
            if { [info exists prevData] } {
              puts $CHANNEL "reset"
              puts $CHANNEL "set title \"2-d Grid Aval. Snapshot, Aval #$i, It #$cnt, L=$L, [make_chart_title $series $titleParamNames]\""
              puts $CHANNEL "set xrange \[0:[expr $columns - 1]\]"
              puts $CHANNEL "set yrange \[0:[expr $rows - 1]\]"
              puts $CHANNEL "set xlabel \"columns\""
              puts $CHANNEL "set ylabel \"rows\""
              puts $CHANNEL "set xtics 5"
              puts $CHANNEL "set ytics 5"
              puts $CHANNEL "set mxtics 5"
              puts $CHANNEL "set mytics 5"
              #puts $CHANNEL "set grid xtics mxtics ytics mytics"
              puts $CHANNEL "set size ratio -1"
              #puts $CHANNEL "set palette negative nops_allcF maxcolors 0 gamma 0.75 gray"
              puts $CHANNEL "set view map"
              puts $CHANNEL $options
              puts $CHANNEL "splot \"[write_list_to_tmp_file [concat $prevData $data]]\" title \"\" with points palette pt 5 ps 1.5"
            }
            
            set prevData $data
        
            if { $maxIterations >0 && $maxIterations <= $iterationCounter } {
              break
            }
            incr iterationCounter
          }
          incr cnt
        }
        close $f
      }
    }
  }
}

###############################################################################
# Calculates profile density distribution
# Each chart displays density distribution for each profile length
# Parameters:
#   series - series directory
#   Ls - list of required avalanche lengths
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   isValueProfile - true - display value profiles, count profiles otherwise
#   options - optional GNUPLOT commands
###############################################################################
proc run_profile_chi_distr { rundir Ls {legendParamNames {}} {isValueProfile 0} {options ""} } {
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM
  
  set NUM_OF_POINTS 50

  foreach L $Ls {
    set tracenum 0
    set profile_plot ""

    set seriesDirs [find_series $rundir]
    foreach series $seriesDirs {
      # access profile file
      set profileFileName [make_profile_file_name $series $L $isValueProfile 0]
      if { ![file exists $profileFileName] || ![file readable $profileFileName] || ![file size $profileFileName]} {
        set profileFileName [make_profile_file_name $series $L $isValueProfile 1]
        if { ![file exists $profileFileName] || ![file readable $profileFileName] || ![file size $profileFileName]} {
          continue
        }
      }
      
      set chis [::nettcl::calcXis $profileFileName]

      if { [llength $chis] > 0 } {
        set stats [::math::statistics::basic-stats $chis]
        set min [lindex $stats 1]
        set max [lindex $stats 2]
        set lim [::nettcl::makeHistogramLimits2 [expr 1.0 / $NUM_OF_POINTS * ($max - $min)] $min $max]
        set dataFile [temp_file]
        ::nettcl::writeHistogram [::math::statistics::histogram $lim $chis] $lim $dataFile
        if {$tracenum > 0} {
          set profile_plot "$profile_plot,\\\n"
        }
        set profile_plot "$profile_plot \"$dataFile\" using 1:2 title \"[description_to_string $series $legendParamNames]\" with lines"
        incr tracenum
      }
    }
    #!!!
    if {[string length $profile_plot]} {
      puts $CHANNEL "reset"
      puts $CHANNEL "set xlabel \"[refine_param_name xi]\""
      puts $CHANNEL "set ylabel \"p([refine_param_name xi])\""
      puts $CHANNEL "set title \"[refine_param_name xi] Distr., L=$L, [make_chart_title $rundir]\""
      puts $CHANNEL $options
      puts $CHANNEL "plot $profile_plot"
    }
  }
}

###############################################################################
# Displays chart per series with average value and standard deviation
# Assumes following format of data files
# Column #1: independent variable
# Column #2: average value
# Column #2: standard deviation
# Parameters:
#   rundir - run directory
#   dataFileName - name of the data files within series directory
#   title - chart title
#   varTitle, averageTitle, stdTitle - axis titles
#   titleParamNames - names of parameters to display in chart title
#   options - optional gnuplot commands
###############################################################################
proc run_avg_with_std { rundir dataFileName title varTitle averageTitle stdTitle {titleParamNames {}} {options ""} } {
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM
  
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    set fn "$series/$dataFileName"
    if { [ file exists $fn ] && [ file readable $fn ] } {
      puts $CHANNEL "reset"
      puts $CHANNEL "set xlabel \"[refine_param_name $varTitle]\""
      puts $CHANNEL "set xtics autofreq"
      puts $CHANNEL "set mxtics 10"
      puts $CHANNEL "set ylabel \"[refine_param_name $averageTitle]\""
      puts $CHANNEL "set ytics nomirror"
      puts $CHANNEL "set mytics 10"
      puts $CHANNEL "set y2label \"[refine_param_name $stdTitle]\""
      puts $CHANNEL "set y2tics autofreq"
      puts $CHANNEL "set my2tics 10"
      puts $CHANNEL "set title \"$title, [make_chart_title $series $titleParamNames]\""
      puts $CHANNEL $options
      puts $CHANNEL "plot \"$fn\" using 1:2 axes x1y1 title \"[refine_param_name $averageTitle]\" with lines, \"$fn\" using 1:3 axes x1y2 title \"[refine_param_name $stdTitle]\" with lines"
    }
  }
}

###############################################################################
# Creates sample S(t) autocorrelation and fourier charts
# Each chart displays first <sampleNum> profiles of given length from specified series
# Parameters:
#   series - series directory
#   sampleNum - number of samples
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   options - optional GNUPLOT commands
#   maxSamplePoints, maxAcfPoints - max points to display on sample and ACF chart relatively
###############################################################################
proc series_sample_s_by_t_analysis { series {sampleNum 3} {titleParamNames {}} {options ""} {maxSamplePoints 0} {maxAcfPoints} } {
  global ::nettcl::chart::CHANNEL
  global ::nettcl::chart::PROFILE_CHART_MIN_AVAL_NUM

  set s_plot ""
  set acf_plot ""
  set dft_plot ""
  set tracenum 0
  
  for {set i 0} {$i < $sampleNum} {incr i} {
    set fn "$series/sample.$i.txt"
    if { [file exists $fn] && [file readable $fn] && [file size $fn]} {
      set s [ ::nettcl::loadTrace $fn 1 ]
      # remove half of S(t) to avoid noise
      set s [ lrange $s [ expr int([ llength $s ] / 2) ] end ]
      set acf [::math::statistics::autocorr $s]
      set dft [::math::fourier::dft $acf]

      if {$tracenum > 0} {
        set s_plot "$s_plot,\\\n"
        set acf_plot "$acf_plot,\\\n"
        set dft_plot "$dft_plot,\\\n"
      }
      set legendTitle "Sample #$i"
      if { $maxSamplePoints > 0 } {
        set tmpname [ temp_file ]
        exec head -n $maxSamplePoints $fn > $tmpname
        set fn $tmpname
      }
      set s_plot "$s_plot \"$fn\" using 1:2 title \"$legendTitle\" with lines"
      set acf_plot "$acf_plot \"[write_list_to_tmp_file $acf 1 0 $maxAcfPoints]\" using 1:2 title \"$legendTitle\" with lines"
      set dftFileName [write_list_to_tmp_file $dft 1 0]
      set dft_plot "$dft_plot \"$dftFileName\" using 1:2 title \"$legendTitle\" with lines"
      incr tracenum
    }    
  }
  
  if { $maxSamplePoints > 0 } {
    set maxSamplePointsHdr " ($maxSamplePoints points)"
  } else {
    set maxSamplePointsHdr ""
  }
  if { $maxAcfPoints > 0 } {
    set maxAcfPointsHdr " ($maxAcfPoints points)"
  } else {
    set maxAcfPointsHdr ""
  }
 
  if {[string length $s_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 1"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"[refine_param_name t]\""
    puts $CHANNEL "set ylabel \"S([refine_param_name t])\""
    puts $CHANNEL "set title \"S(t), [make_chart_title $series $titleParamNames]$maxSamplePointsHdr\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $s_plot"
  }
  if {[string length $acf_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"{/Symbol t}\""
    puts $CHANNEL "set ylabel \"ACF\""
    puts $CHANNEL "set title \"ACF of stationary S(t), [make_chart_title $series $titleParamNames]$maxAcfPointsHdr\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $acf_plot"
  }
  if {[string length $dft_plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set title \"DFT of ACF of stationary S(t), [make_chart_title $series $titleParamNames]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $dft_plot"
  }
}

###############################################################################
# Repeats series_sample_s_by_t_analysis for each series in run 
# Parameters:
#   rundir - run directory
#   sampleNum - number of samples
#   titleParamNames - names of series parameters to put into title (in addition to standard params)
#   options - optional GNUPLOT commands
#   maxSamplePoints, maxAcfPoints - max points to display on sample and ACF chart relatively
###############################################################################
proc run_series_sample_s_by_t_analysis { rundir {sampleNum 3} {titleParamNames {}} {options ""} {maxSamplePoints 0} {maxAcfPoints 0} } {
  set seriesDirs [find_series $rundir]
  foreach series $seriesDirs {
    series_sample_s_by_t_analysis $series $sampleNum $titleParamNames $options $maxSamplePoints $maxAcfPoints
  }
}

###############################################################################
# Builds arbitrary distribution for all series in run
# Parameters:
#   rundir - run directory
#   title - chart title
#   xTitle - name of the veriable
#   dataClosure - closure which accepts "seriesDir" variable and returns data in "data" variable
#   n - number of points in histogramm
#   legendParamNames - parameters to display in legend
#   options - optional GNUPLOT commands
###############################################################################
proc run_data_distr { rundir title xTitle dataClosure {n {100}} {legendParamNames {}} {options ""} } {
  global ::nettcl::chart::CHANNEL
   global ::nettcl::chart::TITLE_PARAM_NAMES
 
  set seriesDirs [find_series $rundir]
  set plot ""
  set tracenum 0

  foreach series $seriesDirs {
    upvar 1 data data
    set data {}
    upvar 1 seriesDir seriesDir
    set seriesDir $series
    uplevel 1 $dataClosure
    if { [llength $data] } {
      set stats [::math::statistics::basic-stats $data ]
      set limits [::nettcl::makeHistogramLimits $n [lindex $stats 1] [lindex $stats 2] ]
      set tempFn [ temp_file ]
      ::nettcl::writeHistogram [::math::statistics::histogram $limits $data] $limits $tempFn  
      if {$tracenum > 0} {
        set plot "$plot,\\\n"
      }
      set plot "$plot \"$tempFn\" using 1:2 title \"[description_to_string $series $legendParamNames]\" with lines"
      incr tracenum
    }
  }

  if {[string length $plot]} {
    puts $CHANNEL "reset"
    puts $CHANNEL "set mxtics 10"
    puts $CHANNEL "set mytics 10"
    puts $CHANNEL "set xlabel \"[refine_param_name $xTitle]\""
    puts $CHANNEL "set ylabel \"p([refine_param_name $xTitle])\""
    puts $CHANNEL "set title \"$title, [make_chart_title $rundir]\""
    puts $CHANNEL $options
    puts $CHANNEL "plot $plot"
  }
}

###############################################################################
# Builds u_a distribution for all series in run
# Parameters:
#   rundir - run directory
#   n - number of points in histogramm
#   legendParamNames - parameters to display in legend
#   options - optional GNUPLOT commands
###############################################################################
proc run_s_by_t_average_distr { rundir n {legendParamNames {}} {options ""} } {
  run_data_distr $rundir "p(u_a)" "u_a" {
    set fn "$seriesDir/s-stat.txt"
    if { [file exists $fn] && [file readable $fn] && [file size $fn]} {
      set data [ ::nettcl::loadTrace $fn 0 ]
    }
  } $n $legendParamNames $options
}

###############################################################################
# Builds xi_a distribution for all series in run
# Parameters:
#   rundir - run directory
#   n - number of points in histogramm
#   legendParamNames - parameters to display in legend
#   options - optional GNUPLOT commands
###############################################################################
proc run_s_by_t_std_distr { rundir n {legendParamNames {}} {options ""} } {
  run_data_distr $rundir "p({/Symbol x}_a)" "{/Symbol x}_a" {
    set fn "$seriesDir/s-stat.txt"
    if { [file exists $fn] && [file readable $fn] && [file size $fn]} {
      set data [ ::nettcl::loadTrace $fn 1 ]
    }
  } $n $legendParamNames $options
}

proc write { text } {
	global ::nettcl::chart::CHANNEL

	set text [regsub -all "\n" $text "<EOL>"]
	set text [regsub -all "\"" $text "\\\""]
	set text [uplevel [list eval "set __result__ \"$text\""]]
	set text [regsub -all "<EOL>" $text "\n"]

	puts $CHANNEL $text 
}

proc plotList { lst } {
	set result ""

	foreach v $lst {
		if { [string length $result] > 0 } {
			append result ", "
		}
		append result $v
	}

	return $result
}

###############################################################################
# Creates chart file(s)
# Parameters:
###############################################################################
proc chart { chartfilename command } {
  global ::nettcl::chart::GNUPLOT
  global ::nettcl::chart::TERMINAL
  global ::nettcl::chart::TITLE_PARAM_NAMES
  global ::nettcl::chart::CHANNEL
  
  set scriptfile [temp_file] 
  set f [open $scriptfile w]
  set CHANNEL $f
  set args(rundir) "."
  set args(channel) $f
  
  puts $f "set terminal $TERMINAL"
  puts $f "set output \"$chartfilename\""
  
  eval "[$command args]"
  close $f
  
  exec $GNUPLOT $scriptfile  
  
  # clear temporary files created if any
  clear_temp
} 

}
