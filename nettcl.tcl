###############################################################################
# stdlib.nettcl --
#
#    Package for basic operation with network
#
# version 1.0:   initial implementation, spring 2008
###############################################################################

package provide nettcl 1.2.0

package require math::statistics 0.5

namespace eval nettcl {

###############################################################################
# Writes network in GraphViz undirected graph format 
# Parameters:
# network - network to be written
# filename - name of the destination file 
###############################################################################
proc writeGraph { network filename {params ""} } {
  upvar $params par
  
  set colorMode [expr {[array exists par] && [llength [array names par -exact colorMode]] > 0 ? $par(colorMode) : ""}]
  set min_size 0.1
  set max_size 0.6
  set yx_ratio 0.88

  set file [open $filename w]
  puts $file "graph G {"
  puts $file "graph \[truecolor, overlap=false, splines=true\]"
  puts $file "node \[label=\"\", shape=ellipse, style=filled, width=[ expr 0.5 * ($max_size + $min_size) ], height=[ expr $yx_ratio * 0.5 * ($max_size + $min_size) ]\]"
  puts $file "edge \[label=\"\"\]"
  puts $file ""

  set kmax [ network_get $network kmax ]
  set zc [ network_get $network zc ]
  
  set i 0;
  foreach node [ network_get $network nodelist ] {
    set value [node_get $node value] 
    set k [node_get $node k]
    set width [ expr $min_size + ($max_size - $min_size) * $k / $kmax ]
    set height [ expr $yx_ratio * ($min_size + ($max_size - $min_size) * $k / $kmax ) ]
    
    if { $colorMode == "lastInc" } {
      set lastInc [node_get $node lastincrement]
      if { $lastInc > 0 } {
        set color "ff0000"
      } elseif { $lastInc < 0 } {
        set color "0000ff"
      } else {
        set color "000000"
      }
    } else {
      if { $value >= 0 } {
        set c [ expr int(256.0 * $value / $zc) ]
        if { $c > 255 } {
          set c 255
        }
        set color "[ format %02x $c ]0000"
      } else {
        set c [ expr int(-256.0 * $value / $zc) ]
        if { $c > 255 } {
          set c 255
        }
        set color "0000[ format %02x $c ]"
      }
    }
    puts $file "$i \[color=\"#$color\", width=$width, height=$height\];";
    incr i;    
  }
  puts $file ""

  set i 0;
  foreach node [ network_get $network nodelist ] {
    foreach relation [ node_get $node relationlist ] {
      puts $file "$i -- [ lindex $relation 0 ];"
    }
    incr i;    
  }
  puts $file ""
  
  puts $file "}"
  close $file
}

###############################################################################
# Load trace file into list 
# Parameters:
# filename - name of the trace file 
# tracenum - number of the trace to be loaded (zero-based)
#            if not specified, first (zero) trace is loaded
# Return value:
# list instance with trace data 
###############################################################################
proc loadTrace { filename tracenum } {
  set res [list]
  if { [catch {open $filename r} file] } {
      puts stderr "Could not open $filename for reading\n$file"
      exit 1
  }
    
  while {[ gets $file line ] >= 0} {
    set line [string trim $line]
    if { ![string length $line] } {
      continue
    }
    # check for comment
    if {[string first # $line] == -1} {
      lappend res [expr 1 * [lindex [split $line] $tracenum] ]
    }
  }
    
  close $file
  return $res;
}

###############################################################################
# Makes histogram limits for using in ::math::statistics::histogram   
# Parameters:
# n - number of histogram cells (must be greater than 1) 
# min - min possible value
# max - max possible value
###############################################################################
proc makeHistogramLimits { n min max } {
  if { $n < 2 } {
  	return -code error -errorcode ARG -errorinfo {n is less than 2} {n is less than 2}
  }
  
  set res {}

  set n [expr int($n) - 1]
  for { set i 0 } { $i <= $n } { incr i } {
    lappend res [expr ($max - $min) / $n * $i + $min]
  }
  
  return $res
}

###############################################################################
# Makes histogram limits for using in ::math::statistics::histogram   
# Parameters:
# precision - histogram step 
# min - min possible value
# max - max possible value
###############################################################################
proc makeHistogramLimits2 { precision min max } {
  set min [expr int($min / $precision) * $precision]
  set max [expr (int($max / $precision) + 1) * $precision]
  return [makeHistogramLimits [expr int(($max - $min) / $precision + 0.5) + 1] $min $max]
}

###############################################################################
# Makes data histogram
# Behaves like ::math::statistics::histogram but notmalized values are stored
# Parameters:
# limits - histogram limits
# values - data values
###############################################################################
proc histogram { limits values } {
  set res {}
  
  set src [::math::statistics::histogram $limits $values]
  set sum 0.0
  
  # first run - calculate total sum
  for { set i 0 } { $i < [llength $src] } { incr i } {
    set sum [expr $sum + [lindex $src $i]]
  }
  
  # second run - make result histogram
  for { set i 0 } { $i < [llength $src] } { incr i } {
    lappend res [expr [lindex $src $i] / $sum] 
  }
  
  return $res
}

###############################################################################
# Writes histogram made by ::math::statistics::histogram to file   
# Parameters:
# histogram - result of ::math::statistics::histogram
# limits - histogram limits
# filename - destination file name
###############################################################################
proc writeHistogram { histogram limits filename } {
  set file [open $filename w]
  
  set sum 0.0
  
  for { set i 0 } { $i < [llength $histogram] } { incr i } {
    if { $i > 0 || $i < [llength $histogram] - 1 } {
      set sum [expr $sum + [lindex $histogram $i]]
    }
  }
  
  set cumul 0.0
  puts $file "# Value\tf(value)\tF(value)\t# values" 
  for { set i 0 } { $i < [llength $limits] } { incr i } {
    set v [expr 1.0 * [lindex $histogram $i] / $sum]
    set cumul [expr $cumul + $v]
    puts $file "[lindex $limits $i]\t${v}\t${cumul}\t[lindex $histogram $i]"
  }
  
  close $file
}

###############################################################################
# Makes network critical   
# Parameters:
# net - network instance
# dh - estimated perturbation value 
###############################################################################
proc makeCritical { net dh } {
  set N [network_get $net size]
  set zc [network_get $net zc]
  set maxdh [expr 10.0 * [network_get $net avgload]]
  set dh2 [expr $zc / 10.0]
  if { $dh2 > $maxdh } {
    set dh2 $maxdh
  }
  set limits [makeHistogramLimits 11 [expr -$zc] $zc]
  set std [::math::statistics::stdev [network_get $net hlist]]
  set avalfilename [network_get $net avalfilename]
  set tmpnetfilename [network_get $net tmpnetfilename]
  
  # disable result output
  network_set $net avalfilename ""
  network_set $net tmpnetfilename ""
  
  # rough perturbation - quickly go into critical state
  for { set i 0 } { $i < 10 } { incr i } {
    set nruns [expr int(25.0 * $N * $zc / $dh2 / [network_get $net boundary])]
    if { $nruns < 1 } {
      set nruns 1
    }
    network_run $net $nruns $dh2 0.0 m1

    set hlist [network_get $net hlist]
    set distr [histogram $limits $hlist]
    set std2 [::math::statistics::stdev $hlist]

    if { [lindex $distr 1] >= 0.49 && [lindex $distr 10] >= 0.49 && [expr abs($std2 - $std) / $std2 ] < 0.01 } {
      break;
    }
    
    set std $std2  
  }

  # smooth perturbation - before statistics
  network_run $net [expr int($dh2 / $dh + 1)] $dh 0.0 m1

  # restore network settings
  network_set $net avalfilename $avalfilename
  network_set $net tmpnetfilename $tmpnetfilename
}

###############################################################################
# Calculates p(k, k') distribution
# where p(k, k') is a probability of that node with k relations has relation with node with k' relations
# Parameters:
# net - network instance
# params - parameters:
#   n - number of histograms, kmax by default
###############################################################################
proc calcPkk { net {params ""} } {
  upvar $params par
  
  set kmax [network_get $net kmax]
  set n [expr {[array exists par] && [llength [array names par -exact n]] > 0 ? $par(n) : $kmax}]
  
  ::struct::matrix m
  m add rows $n
  m add columns $n
  
  for {set i 0} {$i < $n} {incr i} {
    for {set j 0} {$j < $n} {incr j} {
      m set cell $i $j 0      
    }
  }
  
  set nodes [ network_get $net nodelist ]
  set relsum 0
  foreach node $nodes {
    set k [node_get $node k]
    set i [expr int([::tcl::mathfunc::floor [expr ($k - 1) * $n / $kmax]])]
    
    foreach relation [node_get $node relationlist] {
      set node2 [lindex $nodes [lindex $relation 0]]
      set j [expr int([::tcl::mathfunc::floor [expr ([node_get $node2 k] - 1) * $n / $kmax]])]
      m set cell $i $j [expr [m get cell $i $j] + 1]
      incr relsum
    }
  }
  
  for {set i 0} {$i < $n} {incr i} {
    for {set j 0} {$j < $n} {incr j} {
      m set cell $i $j [expr 1.0 * [m get cell $i $j] / $relsum]      
    }
  }

#  puts [m format 2string]
  
  for {set i 0} {$i < $n} {incr i} {
    for {set j 0} {$j < $n} {incr j} {
      puts "[expr int($kmax / $n * ($i + 1))]\t[expr int($kmax / $n * ($j + 1))]\t[m get cell $i $j]"
    }
  }
}

###############################################################################
# Calculates assortativity coefficient
# Parameters:
#   net - network instance
# Return:
#   assortativity coefficient(double)
###############################################################################
proc calcAssort { net } {
  set M 0
  set counter1 0.0
  set counter2 0.0
  set counter3 0.0
  
  set nodes [network_get $net nodelist]
  foreach node $nodes {
    set k [node_get $node k]
    
    foreach relation [node_get $node relationlist] {
      set node2 [lindex $nodes [lindex $relation 0]]
      set j [node_get $node2 k]
      
      incr M
      set counter1 [expr $counter1 + $k * $j]
      set counter2 [expr $counter2 + $k + $j]
      set counter3 [expr $counter3 + $k * $k + $j * $j]
    }
  }
  
  # avoid duplication of relations
  set M [expr $M / 2]
  set counter1 [expr $counter1 / 2]
  set counter2 [expr $counter2 / 2]
  set counter3 [expr $counter3 / 2]
  
  set s [expr (0.5 * $counter2 / $M) ** 2]
  return [expr ($counter1 / $M - $s) / (0.5 * $counter3 / $M - $s)]
}

###############################################################################
# Used internally by calcShellStatistics
###############################################################################
proc nodeShellStatistics { nodesName root resultName } {
  upvar $nodesName nodes
  upvar $resultName result

  array set passedNodes [list]
  set roots [list]
  lappend roots $root

  set stop 0
  for {set shell 0} {!$stop} {incr shell} {
    set stop 1
    set relations [list]
    
    foreach nodeIndex $roots {
      set node [lindex $nodes $nodeIndex]
      if {![info exists passedNodes($nodeIndex)]} {
        set stop 0
        set passedNodes($nodeIndex) 1
        set relationList [node_get $node relationlist]
        set k [llength $relationList]
        
        if {![info exists result($shell,hit)]} {
          # shell has never been stored yet
          set result($shell,hit) 1
          set result($shell,min) $k
          set result($shell,max) $k
          set result($shell,count) 1
          set result($shell,sum) $k
          if {![info exists result(max)] || $result(max) < $shell} {
            set result(max) $shell
          }
          if {![info exists result(min)]} {
            set result(min) $shell
          }
        } else {
          # update shell statistics
          if {$result($shell,min) > $k} {
            set result($shell,min) $k
          }
      
          if {$result($shell,max) < $k} {
            set result($shell,max) $k
          }
          
          incr result($shell,count)
          incr result($shell,sum) $k
        }
    
        if {![info exists result($shell,$k)]} {
          set result($shell,$k) 1 
        } else {
          incr result($shell,$k) 
        }
    
        foreach relation $relationList {
          set relNode [lindex $relation 0]
          lappend relations $relNode
        }
      }
      
      set roots $relations
    } 
  }
}

###############################################################################
# Calculates shell statistics of the specified network
# Works slow on large (more than 1000 nodes) networks especially with large <k>
# Parameters:
#   net - network instance
#   resultName - name of array to hold resulting statistics (will be cleared)
# Result:
#   resultName(min) - min shell number (usually 0)
#   resultName(max) - max shell number
#   resultName($shell,min) - min k value for given shell
#   resultName($shell,max) - max k value for given shell
#   resultName($shell,avg) - average k value for given shell
#   resultName($shell,$k) - probability of k for given shell
#   
###############################################################################
proc calcShellStatistics { net resultName } {
  upvar $resultName result
  
  if {[info exists result]} {
    unset result
  }
  array set result [list]

  set nodes [network_get $net nodelist]
  set N [llength $nodes]
  
  for {set nodeIndex 0} {$nodeIndex < $N} {incr nodeIndex} {
    nodeShellStatistics nodes $nodeIndex result
  }

  foreach name [array names result *,hit] {
    set shell [lindex [split $name ,] 0]
    set min $result($shell,min)
    set max $result($shell,max)
    
    for {set k $min} {$k <= $max} {incr k} {
      if {![info exists result($shell,$k)]} {
        set result($shell,$k) 0
      }
      set result($shell,$k) [expr 1.0 * $result($shell,$k) / $result($shell,count)]
    }
    
    set result($shell,avg) [expr 1.0 * $result($shell,sum) / $result($shell,count)]
  }
  
  array unset result *,count 
  array unset result *,sum 
  array unset result *,hit 
}

###############################################################################
# Calculates critical value of Pt
# Parameters:
#   net - network instance
# Return:
#   Pc (double)
###############################################################################
proc calcPc { net } {
  set s 0.0
  set s2 0.0
  
  set nodes [network_get $net nodelist]
  foreach node $nodes {
    set k [node_get $node k]
    set s [expr $s + $k]
    set s2 [expr $s2 + $k * $k]
  }
  
  if {$s2 > 0} {
    return [expr $s / $s2]
  } else {
    return 0.0;
  }
}

###############################################################################
# Makes network "mono-critical", when all nodes have value of the same sign
# and absolute values are near corresponding zc
# Parameters:
# net - network instance
# dh - estimated perturbation value 
# isPositive - true - make network "positively-critical", negative-critical otherwise
###############################################################################
proc makeMonoCritical { net dh isPositive } {
  set N [network_get $net size]
  if { !$isPositive } {
    set zc [network_get $net zcNeg]
  } else {
    set zc [network_get $net zcPos]
  }
  set maxdh [expr 10.0 * [network_get $net avgload]]
  set dh2 [expr $zc / 10.0]
  if { $dh2 > $maxdh } {
    set dh2 $maxdh
  }
  set avalfilename [network_get $net avalfilename]
  set tmpnetfilename [network_get $net tmpnetfilename]
  if { $isPositive } {
    set ratio 0.001
  } else {
    set ratio 1000
  }
  set pm [pertmethod create m4 $ratio]
  
  # disable result output
  network_set $net avalfilename ""
  network_set $net tmpnetfilename ""
  
  # rough perturbation - quickly go into critical state
  set stop 0
  for { set i 0 } { $i < 100 && !$stop } { incr i } {
    set nruns [expr int($N * $zc / $dh2)]
    if { $nruns < 1 } {
      set nruns 1
    }
    network_run $net $nruns $dh2 0.0 $pm

    set stop 1
    foreach h [network_get $net hlist] {
	  if { $h > 0 && $ratio >= 1.0 || $h < 0 && $ratio < 1.0 } {
        set stop 0
        break
      }
    }
  }

  # smooth perturbation - before statistics
  network_run $net [expr int($N * $dh2 / $dh + 1)] $dh 0.0 $pm

  # restore network settings
  network_set $net avalfilename $avalfilename
  network_set $net tmpnetfilename $tmpnetfilename
}

###############################################################################
# Changes H values of network nodes for average H to be given value
# Parameters:
# net - network instance
# h - required average H
###############################################################################
proc setAverageH { net h } {
  set ah [network_get $net avgh]
  set inc [expr 1.0 * ($h - $ah)]
  
  foreach node [network_get $net nodelist] {
    node_set $node value [expr $inc + [node_get $node value]]
  }
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
# Reads profile file line by line and calls custom reader procedure with parsed profile data
# Profile data are placed into "profile" variable in closure scope
# Parameters:
#   fileName - profile file name
#   closure - closure to call
###############################################################################
proc readProfile { fileName closure } {
  if { [file readable $fileName] } {
    if { [isFileZipped $fileName] } {
      set f [open "|unzip -p $fileName" r+]
    } else {
      set f [open $fileName r+]
    }

    while { [gets $f line] >= 0 } {
      if { ![regexp "^\\s*#" $line] } {
        set data [split $line]
        if { [llength $data] } {
          # data line detected
          upvar 1 profile profile
          set profile $data
          uplevel 1 $closure
        }
      }
    }
    close $f
  }
}

###############################################################################
# Reads given profile file, calculates and returns xi[i]
# xi = standard deviation of x[i], where x[i] - normalized number of excited 
#   nodes at the i-th iteration of avalanche
# Parameters:
#   profileFileName - name of file with profile data (plain, not zipped)
# Return:
#   - list of xi[i]
###############################################################################
proc calcXis { profileFileName } {
  set xis {}
  
  readProfile $profileFileName {
    # data are placed into variable "profile"
    set pstats [::math::statistics::basic-stats $profile]
    # xi is standard deviation on population of x[i]
    lappend xis [lindex $pstats 6]
  }
  
  return $xis
}

###############################################################################
# Reads given profile file, calculates xi[i] and returns statistics of xi values
# xi = standard deviation of x[i], where x[i] - normalized number of excited 
#   nodes at the i-th iteration of avalanche
# Parameters:
#   profileFileName - name of file with profile data (plain, not zipped)
# Return:
#   - result of ::math::statistics::basic-stats with xi statistics data
#   - "" if profile file does not exists or not readable
###############################################################################
proc calcXiStatistics { profileFileName } {
  set xis [calcXis $profileFileName]
  
  if { [llength $xis] > 0 } {
    return [::math::statistics::basic-stats $xis]
  } else {
    return ""
  }
}

###############################################################################
# Writes list to file
# Parameters
#   src - list to write
#   fileName - name of file to write in
#   numberLines - true - write line number prior to data
#   firstNumber - number of first line (used when numberLines is true)
#   maxPoints - max number of points to write, 0 - unlimited (default is 0)
###############################################################################
proc writeList { src fileName {numberLines 0} {firstNumber 0} {maxPoints 0} } {
  set fileid [open $fileName w]
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
}

# ::nettcl::matchVersion --
#
#	Compares current vdiff1d version with required one
# If current version is less than expected, terminates script execution
#
# Arguments:
#	  verRequired		string with min required version number, parts are seperated by comma, e.g. "1.2.3"
proc matchVersion { verRequired } {
  # retrieve current vdiff1d version
  set ver [ network_version ]
  # split given version by parts
  set expected [ split $verRequired . ]
  for { set i 0 } { $i < [ llength $expected ] } { incr i } {
    set n [ lindex $expected $i ]
    if { $n > 0 } {
      if { [ llength $ver ] < $i || [ lindex $ver $i ] < $n } {
        puts stderr "Version mismatch: vdiff1d $verRequired or later required"
        puts stderr "Current version: [join $ver .]"
        exit
      }
      if { [ lindex $ver $i ] > $n } {
        break
      }
    }
  }
}

###############################################################################
# Changes force values of network nodes for average force to be given value
# Parameters:
# net - network instance
# f - required average force
###############################################################################
proc setAverageForce { net f } {
  set nodes [network_get $net nodelist]

  set fsum 0.0
  foreach node $nodes {
    set fsum [expr $fsum + [node_get $node force]]
  }
  set fsum [expr $fsum / [llength $nodes]]

  set inc [expr 1.0 * $f - $fsum]
  foreach node [network_get $net nodelist] {
    node_set $node force [expr $inc + [node_get $node force]]
  }
}

###############################################################################
# Returns an argument of a complex number
# Parameters:
# x - real part
# y - imaginery part
# Return:
# Argument value
###############################################################################
proc arg { x y } {
  if { $x == 0.0 } {
    if { $y == 0.0 } {
      return 0.0
    } else {
      if { $y > 0.0 } {
        return 1.570796326794900
      } else {
        return -1.570796326794900
      }    
    }
  } else {
    if { $x > 0.0 } {
      return [expr atan($y / $x)]
    } else {
      if { $y >= 0 } {
        return [expr atan($y / $x) + 3.141592653589790]
      } else {
        return [expr atan($y / $x) - 3.141592653589790]
      }
    }
  }
}

###############################################################################
# Writes result of Fourier transformation to a file. 
# Only half of entire data will be actually written
# Parameters:
# src - list of (real, img) pairs as ::math::fourier::dft procedure returns
# fileName - output file name
# t - total sample time
###############################################################################
proc writeDft { src fileName t } {
  set fileid [open $fileName w]
  set i 0
  set max [expr int([llength $src] / 2)]
  set m [expr 6.283185307179590 / $t]
  puts $fileid "# w\tre\tim\tr\tphi"

  foreach v $src {
    set re [lindex $v 0]
    set im [lindex $v 1]
    puts $fileid "[expr $i * $m]\t$re\t$im\t[expr sqrt($re * $re + $im * $im)]\t[arg $re $im]"
    incr i
    if { $i >= $max } {
      break
    }
  }
  close $fileid
}

###############################################################################
# Writes result of autocorrelation function to a file
# Parameters:
# src - list of ACF values
# fileName - output file name
# dt - time step
###############################################################################
proc writeAcf { src fileName dt } {
  set fileid [open $fileName w]
  set i 0
  puts $fileid "# t\tACF"

  foreach v $src {
    puts $fileid "[expr $i * $dt]\t$v"
    incr i
  }
  close $fileid
}

###############################################################################
# Takes file name with two columns: time and periodic function value
# Calculates phase of the function assuming that the first data point has phase 0.0
# Parameters:
#   fileName - file name
#   timeIndex - time column index
#   valueIndex - value column index
# Return
#   Phase of -1 of phase is not determined
###############################################################################
proc detectPhase { fileName {timeIndex 0} {valueIndex 1} } {
	set times [loadTrace $fileName $timeIndex]
	set data [loadTrace $fileName $valueIndex]

	set prev ""; set pprev ""
	set result -1
	foreach t $times u $data {

		if { $prev != "" && $pprev != ""} {
			if { $pprev <= $prev && $u <= $prev} {
				set result [expr $tprev - [lindex $times 0]]
				break
			}
		}

		set pprev $prev
		set prev $u
		set tprev $t
	}

	return $result
}

###############################################################################
# Efficiently append a list to another list.
# Both lists are passed by a reference.
# Parameters:
#   destName - name of the variable holding resulting list.
#   appenderName - name of the variable holding list to append
###############################################################################
proc appendList { destName appenderName } {
	upvar $destName dest
	upvar $appenderName appender

	foreach v $appender {
		lappend dest $v
	}
}

###############################################################################
# Takes file name with two columns: time and periodic function value
# Calculates average and std over the period
# Parameters:
#   fileName - file name
#   timeIndex - time column index
#   valueIndex - value column index
# Return
#   list of following values:
#     - average period
#     - number of whole periods found
#     - time of the first maximum
#     - time of the last maximum
#     - result of ::math::statistics::basic-stats procedure calculated over all number within a whoule number of periods
###############################################################################
proc periodicFuncAnalysis { fileName { timeIndex 0 } { valueIndex 1 } } {
	set prev ""; set prevT ""
	set state ""
	set numbers {}
	set tmp unspecified
	set times {}

	readDataFile $fileName [list $timeIndex $valueIndex] {
		# numbers are in the `data' variable
		set t [lindex $data 0]
		set v [lindex $data 1]

		if { $prev != "" } {
			if { $v > $prev } {
				set state 1
			} elseif { $v < $prev } {
				if { $state != "" && $state != -1 } {
					if { $tmp != "unspecified" } {
						appendList numbers tmp
						lappend times $prevT
					}
					set tmp {}
				}
				set state -1
			} else {
				set state 0
			}
		}

		if { $tmp != "unspecified" } {
			lappend tmp $v
		}
		set prev $v
		set prevT $t
	}

	set nPeriods [expr [llength $times] - 1]
	set period [expr 1.0 * ([lindex $times end] - [lindex $times 0]) / $nPeriods]
	set result [list $period $nPeriods [lindex $times 0] [lindex $times end]]
	set stat [::math::statistics::basic-stats $numbers]
	appendList result stat
	return $result
}

###############################################################################
# Takes file name with several numeric columns.
# Read all data line by line.
# Calls given closure with an array of parsed numbers.
# Skips comment lines.
# Parameters:
#   fileName - file name
#   indices - list of zero-based column indices to read
#   closure - Piece of code to call. Data are passed within a special `data' variable.
# Return
#   number of valid data lines read
###############################################################################
proc readDataFile { fileName indices closure } {
	if { ![file readable $fileName] } {
		return ""
	}

	set f [open $fileName r]
	fconfigure $f -buffering line
    upvar 1 data data
	set n 0
	while { ![eof $f] } {
		if { [gets $f line] > 0 } {
			set values [split $line]
			set s [string trim [lindex $values 0]]
			if { [string match "#*" $s] } {
				continue
			}

			set data {}
			set n [llength $values]
			set n2 [llength $indices]
			foreach idx $indices {
				if { $idx < $n } {
					lappend data [lindex $values $idx]
				} else {
					break
				}
			}
		
			if { [llength $data] == $n2 } {
			    uplevel 1 $closure
				incr n
			}
		}
	}
	close $f
	return $n
}

###############################################################################
# Takes result of ::math::fourier::dft operation
# Returns list of intensities
# Parameters:
#   dft - source dft data
# Return
#   list of intensities
###############################################################################
proc dftIntensity { dft } {
  set res [list]

  foreach v $dft {
    set re [lindex $v 0]
    set im [lindex $v 1]
    lappend res [expr sqrt($re * $re + $im * $im)]
  }

  return $res
}

}
