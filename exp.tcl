###############################################################################
# exp.nettcl --
#
#    Package for experiments with network
#
# version 1.0:   initial implementation, spring 2008
###############################################################################

package provide nettcl::exp 1.1.0

package require math::statistics
package require cmdline

namespace eval nettcl::exp {

###############################################################################
# Global constants
###############################################################################

set SEED 12345

set NETWORK_SIZE 1000
set NETWORK_M 1
set NETWORK_J 1.0
set NETWORK_J_SCAT 0.01
set NETWORK_POPULATION_STRATEGY "scale-dependent"
set NETWORK_BOUNDARY -1
set NETWORK_ZC_COEFF 1.0
set NETWORK_NB_COEFF 1.0
set NETWORK_VERBOSE false
set NETWORK_AVAL_FILE_NAME aval.aval
set NETWORK_V 40.0
set NETWORK_TAU 1.0

set PERTURBATION_LIMITS 1000000a
set PERTURBATION_DH 1.0
set PERTURBATION_DH_SCAT 0.0
set PERTURBATION_METHOD m1

set DISTRIBUTION_PREC -1

###############################################################################
# Makes network with default settings 
# Parameters:
# ?params? - associative array with parameters
# Parameters supported:
#   n - network size (default is 1000)
#   m - network m parameter (default is 1)
#   j - network average relation cost (default is 1.0)
#   jScat - network relation cost scattering (default is 0.0)
#   populationStrategy - network population strategy (default is scale-dependent)
#   boundary - network boundary (default is unlimited)
#   verbose - network verbose mode (default is false)
#   avalFileName - network verbose mode (default is aval.aval)
#   seed - random number generator seed (default is 12345)
#   v - V value (default is 40.0)
#   tau - tau value (default is 1.0)
#   vGen - generator for V values
#   tauGen - generator for tau values
###############################################################################
proc makeNetwork { {params ""} } {
  global ::nettcl::exp::NETWORK_SIZE
  global ::nettcl::exp::NETWORK_M
  global ::nettcl::exp::NETWORK_J
  global ::nettcl::exp::NETWORK_J_SCAT
  global ::nettcl::exp::NETWORK_POPULATION_STRATEGY
  global ::nettcl::exp::NETWORK_BOUNDARY
  global ::nettcl::exp::NETWORK_ZC_COEFF
  global ::nettcl::exp::NETWORK_NB_COEFF
  global ::nettcl::exp::NETWORK_VERBOSE
  global ::nettcl::exp::NETWORK_AVAL_FILE_NAME
  global ::nettcl::exp::NETWORK_V
  global ::nettcl::exp::NETWORK_TAU
  global ::nettcl::exp::SEED

  upvar $params par

  set n [expr {[array exists par] && [llength [array names par -exact n]] > 0 ? $par(n) : $NETWORK_SIZE}]   
  set m [expr {[array exists par] && [llength [array names par -exact m]] > 0 ? $par(m) : $NETWORK_M}]   
  set j [expr {[array exists par] && [llength [array names par -exact j]] > 0 ? $par(j) : $NETWORK_J}]   
  set jScat [expr {[array exists par] && [llength [array names par -exact jScat]] > 0 ? $par(jScat) : $NETWORK_J_SCAT}]   
  set populationStrategy [expr {[array exists par] && [llength [array names par -exact populationStrategy]] > 0 ? $par(populationStrategy) : $NETWORK_POPULATION_STRATEGY}]   
  set boundary [expr {[array exists par] && [llength [array names par -exact boundary]] > 0 ? $par(boundary) : $NETWORK_BOUNDARY}]   
  set zcCoeff [expr {[array exists par] && [llength [array names par -exact zcCoeff]] > 0 ? $par(zcCoeff) : $NETWORK_ZC_COEFF}]   
  set nbCoeff [expr {[array exists par] && [llength [array names par -exact nbCoeff]] > 0 ? $par(nbCoeff) : $NETWORK_NB_COEFF}]   
  set verbose [expr {[array exists par] && [llength [array names par -exact verbose]] > 0 ? $par(verbose) : $NETWORK_VERBOSE}]   
  set avalFileName [expr {[array exists par] && [llength [array names par -exact avalFileName]] > 0 ? $par(avalFileName) : $NETWORK_AVAL_FILE_NAME}]   
  set seed [expr {[array exists par] && [llength [array names par -exact seed]] > 0 ? $par(seed) : $SEED}]   
  set fileName [expr {[array exists par] && [llength [array names par -exact fileName]] > 0 ? $par(fileName) : ""}]   
  set v [expr {[array exists par] && [llength [array names par -exact v]] > 0 ? $par(v) : $NETWORK_V}]   
  set tau [expr {[array exists par] && [llength [array names par -exact tau]] > 0 ? $par(tau) : $NETWORK_TAU}]   
  set vGen [expr {[array exists par] && [llength [array names par -exact vGen]] > 0 ? $par(vGen) : ""}]   
  set tauGen [expr {[array exists par] && [llength [array names par -exact tauGen]] > 0 ? $par(tauGen) : ""}]   
  set betaGen [expr {[array exists par] && [llength [array names par -exact betaGen]] > 0 ? $par(betaGen) : ""}]   

  network_srand $seed

  if { [string length $fileName] > 0 } {
    set net [network $fileName]
  } else {
    if { [populator exists $populationStrategy] } {
      if { [network_rng exists $vGen] } {
        if { [network_rng exists $tauGen] } {
          if { [network_rng exists $betaGen] } {
            set net [network $n $populationStrategy $boundary $zcCoeff $nbCoeff $vGen $tauGen $betaGen]
          } else {
            set net [network $n $populationStrategy $boundary $zcCoeff $nbCoeff $vGen $tauGen]
          }
        } else {
          set net [network $n $populationStrategy $boundary $zcCoeff $nbCoeff $vGen]
        }
      } else {
        set net [network $n $populationStrategy $boundary $zcCoeff $nbCoeff]
      }
    } else {
      set net [network $n $m $j $jScat $populationStrategy $boundary $zcCoeff $nbCoeff]
    }
  }
  network_set $net minavalsize [expr 0.99 / [network_get $net size]]
  network_set $net avalfilename $avalFileName
  network_set $net verbose $verbose
  if {![network_rng exists $vGen]} {
    network_set $net v $v
  }
  if {![network_rng exists $tauGen]} {
    network_set $net tau $tau
  }

  return $net
}

###############################################################################
# Experiment: calculate avalanche statistics in perturbated network 
# Parameters:
# net - network to perturbate
# ?params? - associative array with parameters
# Parameters supported:
#   limits - perturbation limits (default is 1000000a)
#   dh - perturbation average value (default is 1.0)
#   dhScat - perturbation value scattering (default is 0.0)
#   method - perturbation method (default is m1)
#   seed - random number generator seed (default is 12345)
###############################################################################
proc avalancheStatistics { net {params ""} } {
  global ::nettcl::exp::SEED
  global ::nettcl::exp::PERTURBATION_LIMITS
  global ::nettcl::exp::PERTURBATION_DH
  global ::nettcl::exp::PERTURBATION_DH_SCAT
  global ::nettcl::exp::PERTURBATION_METHOD
  global ::nettcl::exp::DISTRIBUTION_PREC

  upvar $params par

  set limits [expr {[array exists par] && [llength [array names par -exact limits]] > 0 ? $par(limits) : $PERTURBATION_LIMITS}]   
  set dh [expr {[array exists par] && [llength [array names par -exact dh]] > 0 ? $par(dh) : $PERTURBATION_DH}]   
  set dhScat [expr {[array exists par] && [llength [array names par -exact dhScat]] > 0 ? $par(dhScat) : $PERTURBATION_DH_SCAT}]   
  set method [expr {[array exists par] && [llength [array names par -exact method]] > 0 ? $par(method) : $PERTURBATION_METHOD}]   
  set seed [expr {[array exists par] && [llength [array names par -exact seed]] > 0 ? $par(seed) : $SEED}]   
  set makeCritical [expr {[array exists par] && [llength [array names par -exact makeCritical]] > 0 ? $par(makeCritical) : 1}]   

  if {$makeCritical != 0} {
    # make network critical
    set saved [network_profile_trace_get $net enabled]
    network_profile_trace_set $net enabled 0
    ::nettcl::makeCritical $net $dh
    network_profile_trace_set $net enabled $saved
  }
  
  # write H distribution
  set lim [::nettcl::makeHistogramLimits 101 [expr - [network_get $net minh]] [network_get $net maxh]]
  ::nettcl::writeHistogram [::math::statistics::histogram $lim [network_get $net hlist]] $lim pre.z.distr  

  # initialize RNG
  network_srand $seed
  
  # main run
  set cmd "pertmethod create $method"
  network_run $net $limits $dh $dhScat [eval $cmd]
  
  # write network to file
  network_write $net network.net
  set lim [::nettcl::makeHistogramLimits 101 [expr - [network_get $net minh]] [network_get $net maxh]]
  ::nettcl::writeHistogram [::math::statistics::histogram $lim [network_get $net hlist]] $lim z.distr  
  
  # make avalanche distribution
  set data [::nettcl::loadTrace [network_get $net avalfilename] 1]
  set stats [::math::statistics::basic-stats $data]
  set prec [expr $DISTRIBUTION_PREC > 0 ? $DISTRIBUTION_PREC : [expr 1.0 / [network_get $net size]]]
  set lim [::nettcl::makeHistogramLimits2 $prec 0 [lindex $stats 2]]
  ::nettcl::writeHistogram [::math::statistics::histogram $lim $data] $lim aval.distr  
  
  # remove temporary files
  file delete -force [network_get $net tmpnetfilename]
}

###############################################################################
# Save network forces into forces.txt
# Parameters:
# net - network to perturbate
###############################################################################
proc writeForces { net {fileName "forces.txt"}} {
  if { [catch {open $fileName w} dest] } {
    puts stderr "Could not open writing\n$dest"
    close $src
    exit 1
  }
  foreach node [network_get $net nodelist ] {
    puts $dest [node_get $node force]
  }
  close $dest
}

###############################################################################
# Creates 2-dimensional grid
# Options:
#   see <opts> list definition below
###############################################################################
proc make2dGrid { args } {
	set opts {
		{rows.arg			25		"number of rows"}
		{columns.arg		25		"number of columns"}
		{N.arg				""		"number of nodes"}
		{potential.arg		1		"whether network is potential"}
		{J.arg				1		"average J"}
		{dJ.arg				""		"J scattering"}
		{V.arg				40.0	"average V"}
		{dV.arg				""		"V scattering"}
		{Beta.arg			1.0		"average beta"}
		{dBeta.arg			""		"beta scattering"}
		{Tau.arg			1.0		"average tau"}
		{dTau.arg			""		"tau scattering"}
		{delta.arg			0.0		"generic scattering"}
		{generator.arg		""		"random number generator"}
	}

	set usage ": make2dGrid \[options]\noptions:"
	array set options [::cmdline::getoptions args $opts $usage]

	set rows [expr int($options(rows))]

	if { $options(dJ) == "" } {
		set dJ [expr $options(J) * $options(delta)]
	} else {
		set dJ $options(dJ) 
	}
	set populationMethod [populator create grid2d $rows $options(J) $dJ $options(potential) ]

	if { $options(N) != "" } {
		set N $options(N)
	} else {
		set N [expr int($rows * $options(columns))]
	}

	return [makeGenericNetwork $N $populationMethod options]
}

###############################################################################
# Assigns currents to 2D-grid to emulate 2-way magnetic field 
#   with superconducting screen attched to bottom and top edges of grid
# Arguments:
#   network - network to modify
#   Lx - horizontal size of the grid
#   averageZ - average current required 
#   epsilon - horizontal shift of the attachment position
###############################################################################
proc simpleSuperconductingScreen2d { network Lx averageZ { epsilon 0 } } {
	set z [expr 0.5 * $Lx * $averageZ]

	nettcl2d::foreachContact c $network {} {
		nettcl2d::contact set $c z 0.0
	}

	nettcl2d::foreachContact c $network { left | right } {
		nettcl2d::contact set $c z $z
	}
	nettcl2d::foreachContact c $network { bottom } {
		if { [nettcl2d::contact get-prop $c x] < int($Lx / 2) } {
			nettcl2d::contact set $c z -$z
		} else {
			nettcl2d::contact set $c z $z
		}
	}
	nettcl2d::foreachContact c $network { top } {
		if { [nettcl2d::contact get-prop $c x] < int($Lx / 2) } {
			nettcl2d::contact set $c z $z
		} else {
			nettcl2d::contact set $c z -$z
		}
	}
}

###############################################################################
# Assigns currents to 2D-grid to emulate 2-way magnetic field 
#   with superconducting screen attched to 4 corners of grid
# Arguments:
#   network - network to modify
#   averageZ - average current required 
###############################################################################
proc simpleDiagonalSuperconductingScreen { network averageZ } {
    # total number of contacts
    set n [nettcl2d::network get $network num-of-contacts]

    # total numer or boundary contacts
    set nb [llength [nettcl2d::network get $network contacts { left | right | top | bottom }]]

    # current to inject
    set z [expr {$n / $nb * $averageZ}]
puts "n = $n, nb = $nb, z = $z"

    nettcl2d::foreachContact c $network {} {
	nettcl2d::contact set $c z 0.0
    }

    nettcl2d::foreachContact c $network { left | bottom } {
	nettcl2d::contact set $c z -$z
    }
    nettcl2d::foreachContact c $network { right | top } {
	nettcl2d::contact set $c z $z
    }
}

###############################################################################
# Private procedures
###############################################################################

proc makeGenericNetwork { N populationMethod optionsName } {
	upvar $optionsName options

	if { [network_rng exists $options(generator)] } {
		set rng $options(generator)
	} else {
		set rng [network_rng create uniform 10000.0 20000.0]
	}

	set params(seed) [expr int([network_rng next $rng])]
	set params(n) $N
	set params(populationStrategy) $populationMethod

	if { $options(dV) == "" } {
		set dV [expr $options(V) * $options(delta)]
	} else {
		set dV $options(dV) 
	}
	set vGen [network_rng create uniform $options(V) $dV]
	network_rng seed $vGen [expr int([network_rng next $rng])]
	set params(vGen) $vGen

	if { $options(dBeta) == "" } {
		set dBeta [expr $options(Beta) * $options(delta)]
	} else {
		set dBeta $options(dBeta) 
	}
	set betaGen [network_rng create uniform $options(Beta) $dBeta]
	network_rng seed $betaGen [expr int([network_rng next $rng])]
	set params(betaGen) $betaGen

	if { $options(dTau) == "" } {
		set dTau [expr $options(Tau) * $options(delta)]
	} else {
		set dTau $options(dTau) 
	}
	set tauGen [network_rng create uniform $options(Tau) $dTau]
	network_rng seed $tauGen [expr int([network_rng next $rng])]
	set params(tauGen) $tauGen

	return [::nettcl::exp::makeNetwork params]
}

}
