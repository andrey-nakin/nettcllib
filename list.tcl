#!/usr/bin/tclsh
###############################################################################
# list.tcl --
#
#    Package for arithmetic operation with lists
#
# version 1.0:
###############################################################################

package provide nettcl::list 1.0.0

package require math::statistics 0.5

namespace eval nettcl::list {
	namespace export
}

# Sums all corresponding elements in given lists and returns resulting list.
# Arguments
#   lists of real numbers
# Return
#   Resulting list of real numbers
proc nettcl::list::add { args } {
	set result [list]

	foreach lst $args {
		for { set i 0; set n [llength $lst] } { $i < $n } { incr i } {
			if { [llength $result] == $i } {
				lappend result [lindex $lst $i]
			} else {
				lset result $i [expr [lindex $lst $i] + [lindex $result $i]]
			}
		}
	}

	return $result
}

# Divide all elements in list by given number.
# Arguments
#   lst - list to affect
#   num - divisor
# Return
#   Resulting list
proc nettcl::list::divByNum { lst num } {
	set result [list]

	foreach v $lst {
		lappend result [expr $v / $num]
	}

	return $result
}

# Given list of lists of real number, calculates list of mean values.
# Arguments
#   lst - list of lists of real numbers
# Return
#   Resulting list of means
proc nettcl::list::mean { lst } {
	return [stat lst ::math::statistics::mean]
}

# Given list of lists of real number, calculates list of min values.
# Arguments
#   lst - list of lists of real numbers
# Return
#   Resulting list of min values
proc nettcl::list::min { lst } {
	return [stat lst ::math::statistics::min]
}

# Given list of lists of real number, calculates list of max values.
# Arguments
#   lst - list of lists of real numbers
# Return
#   Resulting list of max values
proc nettcl::list::max { lst } {
	return [stat lst ::math::statistics::max]
}

# Given list of lists of real number, calculates list of std values.
# Arguments
#   lst - list of lists of real numbers
# Return
#   Resulting list of stds
proc nettcl::list::pstdev { lst } {
	return [stat lst ::math::statistics::pstdev]
}

# Returns a list of sequental integer numbers
# Arguments
#   start - starting number (inclusively)
#   end - ending number (exclusively)
# Return
#   Resulting list of numbers
proc nettcl::list::order { start end } {
	set nums {}
	for { set i $start } { $i < $end } { incr i } {
		lappend nums $i
	}
	return $nums
}

# Shuffles a list
# Arguments
#   list - list to shuffle
# Return
#   Resulting list
proc nettcl::list::shuffle { list } {
	set n [llength $list]
	for { set i 0 } { $i < $n } { incr i } {
		set j [expr {int(rand()*$n)}]
		set temp [lindex $list $j]
		set list [lreplace $list $j $j [lindex $list $i]]
		set list [lreplace $list $i $i $temp]
	}
	return $list
}

##############################################################################
# Private
##############################################################################

proc nettcl::list::stat { lstName op } {
	upvar $lstName lst

	if { [llength $lst] == 0 } {
		return {}
	}

	set result [list]
	set n [llength [lindex $lst 0]]
	for { set i 0 } { $i < $n } { incr i } {
		set accum [list]
		foreach l $lst {
			catch {	lappend accum [lindex $l $i] }
		}
		lappend result [$op $accum]
	}

	return $result
}

#puts "Test 1:\t[nettcl::list::add {1 2 3} {4 5 6} {7 8 9}]"
#puts "Test 2:\t[nettcl::list::add {1 2 3} {4 5 6 7 8 9}]"
#puts "Test 3:\t[nettcl::list::divByNum {1.0 2.0 3.0} 4.0]"
#puts "Test 4:\t[nettcl::list::mean {{1.0 2.0 3.0} {1.0 2.1 3.1} {1.0 2.2}} ]"
#puts "Test 5:\t[nettcl::list::pstdev {{1.0 2.0 3.0} {1.0 2.1 3.1} {1.0 2.2}} ]"

