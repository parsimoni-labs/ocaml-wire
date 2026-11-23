# Stage classes are declared by timing.py, not inferred from anonymous cell IDs.
# AMD UG835 get_timing_paths and UG912 TIMING_PATH define the queried objects.
proc wire_stage {name patterns} {
    global wire_report_dir wire_stage_channel wire_stage_missing
    set registers {}
    set selected {}
    foreach cell [all_registers -cells] {
        set cell_name [get_property NAME $cell]
        dict set registers $cell_name $cell
        foreach pattern $patterns {
            if {[string match $pattern $cell_name]} {
                dict set selected $cell_name $cell
                break
            }
        }
    }
    # Synthesis can choose an anonymous feedback alias for the cell name while
    # retaining the semantic Q-net name. Resolve that name through its driver.
    foreach pattern $patterns {
        set nets [get_nets -hierarchical -filter "NAME =~ $pattern"]
        if {[llength $nets] == 0} { continue }
        set drivers [get_pins -leaf -of_objects $nets -filter {DIRECTION == OUT}]
        foreach cell [get_cells -of_objects $drivers] {
            set cell_name [get_property NAME $cell]
            if {[dict exists $registers $cell_name]} {
                dict set selected $cell_name $cell
            }
        }
    }
    set endpoints [dict values $selected]
    if {[llength $endpoints] == 0} {
        puts $wire_stage_channel [join [list $name 0 {} {} {} {} {} {} {} missing_anchor] "\t"]
        incr wire_stage_missing
        return
    }
    set paths [get_timing_paths -to $endpoints -delay_type max -max_paths 20]
    if {[llength $paths] == 0} {
        puts $wire_stage_channel [join [list $name [llength $endpoints] {} {} {} {} {} {} {} no_timed_path] "\t"]
        incr wire_stage_missing
        return
    }
    report_timing -of_objects $paths -input_pins -path_type full_clock_expanded \
        -file "$wire_report_dir/$name.rpt"
    set worst [lindex $paths 0]
    redirect -file "$wire_report_dir/$name.properties.rpt" {report_property -all $worst}
    set fanout 0
    set fanout_net {}
    foreach net [get_nets -of_objects $worst] {
        if {[llength [get_clocks -of_objects $net]] != 0} { continue }
        set loads [llength [get_pins -leaf -of_objects $net -filter {DIRECTION == IN}]]
        if {$loads > $fanout} {
            set fanout $loads
            set fanout_net [get_property NAME $net]
        }
    }
    set row [list $name [llength $endpoints] [get_property DATAPATH_DELAY $worst] \
        [get_property LOGIC_LEVELS $worst] [get_property SLACK $worst] \
        $fanout $fanout_net [get_property STARTPOINT_PIN $worst] \
        [get_property ENDPOINT_PIN $worst] measured]
    foreach value $row {
        if {[regexp {[\t\n\r]} $value]} { error "tab/newline in stage metric" }
    }
    puts $wire_stage_channel [join $row "\t"]
}

proc wire_reports {directory} {
    global wire_report_dir wire_stage_channel wire_stage_missing
    file mkdir $directory
    set wire_report_dir $directory
    set wire_stage_missing 0
    report_utilization -hierarchical -file "$directory/utilization.rpt"
    report_timing_summary -delay_type min_max -report_unconstrained \
        -file "$directory/timing.rpt"
    report_high_fanout_nets -max_nets 50 -timing -load_types \
        -file "$directory/fanout.rpt"
    redirect -file "$directory/check_timing.rpt" {check_timing -verbose}
    foreach pin [all_registers -clock_pins] {
        if {[llength [get_clocks -of_objects $pin]] == 0} {
            error "unclocked register pin: $pin"
        }
    }
    set wire_stage_channel [open "$directory/stages.tsv" w]
    puts $wire_stage_channel "stage\tregisters\tdatapath_ns\tlogic_levels\tslack_ns\tmax_path_fanout\tfanout_net\tstartpoint\tendpoint\tstatus"
    source stages.tcl
    close $wire_stage_channel
}
