#!/bin/csh -f

set dc_tag = $1
set top = DGR_DUT

echo "set dc_tag is $dc_tag"

# pt min::max; dc min:typ:max

cd $1
# dc_shell -x "source -echo -verbose ../rm_setup/dc_setup.tcl; read_ddc ./results/$top.mapped.ddc; write_sdf -version 2.1 -significant_digits 3 results/$top.mapped.sdf; quit"
pt_shell -x "set p slow; set v 0p81v; set t m40; source -echo -verbose ../../4STA/scripts/pt_load_lib.tcl; read_ddc ./results/$top.mapped.ddc; write_sdf -version 3.0 -context verilog -significant_digits 3 -no_edge -include {SETUPHOLD RECREM} -exclude {checkpins no_condelse} -no_negative_values {cell_delays net_delays} results/$top.mapped.sdf.gz -compress gzip; exit"
cd ..
