#*******************************************************************
#set design optimization constraints
#*******************************************************************
create_clock -name clock_main -period 2.50 [get_pins "clock_PAD/C"]

#set_clock_latency      0.05 [get_clocks "clock_main"]
#set 15%~25% cycle
set_clock_uncertainty  0.40 -setup [get_clocks "clock_main"]
set_clock_uncertainty  0.05 -hold [get_clocks "clock_main"]
set_clock_transition   0.10 [get_clocks "clock_main"]

#clock_main
#set 2%~40% cycle
set_input_delay  -max 1.00  -clock clock_main [remove_from_collection [all_inputs] [get_ports  "clock_pad"]]
set_input_delay  -min 0.05	-clock clock_main [remove_from_collection [all_inputs] [get_ports  "clock_pad"]]
set_output_delay -max 1.00 	-clock clock_main [all_outputs]
set_output_delay -min 0.05	-clock clock_main [all_outputs]

#*******************************************************************
#set design rule constraints
#*******************************************************************

set_max_area 2000000.00
set_ideal_network [get_pins "reset_PAD/C"]
set_ideal_network [get_pins "clock_PAD/C"]

set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[32]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[33]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[34]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[35]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[36]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[37]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[38]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[39]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[40]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[41]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[42]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[43]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[44]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[45]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[46]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[47]"]

set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[0]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[1]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[2]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[3]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[4]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[5]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[6]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[7]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[8]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[9]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[10]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[11]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[12]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[13]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[14]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[15]"]

set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_valid_pad"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[16]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[17]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[18]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[19]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[20]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[21]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[22]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[23]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[24]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[25]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[26]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[27]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[28]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[29]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[30]"]
set_driving_cell -lib_cell PRUW08DGZ_H_G -from_pin PAD -pin C [get_ports "bskctcmd_bits_pad[31]"]

set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[0]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[1]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[2]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[3]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[4]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[5]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[6]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_bits_pad[7]"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "ksk_valid_pad"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "uartrx_pad"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "inbar_pad"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "clock_pad"]
set_driving_cell -lib_cell PRUW08DGZ_V_G -from_pin PAD -pin C [get_ports "reset_pad"]

group_path -name REGOUT      -to   [all_outputs]
group_path -name REGIN       -from [remove_from_collection [all_inputs] [get_ports "clock_pad"]]
group_path -name FEEDTHROUGH -from [remove_from_collection [all_inputs] [get_ports "clock_pad"]] -to [all_outputs]
