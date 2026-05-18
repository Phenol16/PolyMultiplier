puts "RM-Info: Running script [info script]\n"

##########################################################################################
# Variables common to all reference methodology scripts
# Script: common_setup.tcl
# Version: N-2017.09-SP4 (April 23, 2018)
# Copyright (C) 2007-2017 Synopsys, Inc. All rights reserved.
##########################################################################################

set PRJ_ROOT                       "/export4/Home/iac01/shiliubowen/Work/BackFlow28"
set WORK_DIR                       "$PRJ_ROOT/4Backend/1DC-RM"
set MEM_DIR                        "/export4/Home/iac01/shiliubowen/Work/SramLibrary28"

set DESIGN_NAME                    "ToomCook1024"  ;     #  The name of the top-level design

set DESIGN_REF_DATA_PATH           "$PRJ_ROOT/2RtlCode/ToomCook1024"  ;
  		#  Absolute path prefix variable for library/design data.
                                                                                #  Use this variable to prefix thecommon absolute path
                                                                                #  to the common variables defined below.
                                                                                #  Absolute paths are mandatory for hierarchical 
                                                                                #  reference methodology flow.

##########################################################################################
# Hierarchical Flow Design Variables
##########################################################################################

set HIERARCHICAL_DESIGNS           "$DESIGN_NAME" ; # List of hierarchical block design names "DesignA DesignB" ...
set HIERARCHICAL_CELLS             "" ;         # List of hierarchical block cell instance names "u_DesignA u_DesignB" ...

##########################################################################################
# Library Setup Variables
##########################################################################################

# For the following variables, use a blank space to separate multiple entries.
# Example: set TARGET_LIBRARY_FILES "lib1.db lib2.db lib3.db"
set ADDITIONAL_SEARCH_PATH         "$MEM_DIR/ts1n28hpcphvtb32x96m4s_180a/NLDM   \
				   	                $MEM_DIR/ts1n28hpcphvtb64x96m4s_180a/NLDM   \
				   	                $MEM_DIR/ts1n28hpcphvtb64x112m4s_180a/NLDM  \
				   	                $MEM_DIR/ts1n28hpcphvtb96x64m4s_180a/NLDM   \
				   	                $MEM_DIR/ts1n28hpcphvtb128x96m4s_180a/NLDM  \
				   	                $MEM_DIR/ts1n28hpcphvtb256x24m4s_180a/NLDM  \
				   	                $MEM_DIR/ts1n28hpcphvtb256x96m4s_180a/NLDM  \
				   	                $MEM_DIR/ts1n28hpcphvtb768x16m4s_180a/NLDM  \
				   	                $MEM_DIR/ts1n28hpcphvtb1056x16m4s_180a/NLDM \
				   	                /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140hvt_180a   \
				   	                /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140_180a      \
				   	                /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140lvt_180a   \
					                /export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Front_End/timing_power_noise/NLDM/tphn28hpcpgv18_170a " ;          #  Additional search path to be added to the default search path

# set ADDITIONAL_SEARCH_PATH         "/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140hvt_180a \
				   	                /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140_180a      \
				   	                /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140lvt_180a   \
					                /export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Front_End/timing_power_noise/NLDM/tphn28hpcpgv18_170a " ;          #  Additional search path to be added to the default search path

set TARGET_LIBRARY_FILES           "tcbn28hpcplusbwp40p140hvtssg0p81vm40c_ccs.db    \
                                    tcbn28hpcplusbwp40p140lvtssg0p81vm40c_ccs.db    \
                                    tcbn28hpcplusbwp40p140ssg0p81vm40c_ccs.db       ";   #  Target technology logical libraries

# set TARGET_LIBRARY_FILES           "tcbn28hpcplusbwp40p140hvttt0p9v25c_ccs.db       \
                                    tcbn28hpcplusbwp40p140lvttt0p9v25c_ccs.db       \
                                    tcbn28hpcplusbwp40p140tt0p9v25c_ccs.db          ";   #  Target technology logical libraries

set ADDITIONAL_LINK_LIB_FILES      "tphn28hpcpgv18ssg0p81v1p62vm40c.db          \
                                    ts1n28hpcphvtb32x96m4s_ssg0p81vm40c.db      \
                                    ts1n28hpcphvtb64x96m4s_ssg0p81vm40c.db      \
                                    ts1n28hpcphvtb64x112m4s_ssg0p81vm40c.db     \
                                    ts1n28hpcphvtb96x64m4s_ssg0p81vm40c.db      \
                                    ts1n28hpcphvtb128x96m4s_ssg0p81vm40c.db     \
                                    ts1n28hpcphvtb256x24m4s_ssg0p81vm40c.db     \
                                    ts1n28hpcphvtb256x96m4s_ssg0p81vm40c.db     \
                                    ts1n28hpcphvtb768x16m4s_ssg0p81vm40c.db     \
                                   	ts1n28hpcphvtb1056x16m4s_ssg0p81vm40c.db    ";   #  Extra link logical libraries not included in TARGET_LIBRARY_FILES

# set ADDITIONAL_LINK_LIB_FILES      "tphn28hpcpgv18tt0p9v1p8v25c.db                  ";   #  Extra link logical libraries not included in TARGET_LIBRARY_FILES

set MIN_LIBRARY_FILES              "tphn28hpcpgv18ssg0p81v1p62vm40c.db                tphn28hpcpgv18ffg0p99v1p98v125c.db                \
                                    tcbn28hpcplusbwp40p140ssg0p81vm40c_ccs.db         tcbn28hpcplusbwp40p140ffg0p99v125c_ccs.db         \
                                    tcbn28hpcplusbwp40p140hvtssg0p81vm40c_ccs.db      tcbn28hpcplusbwp40p140hvtffg0p99v125c_ccs.db      \
                                    tcbn28hpcplusbwp40p140lvtssg0p81vm40c_ccs.db      tcbn28hpcplusbwp40p140lvtffg0p99v125c_ccs.db      \
				                    ts1n28hpcphvtb32x96m4s_ssg0p81vm40c.db	          ts1n28hpcphvtb32x96m4s_ffg0p99v125c.db	        \
				                    ts1n28hpcphvtb64x96m4s_ssg0p81vm40c.db            ts1n28hpcphvtb64x96m4s_ffg0p99v125c.db            \
		  		                    ts1n28hpcphvtb64x112m4s_ssg0p81vm40c.db	          ts1n28hpcphvtb64x112m4s_ffg0p99v125c.db	        \
				                    ts1n28hpcphvtb96x64m4s_ssg0p81vm40c.db            ts1n28hpcphvtb96x64m4s_ffg0p99v125c.db            \
		  		                    ts1n28hpcphvtb128x96m4s_ssg0p81vm40c.db	          ts1n28hpcphvtb128x96m4s_ffg0p99v125c.db	        \
				                    ts1n28hpcphvtb256x24m4s_ssg0p81vm40c.db           ts1n28hpcphvtb256x24m4s_ffg0p99v125c.db           \
		  		                    ts1n28hpcphvtb256x96m4s_ssg0p81vm40c.db	          ts1n28hpcphvtb256x96m4s_ffg0p99v125c.db	        \
		  		                    ts1n28hpcphvtb768x16m4s_ssg0p81vm40c.db	          ts1n28hpcphvtb768x16m4s_ffg0p99v125c.db	        \
		  		                    ts1n28hpcphvtb1056x16m4s_ssg0p81vm40c.db          ts1n28hpcphvtb1056x16m4s_ffg0p99v125c.db	        ";   #  List of max min library pairs "max1 min1 max2 min2 max3 min3"...

set MW_REFERENCE_LIB_DIRS          "$MEM_DIR/ts1n28hpcphvtb32x96m4s_180a/MILKYWAY           \
                                    $MEM_DIR/ts1n28hpcphvtb64x96m4s_180a/MILKYWAY           \
                                    $MEM_DIR/ts1n28hpcphvtb64x112m4s_180a/MILKYWAY          \
                                    $MEM_DIR/ts1n28hpcphvtb96x64m4s_180a/MILKYWAY           \
                                    $MEM_DIR/ts1n28hpcphvtb128x96m4s_180a/MILKYWAY          \
                                    $MEM_DIR/ts1n28hpcphvtb256x24m4s_180a/MILKYWAY          \
                                    $MEM_DIR/ts1n28hpcphvtb256x96m4s_180a/MILKYWAY          \
                                    $MEM_DIR/ts1n28hpcphvtb768x16m4s_180a/MILKYWAY          \
                                    $MEM_DIR/ts1n28hpcphvtb1056x16m4s_180a/MILKYWAY         \
                                    /export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Back_End/milkyway/tphn28hpcpgv18_110a/mt_2/8lm/frame_only/tphn28hpcpgv18   \
				                    /export/Library/TN28HPC+/IO_ext/tpbn28v_140b/TSMCHOME/digital/Back_End/milkyway/tpbn28v_140a/cup/10m/10M_5X2Y2R/cell_frame/tpbn28v              \
                                    /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140hvt_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140hvt   \
				                    /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140lvt_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140lvt   \
				                    /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140	"       ;#  Milkyway reference libraries (include IC Compiler ILMs here)

set MW_REFERENCE_CONTROL_FILE      ""  ;#  Reference Control file to define the Milkyway reference libs

set TECH_FILE                      "/export/Library/TN28HPC+/Technology_File/N28_PRTF_Syn_v1d5a/PR_tech/Synopsys/TechFile/VHV/tsmcn28_10lm5X2Y2RUTRDL.tf"  ;    #  Milkyway technology file
set MAP_FILE                       "/export/Library/TN28HPC+/PEXT_rules/Star_RCXT/1p10m_5x2y2r/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_typical/Reference/MAP/star.map_icc_cln28hpc+_1p10m_5x2y2r_ut-alrdl"  ;  #  Mapping file for TLUplus
set TLUPLUS_MAX_FILE               "/export4/Home/yvxinglong/Work/BackFlow28/4Backend/techfile/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_cworst_T/cln28hpc+_1p10m+ut-alrdl_5x2y2r_cworst_T.tluplus"  ;           #  Max TLUplus file
set TLUPLUS_MIN_FILE               "/export4/Home/yvxinglong/Work/BackFlow28/4Backend/techfile/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_cbest/cln28hpc+_1p10m+ut-alrdl_5x2y2r_cbest.tluplus"  ;                 #  Min TLUplus file

set MIN_ROUTING_LAYER              "M2"   ;  # Min routing layer
set MAX_ROUTING_LAYER              "M8"   ;  # Max routing layer

set LIBRARY_DONT_USE_FILE                   "$WORK_DIR/constraint/lib_cells_dont_use.tcl";    # Tcl file with library modifications for dont_use
set LIBRARY_DONT_USE_PRE_COMPILE_LIST       "$WORK_DIR/constraint/lib_cells_dont_use.tcl";    # Tcl file for customized don't use list before first compile
set LIBRARY_DONT_USE_PRE_INCR_COMPILE_LIST  "$WORK_DIR/constraint/lib_cells_dont_use.tcl";    # Tcl file with library modifications for dont_use before incr compile

##########################################################################################
# Multivoltage Common Variables
#
# Define the following multivoltage common variables for the reference methodology scripts 
# for multivoltage flows. 
# Use as few or as many of the following definitions as needed by your design.
##########################################################################################

#set PD1                          ""           ;# Name of power domain/voltage area  1
#set VA1_COORDINATES              {}           ;# Coordinates for voltage area 1
#set MW_POWER_NET1                "VDD1"       ;# Power net for voltage area 1
#
#set PD2                          ""           ;# Name of power domain/voltage area  2
#set VA2_COORDINATES              {}           ;# Coordinates for voltage area 2
#set MW_POWER_NET2                "VDD2"       ;# Power net for voltage area 2
#
#set PD3                          ""           ;# Name of power domain/voltage area  3
#set VA3_COORDINATES              {}           ;# Coordinates for voltage area 3
#set MW_POWER_NET3                "VDD3"       ;# Power net for voltage area 3
#
#set PD4                          ""           ;# Name of power domain/voltage area  4
#set VA4_COORDINATES              {}           ;# Coordinates for voltage area 4
#set MW_POWER_NET4                "VDD4"       ;# Power net for voltage area 4

puts "RM-Info: Completed script [info script]\n"

