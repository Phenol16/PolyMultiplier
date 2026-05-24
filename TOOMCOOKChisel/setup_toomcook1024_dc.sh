#!/usr/bin/env bash
set -euo pipefail

TOP="ToomCook1024"
CLOCK_PERIOD_NS="${CLOCK_PERIOD_NS:-10.0}"

# 当前目录应为：
# /home/shiliubowen/Work/BackFlow28/4Backend/1DC-RM
DC_RM_DIR="$(pwd)"

# BackFlow28 根目录：
# /home/shiliubowen/Work/BackFlow28
PRJ_ROOT="$(cd ../.. && pwd)"

# Work 根目录：
# /home/shiliubowen/Work
WORK_ROOT="$(cd ../../.. && pwd)"

RTL_DIR="${PRJ_ROOT}/2RtlCode/${TOP}"
MEM_DIR="${WORK_ROOT}/SramLibrary28"

SRAM32_DIR="${MEM_DIR}/ts1n28hpcphvtb32x96m4s_180a"
SRAM256_DIR="${MEM_DIR}/ts1n28hpcphvtb256x96m4s_180a"

echo "[INFO] DC_RM_DIR       = ${DC_RM_DIR}"
echo "[INFO] PRJ_ROOT        = ${PRJ_ROOT}"
echo "[INFO] WORK_ROOT       = ${WORK_ROOT}"
echo "[INFO] RTL_DIR         = ${RTL_DIR}"
echo "[INFO] MEM_DIR         = ${MEM_DIR}"
echo "[INFO] SRAM32_DIR      = ${SRAM32_DIR}"
echo "[INFO] SRAM256_DIR     = ${SRAM256_DIR}"
echo "[INFO] CLOCK_PERIOD_NS = ${CLOCK_PERIOD_NS}"

if [ ! -d "${RTL_DIR}" ]; then
    echo "[ERROR] RTL directory not found: ${RTL_DIR}"
    exit 1
fi

if [ ! -f "${RTL_DIR}/ToomCook1024.v" ]; then
    echo "[ERROR] Missing ${RTL_DIR}/ToomCook1024.v"
    exit 1
fi

if [ ! -f "${RTL_DIR}/sp_ram_macro.v" ]; then
    echo "[ERROR] Missing ${RTL_DIR}/sp_ram_macro.v"
    exit 1
fi

if [ ! -d "${SRAM32_DIR}" ]; then
    echo "[ERROR] Missing SRAM32_DIR: ${SRAM32_DIR}"
    exit 1
fi

if [ ! -d "${SRAM256_DIR}" ]; then
    echo "[ERROR] Missing SRAM256_DIR: ${SRAM256_DIR}"
    exit 1
fi

if [ ! -d constraint ] || [ ! -d rm_setup ] || [ ! -d rm_dc_scripts ]; then
    echo "[ERROR] Please run this script inside 1DC-RM."
    exit 1
fi

# ============================================================
# 1. 生成约束文件
# ============================================================

cat > "constraint/${TOP}.constraints.tcl" <<EOF
# Auto-generated constraint for ${TOP}

set CLK_PERIOD ${CLOCK_PERIOD_NS}

create_clock -name clock_main -period \$CLK_PERIOD [get_ports "clock"]

set_clock_uncertainty [expr \$CLK_PERIOD * 0.10] -setup [get_clocks "clock_main"]
set_clock_uncertainty 0.10 -hold [get_clocks "clock_main"]
set_clock_transition 0.05 [get_clocks "clock_main"]

set_ideal_network [get_ports "clock"]

if {[sizeof_collection [get_ports -quiet "reset"]] > 0} {
    set_ideal_network [get_ports "reset"]
    set_false_path -from [get_ports "reset"]
}

set INPUT_DELAY  [expr \$CLK_PERIOD * 0.20]
set OUTPUT_DELAY [expr \$CLK_PERIOD * 0.20]

set input_ports_no_clk [remove_from_collection [all_inputs] [get_ports "clock"]]

if {[sizeof_collection [get_ports -quiet "reset"]] > 0} {
    set input_ports_no_clk [remove_from_collection \$input_ports_no_clk [get_ports "reset"]]
}

if {[sizeof_collection \$input_ports_no_clk] > 0} {
    set_input_delay \$INPUT_DELAY -clock [get_clocks "clock_main"] \$input_ports_no_clk
}

if {[sizeof_collection [all_outputs]] > 0} {
    set_output_delay \$OUTPUT_DELAY -clock [get_clocks "clock_main"] [all_outputs]
}

# 先以跑通为主，面积不设上限
set_max_area 0

# SRAM macro 不要被 DC 优化掉
set sram_cells [get_cells -quiet -hier -filter "ref_name =~ TS1N28HPCPHVTB*"]
if {[sizeof_collection \$sram_cells] > 0} {
    set_dont_touch \$sram_cells
}

group_path -name REGIN   -from \$input_ports_no_clk
group_path -name REGOUT  -to [all_outputs]
group_path -name REG2REG -from [all_registers] -to [all_registers]
EOF

echo "[INFO] Generated constraint/${TOP}.constraints.tcl"

# ============================================================
# 2. 修改 rm_setup/common_setup.tcl
# ============================================================

COMMON_SETUP="rm_setup/common_setup.tcl"
cp "${COMMON_SETUP}" "${COMMON_SETUP}.bak_$(date +%Y%m%d_%H%M%S)"

awk '
/# >>> TOOMCOOK1024 AUTO SETUP/ {skip=1; next}
/# <<< TOOMCOOK1024 AUTO SETUP/ {skip=0; next}
!skip {print}
' "${COMMON_SETUP}" > "${COMMON_SETUP}.tmp"
mv "${COMMON_SETUP}.tmp" "${COMMON_SETUP}"

cat >> "${COMMON_SETUP}" <<EOF

# >>> TOOMCOOK1024 AUTO SETUP

set PRJ_ROOT "${PRJ_ROOT}"
set WORK_DIR "${DC_RM_DIR}"
set DESIGN_NAME "${TOP}"
set DESIGN_REF_DATA_PATH "${RTL_DIR}"

set MEM_DIR "${MEM_DIR}"
set SRAM32_DIR "${SRAM32_DIR}"
set SRAM256_DIR "${SRAM256_DIR}"

if {![info exists ADDITIONAL_SEARCH_PATH]} {
    set ADDITIONAL_SEARCH_PATH ""
}

foreach p [list \
    "\$SRAM32_DIR/NLDM" \
    "\$SRAM256_DIR/NLDM" \
] {
    if {[file isdirectory \$p]} {
        set ADDITIONAL_SEARCH_PATH [concat \$ADDITIONAL_SEARCH_PATH [list \$p]]
    } else {
        puts "WARNING: SRAM NLDM path not found: \$p"
    }
}

# SRAM macro link library
# wrapper 中用到：
#   TS1N28HPCPHVTB32X96M4S
#   TS1N28HPCPHVTB256X96M4S

set SRAM_LINK_DBS [concat \
    [glob -nocomplain -directory "\$SRAM32_DIR/NLDM"  "*32x96*m4s*ssg0p81vm40c*.db"] \
    [glob -nocomplain -directory "\$SRAM256_DIR/NLDM" "*256x96*m4s*ssg0p81vm40c*.db"] \
]

if {![info exists ADDITIONAL_LINK_LIB_FILES]} {
    set ADDITIONAL_LINK_LIB_FILES ""
}

set ADDITIONAL_LINK_LIB_FILES [concat \$ADDITIONAL_LINK_LIB_FILES \$SRAM_LINK_DBS]

puts "INFO: DESIGN_NAME              = \$DESIGN_NAME"
puts "INFO: DESIGN_REF_DATA_PATH     = \$DESIGN_REF_DATA_PATH"
puts "INFO: SRAM_LINK_DBS            = \$SRAM_LINK_DBS"
puts "INFO: ADDITIONAL_SEARCH_PATH   = \$ADDITIONAL_SEARCH_PATH"
puts "INFO: ADDITIONAL_LINK_LIB_FILES= \$ADDITIONAL_LINK_LIB_FILES"

# <<< TOOMCOOK1024 AUTO SETUP
EOF

echo "[INFO] Patched rm_setup/common_setup.tcl"

# ============================================================
# 3. 修改 rm_setup/dc_setup.tcl
# ============================================================

DC_SETUP="rm_setup/dc_setup.tcl"
cp "${DC_SETUP}" "${DC_SETUP}.bak_$(date +%Y%m%d_%H%M%S)"

awk '
/# >>> TOOMCOOK1024 RTL SETUP/ {skip=1; next}
/# <<< TOOMCOOK1024 RTL SETUP/ {skip=0; next}
!skip {print}
' "${DC_SETUP}" > "${DC_SETUP}.tmp"
mv "${DC_SETUP}.tmp" "${DC_SETUP}"

cat >> "${DC_SETUP}" <<EOF

# >>> TOOMCOOK1024 RTL SETUP

set RTL_SOURCE_FILES [list \
    "${RTL_DIR}/ToomCook1024.v" \
    "${RTL_DIR}/sp_ram_macro.v" \
]

puts "INFO: RTL_SOURCE_FILES = \$RTL_SOURCE_FILES"

# wrapper 中用了 \$clog2、generate-if、part-select +:
# 因此建议用 SystemVerilog 方式读入
set RTL_SOURCE_FORMAT sverilog

# <<< TOOMCOOK1024 RTL SETUP
EOF

echo "[INFO] Patched rm_setup/dc_setup.tcl"

# ============================================================
# 4. 尝试把 dc.tcl 中 read/analyze 格式改成 sverilog
# ============================================================

if grep -q "analyze.*-format verilog" rm_dc_scripts/dc.tcl; then
    cp rm_dc_scripts/dc.tcl rm_dc_scripts/dc.tcl.bak_$(date +%Y%m%d_%H%M%S)
    perl -pi -e 's/analyze\s+-format\s+verilog/analyze -format sverilog/g' rm_dc_scripts/dc.tcl
    echo "[INFO] Patched rm_dc_scripts/dc.tcl: analyze -format verilog -> analyze -format sverilog"
else
    echo "[INFO] No explicit analyze -format verilog found in rm_dc_scripts/dc.tcl"
fi

echo ""
echo "[DONE] Setup finished."
echo ""
echo "Run DC:"
echo "  ./dc_go.csh ToomCook1024_10ns"
echo ""
echo "Check log:"
echo "  tail -f ToomCook1024_10ns/logs/dc.log"
echo ""