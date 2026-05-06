# Synopsys Reference Methodology 笔记

在 yuxinglong 的28nm工作目录 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/` 下，可见 `2RtlCode`，`4Backend`，`5PostSyn` 三个子目录，分别用来存放 ② RTL 及 Testbench 前端设计代码，④ 后端流程脚本，⑤ 设计仿真验证脚本。事实上，可以多保留 `1Document` 和 `3FPGA` 两个子目录以保证整个芯片项目的完整性与可读性。

本文档着重分析介绍 `4Backend` 和 `5PostSyn` 两个子目录的工作流程。

## 后端设计

### 1DC-RM

DC 逻辑综合的脚本分布在三个子目录，`constraint`，`rm_dc_scripts`，`rm_setup` 分别包含 ①约束信息，②DC命令脚本，③DC环境设置。

对于同一设计同一约束，DC 逻辑综合的结果将是一致的。注意 DC 时序约束过紧也会导致设计功耗增加，即为满足时序约束而使用相较 HVT Cell 功耗高延迟低的 LVT Cell。

#### constraint

`constraint` 目录下为顶层设计的约束信息文件，需要以 RTL 顶层 module 命名，如 顶层 `module DGR_DUT`，则将约束文件命名为 `DGR_DUT.constraints.tcl`。

对于一般的不含有 IO PAD 的模块设计，如果需求经过 逻辑综合 获得 面积/时序/功耗 信息，则主要考虑设置以下信息：

+ `create_clock -name clock_main -period 3 [get_ports "clock"]`，为设计创建时钟域。通过 DC 工具的 get_ports 的命令可以获取数字设计中采用的时钟信号，例如 Verilog 设计中的 `input clock`，此处需要根据具体设计的时钟信号命名做修改。另外注意 -period 3 标记了该命名时钟域 clock_main 的时钟周期数 3ns，此处根据设计需求做更改。
+ `set_clock_uncertainty  0.45 -setup [get_clocks "clock_main"]`，为建立时间考虑时钟不确定性。时钟不确定性一般包括 时钟抖动（Clock Jitter），时钟偏斜（Clock Skew）和 制造偏差（Margin），对于特定的时钟域 clock_main 会一般设置为 其时钟周期的 15%~25% 为 -setup 的时钟不确定性，即 3ns * 15% = 0.45ns，数值越大越严格。
+ `set_clock_uncertainty  0.10 -hold [get_clocks "clock_main"]`，为保持时间考虑时钟不确定性。一般设置 0.05, 0.10, 0.20 为典型值，数值越大越严格。
+ `set_clock_transition   0.05 [get_clocks "clock_main"]`，设置时钟转换时间。一般设置 0.05, 0.10, 0.20 为典型值，用来指定时钟信号在触发器的时钟引脚上的转换时间，数值越小越严格。
+ `set_max_area 2000000.00`，设置单元 Cell 的最大总和面积。根据需求设置。倘若寻求面积的最优化，可以注释掉该行， DC 工具会默认设置为 `set_max_area 0`。
+ `set_ideal_network [get_ports "reset"]` 和 `set_ideal_network [get_ports "clock"]`，设置理想网络。对数字设计中采用的时钟信号 clock 和复位信号 reset 标记为理想网络，这意味着这些网络的电阻和电容都被视为零，同时相关的单元（cells）和网络（nets）都会被设置为不可触碰（dont_touch），即综合过程中不会对它们进行任何优化，它们的延迟也会被视为零。这比单纯的 dont_touch 属性更为极端，因为 ideal_network 甚至不再计算延迟。无需更改。
+ `group_path -name`，时序路径组命名。需要将 `[get_ports "clock"]` 中的时钟信号改为数字设计中的 `input clock`。

对于含有 IO PAD 的模块设计，则需要另外考虑设置以下信息：

+ `create_clock -name clock_main -period 3 [get_pins "clock_PAD/C"]`，从 clock PAD 的引脚 pin `/C` 上获取时钟信号。
+ `set_input_delay` 和 `set_output_delay`，设置输入输出的最大和最小延迟。典型值为时钟周期的 40% 和 2%。可以根据芯片的设计需求灵活调整 Output Delay。`[get_ports  "clock_pad"]` 注意从数字设计的时钟端口处 `input clock_pad` 获取信号。
+ `set_ideal_network`，从 clock PAD 和 reset PAD 的引脚 pin `/C` 上获取时钟和复位信号。
+ `set_driving_cell -lib_cell`，设置驱动单元。DC 默认输入端口信号的 transition 转换时间是 0，设置该输入端口前面的驱动单元，DC 工具会从指定的库中查找得出更加真实的 transition 转换时间来代替 0。
+ `group_path -name`，注意从数字设计的时钟端口处 `input clock_pad` 获取信号 `[get_ports "clock_pad"]`。

对于多时钟域的芯片设计，待以后补充 ==[Future]== 。

#### rm_dc_scripts

内含 DC (design compiler) 和 FM (Formality) 工具执行的命令脚本，大部分情况无需修改，执行完成 DC 逻辑综合后会自动为 逻辑综合的网表文件 做 Formality 形式验证，确保数字设计 RTL 代码与综合后网表功能一致。此次值得提醒的地方有：

+ 在 DC 工具运行的过程中，不要企图在同一个 1DC-RM 目录下修改 `rm_setup/common_setup.tcl` 文件以同时运行多个数字设计的 DC 逻辑综合流程。因为这会导致 DC 工具执行完成后，FM 工具再次读取 `rm_setup/common_setup.tcl` 以错误的 RTL 设计导致形式验证失败。
+ 在 dc.tcl 中默认包含 `compile_ultra -gate_clock -no_autoungroup` 命令启动 DC 综合和优化进程.
  + -gate_clock 自动设置门控时钟以优化功耗
  + -no_autoungroup 关闭自动取消划分特性
  + -scan 可测试性设计，添加 扫描链电路（Scan Chain）
  + -no_boundary_optimization 不作边界优化
  + -area_high_effort_script 面积优化
  + -timing_high_effort_script 时序优化
+ 具体深入的命令学习，请多阅读和思考 `dc.tcl` 和 `fm.tcl` 脚本。

#### rm_setup

此处是 Reference Methodology 的公共设置脚本文件，包括 `common_setup.tcl`，`dc_setup.tcl` 和 `dc_setup_filenames.tcl` 分别表示 ①公共设置 ②DC设置 ③文件命名。

+ `common_setup.tcl` 可能需要修改的地方包括：
   + TCL变量设置：`PRJ_ROOT` 项目根目录, `WORK_DIR` DC 工作目录, `DESIGN_NAME` 数字设计顶层模块名称, `DESIGN_REF_DATA_PATH` RTL 设计目录
   + `ADDITIONAL_SEARCH_PATH` 用于指向 db 库文件的搜索路径，db 库文件一般包含 标准单元库（Std Cell）、I/O PAD 库 和 宏单元（Macro Cell）库
       + 如 $MEM_DIR/ts1n28hpcplvtb32x16m4s_180a/NLDM 代表 SRAM Macro Cell 的搜索路径
       + 如 /export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140_180a 代表 Std Cell 的搜索路径
       + 如 /export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Front_End/timing_power_noise/NLDM/tphn28hpcpgv18_170a 代表 Pad IO 的搜素路径
   + `TARGET_LIBRARY_FILES` 用于表示目标库文件，一般特指标准单元库，存放所要映射的逻辑单元，会设置 HVT cell，LVT cell 和 SVT cell，分别代表 高阈值电压、低阈值电压 和 标准阈值电压 单元库。
       + HVT cell 的特点是阈值电压高，功耗低，速度慢；LVT cell 的特点是阈值电压低，功耗高，速度快；SVT cell 介于两者之间。
       + 通常情况下，DC 综合工具会将 HVT/LVT/SVT 单元库都读入，然后在满足时序约束的前提下自动选择 cell 使用，大部分情况下 HVT/LVT/SVT cell 都会使用，在时序紧张的路径上会大量使用 LVT cell，时序裕量比较大的地方使用 HVT/SVT cell。
       + 如 tcbn28hpcplusbwp40p140hvtssg0p81vm40c_ccs.db 代表 TSMC 28nm 在 ssg 工艺角，0.81V 电压 和 -40℃ 温度条件下的 HVT CELL 工艺库。
       + 注意 ssg, 0.81V 和 -40℃ 条件并非最慢条件而为较慢条件，但 ssg0p81vm40c 为后端流程最为稳妥的条件，不建议做任何更改。ssg & 0.81V & 125℃ 是为最慢条件，ffg & 0.99V & -40℃ 是为最快条件，tt & 0.9V & 25℃ 是为标准条件。
   + `ADDITIONAL_LINK_LIB_FILES` 用于表示链接库文件，一般特指 Pad IO 和 Macro Cell 标准库。
       + 如 tphn28hpcpgv18ssg0p81v1p62vm40c.db 代表 Pad IO 标准库。
       + 如 ts1n28hpcplvtb32x16m4s_ssg0p81vm40c.db 代表 Sram Macro 单元库。
   + `MIN_LIBRARY_FILES` 用于表示 最慢条件 和 最快条件 成对出现 的工艺库，包含所有 Std Cell/IO PAD/Macro Cell 两种条件的工艺库。
   + `MW_REFERENCE_LIB_DIRS` 用于表示 ICC 版图综合后端使用的物理工艺库 MILKYWAY 目录，包含所有 Std Cell/IO PAD/Macro Cell 的 MILKYWAY 目录。
   + `TECH_FILE` 用于表示工艺文件，`.tf` 文件是每种工艺对应的唯一工艺文件，包含各个金属层对应的工艺参数（如图形定义及显示、互连线工艺信息和通孔信息）。
   + `MAP_FILE` 用于表示工艺映射文件，旨在消除各 EDA 公司使用工艺文件不同所带来的差异。
   + `TLUPLUS_MAX_FILE` 和 `TLUPLUS_MIN_FILE` 用于表示寄生参数 RC 查找表文件，需要分别设置最快和最慢情况。
   + 定制 Macro 宏单元（如 SRAM）库需要设置：`ADDITIONAL_SEARCH_PATH`，`ADDITIONAL_LINK_LIB_FILES`，`MIN_LIBRARY_FILES`，`MW_REFERENCE_LIB_DIRS`。
       + 一般 Macro 宏单元的后端设计 必须包含 DATASHEET 数据表，GDSII 版图文件(`.gds`)，LEF 库交换格式文件(`.lef`)，NLDM (Non-linear Delay Model) 非线性延迟模型文件(`.lib`) 和 VERILOG 设计文件(`.v`)
   + 库文件准备过程需要：Library Compiler 工具将 `.lib` 文件转换为 `.db` 文件；Milkyway 工具将 `.lef` 文件转换为 FRAM 和 CEL 文件。
       + CEL 含有完整的版图信息，诸如通孔via、标准单元、宏单元或整个芯片的物理结构，包含cell的布局、布线、引脚和网表信息
       + FRAM 用于布局布线的抽象化的版图物理信息，只有单元的轮廓、引脚位置和层、Metal blockages
+ `dc_setup.tcl` 需要修改的地方仅为 `RTL_SOURCE_FILES` 即 RTL 源代码文件列表
+ `dc_setup_filenames.tcl` 负责输出文件的命名，一般情况下无须更改
+ Makefile 中包含 `make dc` 和 `make fm` 的 shell 执行命令

#### DC 流程自定义

在 28nm 工艺下执行如下操作即可：
+ 复制工程 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/1DC-RM` 至本地目录 `1DC-RM`，注意丢弃 `./dc_go.csh` 产生的运行目录
+ 修改 `rm_setup/common_setup.tcl` 中的 `PRJ_ROOT`（工程根目录），`DESIGN_NAME`（顶层模块名）和 `DESIGN_REF_DATA_PATH`（源文件目录）
+ 如设计中存在 sram macro cell，则需要修改 `rm_setup/common_setup.tcl` 中的 `ADDITIONAL_SEARCH_PATH`（库搜索路径），`ADDITIONAL_LINK_LIB_FILES`（链接库文件），`MIN_LIBRARY_FILES`（最慢/最快库） 和 `MW_REFERENCE_LIB_DIRS`（Milkyway 目录）
+ 修改 `rm_setup/dc_setup.tcl` 中的 `RTL_SOURCE_FILES` RTL 源代码文件列表
+ 设计 `constraint` 路径下的 TOP.constraints.tcl 综合约束文件，其中 `TOP` 必须为数字设计顶层模块的名称
+ 在 `1DC-RM` 目录下执行 `./dc_go.csh XXXX` 即可，其中 `XXXX` 可以根据 数字设计名称及其约束条件 命名
+ 待 dc_shell 执行一段时间后，查看 `XXXX/logs/dc.log` 文件内容以检查 错误 error 和 警告 warning 情况

DC 逻辑综合报告需要重点关注：
+ `TOP.check_design.rpt`: RTL 设计检查报告，可以根据其提示完成 RTL 设计改善
+ `TOP.hier.power.result`：层次型设计功耗结果，可以查看设计功耗的逐模块划分
+ `TOP.power.result`：设计功耗报告，可以查看设计的总功耗
+ `TOP.mapped.area.rpt`：设计面积报告，可以查看设计面积的逐模块划分，注意单位为um^2
+ `TOP.mapped.clock_gating.rpt`: 门控时钟设置总结
+ `TOP.mapped.qor.rpt`: 设计质量报告，总结设计的所有时序、面积和规则违例信息
+ `TOP.max_timing.rpt`：最长时序路径报告，检查建立时间裕量
+ `TOP.min_timing.rpt`：最短时序路径报告，检查保持时间违例，通常情况总会出现违例
+ `TOP.vio.rpt`：设计综合结果违例报告，打印所有的 建立时间违例、保持时间违例、电容负载（max capacitance）违例 和 转换时间（max transition）违例

Verilog RTL 中端设计要求：
+ clock 多时钟域设计
+ reset 统一复位方式
+ pad 确定四边排列数量上限
+ macro 确定类型与拼接
+ dc timing 时序收敛优化
+ dc check_design LINT 完善
+ dc warning 原因确认完善

### 2ICC-RM

ICC 版图综合的脚本分布在 根目录 和 五个子目录，`rm_setup`，`user_scripts`，`rm_icc_scripts`，`rm_icc_dp_scripts` 和 `rm_icc_zrt_scripts` 分别包含 ①ICC环境设置，②用户自定义命令脚本，③ICC公共命令脚本，④ICC设计规划脚本和 ⑤ICC布线命令脚本，而根目录下则是与 STA/DRC/LVS 布线后修复 timing/drc/lvs 相关的命令脚本。

布局布线过程主要由 ICC 工具自动化完成，实验室同学很难进行干预。为使设计的布局布线顺利，我们需要把极大精力放在 floorplan 布局规划上，主要包括 macro cell 摆放和 placement blockage 摆放。只有 floorplan 设计好，典型 70% 标准单元利用率的布局才能成功、布线才不拥塞、DRC 数量才少。一般 28nm 芯片 十层金属 core 面积利用率情况会是 $Area_{macro} / 1.1 + Area_{std} / 0.7$

#### rm_setup

+ `common_setup.tcl` 是 ICC 与 DC 共享的环境设置命令脚本，将 `1DC-RM/rm_setup/common_setup.tcl` 复制到 `2ICC/rm_setup` 即可。
+ `icc_setup.tcl` 中需要更改的地方包括：`DC_TAG` 和 `ICC_ECO_FILE`，此文件有设置 ICC 读入 DC 逻辑综合后的网表文件`.mapped.v`，时序约束文件`.mapped.sdc` 和 综合结果数据库文件`.mapped.ddc`。根据 ICC 版图综合时序需求，容易更改 时序约束文件`.mapped.sdc` 的内容。一般情况下 DC 与 ICC 设置的时钟频率一致，需要修改的地方主要为 set_clock_uncertainty -setup 建立时间时钟不确定性，比如 DC 设置 15% 时钟周期，而 ICC 设置 8% 时钟周期。
+ Makefile_zrt 中包含 `make init_design_icc`，`make flat_dp`，`make place_opt_icc`，`make clock_opt_cts_icc`，`make clock_opt_psyn_icc`，`make route_icc`，`make route_opt_icc`，`make chip_finish_icc`，`make metal_fill_icc`，`make signoff_drc_icc`，`make outputs_icc` 和累积的 `make ic` 的 shell 执行命令。执行 ICC 版图综合的 step-by-step 意见为 init_design_icc -> place_opt_icc -> route_opt_icc，在 init_design_icc 后确定 macro cell 和 placement blockage 的摆放，在 place_opt_icc 后确定 standard cell 能否摆放成功，在 route_opt_icc 后要确定 DRC violations 数量控制在 500 以内。倘若 shell 终端在 route_icc 阶段一直显示 DRC violations 数量为 700w~900w 则可终止运行因为其代表布线不通。对于最后所剩的 DRC violations 需要手动修复，因此所剩 DRC violations 数量越少越好比如 10~20，最终修复 DRC 违例无需参考 ICC 报告，修复全部的 Calibre DRC 违例即可。

#### user_scripts

用户自定义脚本大多数与布局规划 floorplan 相关，注意 floorplan 的创建在 `rm_icc_scripts/init_design_icc.tcl` 文件中：

```tcl
# 28nm IO PAD 的长宽分别为 110um 和 20um
# io2core 的尺寸受 电源环 影响，一般 38um 足够预留 两对电源环 38um > 4 * 9um
# MPW 流片要预留 0.5 * 80um 用于划片，本设计额外压缩 40um
# 为 bonding pad 预留 2 * 11.66um = 23.32 um
# core width = 2000um - 2 * 110um - 2 * 38um - 24um = 1680um
# core height = 2000um - 2 * 110um - 2 * 38um - 40um - 24um = 1640um
create_floorplan \
      -control_type width_and_height \
      -core_width  1680 \
      -core_height 1640 \
      -left_io2core 38 \
      -bottom_io2core 38 \
      -right_io2core 38 \
      -top_io2core 38 \
      -start_first_row \
      -flip_first_row 
```

另外 init_design_icc 这一步可能需要手动调整 TapCell 与 EndCap、Boundary 这些 Physical Only Cells 的摆放，以避免 ERC floating.nxwell_float 问题和 ICC placement legality 问题。

+ `pad_constraints.tcl` IO PAD 约束文件。分布 IO PAD 的物理位置，并增加电源相关 PAD 如 vssio/vddio 和 vsscore/vddcore 分别成对散落出现以给 IO 和 Core 供电。
    + 普通的 PAD 稳定支持 125MHz~150MHz 的极限频率，最高支持 200MHz 的实验频率，如需要高速数字 IO PAD 则需要考虑 LVDS (Low-Voltage Differential Signaling) 低电压差分信号 PAD，其可以支持 500MHz~1GHz 的数字 I/O。
    + 长为 $w$ 的边最多容纳 $(w - 2 \times l_{pad})/max(w_{bond}, 1.5 \times w_{pad})$ 个 PAD，其中 $l_{pad}$ 代表 PAD 的长，$w_{pad}$ 代表 PAD 的宽，$w_{bond}$ 代表 Bonding PAD 的宽。
    + 28nm 标准 IO 库中的 PAD 输入电压为 1.8V，输出电压为 0.9V。
    + 注意添加 poc_cell 对 io pad 进行功耗控制以避免 LVS 违例。
    + 注意倘若任意一边的 PAD 数量为偶数，则最好要在 上下左右四边 均添加一对 dummy pad 用于占位。
+ `create_physical_only_cells.tcl` 创建物理 Cell 单元文件。根据 `pad_constraints` 增加相应的 IO PAD 单元名称，并同时决定电源 PAD 的选型。
+ `create_place_bondpads.tcl` 创建引线键合（Bonding）的 PAD 。根据 `pad_constraints` 增加相应的 IO PAD 单元名称。
    + bonding pad 的摆放规则是 相间出入、角落皆出，相间出入实现芯片引脚的两排摆放，角落皆出保证芯片 bonding pad 不会相互交叠而引起 DRC/LVS 违例。
+ `create_blockage_for_bondpad.tcl` 为 bondpad 创建 blockage 阻塞。需要修改 `$llx+148`，`$lly+148`，`$urx-148`，`$ury-148` 四处的间距值，计算规则为 $w_{pad} + w_{io2core} = 110um + 38um = 148um$
+ `floorplan_physical_constraints.tcl` 布图规划物理约束文件，其主要调用 `pad_rings.tcl`，`place_macro.tcl` 和 `create_blockage_for_bondpad.tcl`。
+ `pad_rings.tcl` 插入 PAD filler 填充物 并 创建电源环。
+ `dp_place_constraints` 设计规划布局约束，内容主要为创建 Macro Cell 的禁止边距。典型设置为 `set_keepout_margin -type hard -outer {2 2 2 2} [all_macro_cells]`
+ `tpns.tcl` 电源规划脚本，实验室流片成功的 tpns.tcl 脚本不建议做任何更改。
+ `place_macro.tcl` 宏 Cell 单元摆放约束文件。此文件将决定后续布局布线的成败，如若数字设计中不存在 Macro Cell，则需删去此文件。为生成数字设计专属的 Macro Cell 摆放约束，需要如下操作：
   + 首先在 2ICC-RM 目录下首先创建一个空文件夹如 Macro，而后 `cd Macro`
   + `make -f ../rm_setup/Makefile_zrt init_design_icc` 即仅执行到 init_design_icc 这一步
   + `icc_shell -gui` 打开 icc 的用户交互界面，依次点击 File -> Open Design 然后选择 init_design_icc 的 CEL 视图开始手动摆放 Macro Cell
   + Macro Cell 的摆放间距规则：间距 > (1 + 0.5) * num_of_pins * (M4) pitch / num_of_route_lower_metal_layers = 0.05 * num_of_pins (um) in 28nm tech.
   + Macro Cell 的横向摆放间距还要考虑至少要放下一组 VDD 与 VSS，否则会造成 LVS Power/Ground Open 开路。或者 floorplan 设计时采用合适的 keepout margin 间距使得 Macro Cell 之间不存在 VSS/VDD 的第一层金属
+ `place_opt_blockage.tcl` 设置标准单元布局阻塞区域。blockage 一般根据 sram macro cell 位置 及 布局拥塞图 确定具体放置，一般可以选择 soft 类型和 partial 类型，创建 blockage 的主要目的在于消除 sram macro cell 临近区域的 congestion 拥塞现象以减少布线时 DRC violations 数量。探索布局规划时可先使用 soft blockage，如若想要降低标准单元密度则可以选择 partial blockage，对于 partial blockage 来说要设置 blocked_percentage，数值越大代表其中标准单元数量越少。需要强调的是，soft blockage 和 partial blockage 只在 placement 阶段起作用，在 legalize，optimize 阶段都不起作用。如若数字设计中不存在 Macro Cell，则需删去此文件。

下面给出一些手动摆放 Macro Cell 可能用到的 TCL 命令：
```tcl
# 改变 Macro Cell 的 is_fixed 和 is_placed 属性 false 使其可以被手动调换位置
if {[all_macro_cells] != "" } {
  set_attribute [all_macro_cells] is_fixed false
  set_attribute [all_macro_cells] is_placed false
}

# 设置 快速摆放 macro cell 的策略
set_fp_placement_strategy -macros_on_edge auto -auto_grouping low -min_distance_between_macros 10 -sliver_size 10
# 快速摆放 macro cell & std cell
create_fp_placement -effort low -exploration

# 改变 Macro Cell 的 is_fixed 和 is_placed 属性 true 而后开始探索模式全局布线，以查看 Macro 连接关系
if {[all_macro_cells] != "" } {
  set_attribute [all_macro_cells] is_fixed true
  set_attribute [all_macro_cells] is_placed true
}

# 探索模式全局布线，以即时查看拥塞并决定 Floorplan 的改进
route_zrt_global -congestion_map_only true

# macro cell 定位及方向，注意 origin 位置并非固定为左下角而与 orientation 相关，FS: 左上；S：右上；N：左下；FN：右下
set_undoable_attribute [get_cells -all u_TS1N28HPCPLVTB1056X16M4S] origin {1035.565  422.800}
set_undoable_attribute [get_cells -all u_TS1N28HPCPLVTB1056X16M4S] orientation S

# 获取全部的 Macro Cell 的 full_name
if {[all_macro_cells] != "" } {
  set name [get_attribute [all_macro_cells] full_name]
}

# 输出 floorplan macro 摆放的脚本文件
write_floorplan -placement { hard_macro } -no_placement_blockage -no_bound -no_plan_group -no_voltage_area -no_route_guide -no_create_boundary -sm_placement { hard_macro } /export4/Home/yvxinglong/Work/BackFlow28/4Backend/2ICC-RM-floorplan2/ecpu_4lane_5ns_hvt_driving/macro.tcl
```

注意手动摆放 Macro Cell，可以选择关闭 Cell-Standard 和 Cell-Physical Only 视图，可以选择打开 Pin-Macro 视图且关闭 Layers-M4 视图以查看 Macro 的 pin 引脚排布，可以选择打开 Global Route 以查看 Cell 连接关系。

#### 脚本调用关系

+ rm_setup/icc_setup.tcl 
    + rm_setup/common_setup.tcl 
+ rm_icc_scripts/init_design_icc.tcl
    + rm_icc_scripts/common_optimization_settings_icc.tcl
    + rm_icc_scripts/common_placement_settings_icc.tcl
        + user_scripts/create_physical_only_cells.tcl
        + user_scripts/floorplan_physical_constraints.tcl
            + user_scripts/pad_rings.tcl
            + user_scripts/place_macro.tcl
            + user_scripts/create_blockage_for_bondpad.tcl
        + user_scripts/pad_constraints.tcl
+ rm_icc_dp_scripts/flat_dp.tcl
    + rm_icc_dp_scripts/proc_explore.tcl
        + user_scripts/dp_place_constraints.tcl
    + rm_icc_dp_scripts/baseline.tcl
        + user_scripts/tpns.tcl
+ rm_icc_scripts/place_opt_icc.tcl
    + user_scripts/place_opt_blockage.tcl
    + rm_icc_scripts/common_cts_settings_icc.tcl 
+ rm_icc_zrt_scripts/clock_opt_cts_icc.tcl
    + rm_icc_scripts/common_post_cts_timing_settings.tcl
+ rm_icc_zrt_scripts/clock_opt_psyn_icc.tcl
+ rm_icc_zrt_scripts/clock_opt_route_icc.tcl
    + rm_icc_scripts/common_route_si_settings_zrt_icc.tcl
+ rm_icc_zrt_scripts/route_icc.tcl
+ rm_icc_zrt_scripts/route_opt_icc.tcl

#### ICC 流程自定义

如是实验室流片项目，磨刀不误砍柴工，建议首先阅读如下材料：
+ [bilibili IC Compiler 免费课程 第六讲/第七讲/第八讲](https://space.bilibili.com/87200978)
+ [icc 图文学习](https://icode.best/category/icc+%E5%9B%BE%E6%96%87%E5%AD%A6%E4%B9%A0)
+ [宏单元摆放的原则和建议](https://www.wolai.com/ej2Rp1FyjaXaoBFcJw3FX2)

在 28nm 工艺下执行如下操作即可：
+ 复制工程 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/2ICC-RM` 至本地目录 `2ICC-RM`，注意丢弃 `./icc_go.csh` 产生的运行目录
+ 将 `4Backend/1DC-RM/rm_setup/common_setup.tcl` 复制到 `4Backend/2ICC-RM/rm_setup/` 下
+ 根据 `./dc_go.csh` 产生的运行目录名称 修改 `rm_setup/icc_setup.tcl` 中的 `DC_TAG` 内容
+ 根据芯片尺寸设计修改 `rm_icc_scripts/init_design_icc.tcl` 中的 `create_floorplan` 命令参数
+ 根据添加 PAD 设计修改 `user_scripts/pad_constraints.tcl`，`user_scripts/create_blockage_for_bondpad.tcl`，`user_scripts/create_physical_only_cells.tcl`
+ 根据布局规划 floorplan 设计修改 `user_scripts/place_macro.tcl` 和 `user_scripts/place_opt_blockage.tcl`
+ 修改 `icc_go.csh` 中的 `make -f ../rm_setup/Makefile_zrt` 决定执行到 init_design_icc，place_opt_icc 还是 route_opt_icc
+ 根据 ICC 设计约束修改 DC 设计约束 `XXXX/results/DGR_DUT.mapped.sdc`，其中 `XXXX` 为 `./dc_go.csh` 产生的运行目录名称
+ 在 `2ICC-RM` 目录下执行 `./icc_go.csh XXXX XXXX` 即可，其中 `XXXX` 为 `./dc_go.csh` 产生的运行目录名称
+ 待 icc_shell 执行一段时间后，查看 `XXXX/logs_zrt` 中的日志文件内容以检查 错误 error 和 警告 warning 情况

ICC 版图综合报告需要重点关注：
+ `snapshot/init_design_icc.qor.rpt`: ICC 初始化设计质量报告，总结设计的所有时序、面积和规则违例信息
+ `snapshot/init_design_icc.tim.max.rpt`: ICC 初始化设计最长时序路径报告，出现 setup 违例很正常
+ `snapshot/init_design_icc.ss.sum`: ICC 初始化设计总结报告
+ `snapshot/place_opt_icc.qor.rpt`: ICC 布局优化设计质量报告，总结设计的所有时序、面积和规则违例信息
+ `snapshot/place_opt_icc.tim.max.rpt`: ICC 布局优化设计最长时序路径报告，出现 setup 违例很正常
+ `snapshot/place_opt_icc.ss.sum`: ICC 布局优化设计总结报告
+ `snapshot/place_opt_icc.phys.rpt`: ICC 布局优化资源利用率报告
+ `snapshot/clock_opt_route_icc.qor.rpt`: ICC 时钟树综合优化设计质量报告，总结设计的所有时序、面积和规则违例信息
+ `snapshot/clock_opt_route_icc.tim.max.rpt`: ICC 时钟树综合优化设计最长时序路径报告，出现 setup 违例很正常
+ `snapshot/clock_opt_route_icc.ss.sum`: ICC 时钟树综合优化设计总结报告
+ `snapshot/clock_opt_route_icc.clk.rpt`: ICC 时钟树综合优化设计时钟树报告
+ `snapshot/route_opt_icc.qor.rpt`: ICC 布线优化设计质量报告，总结设计的所有时序、面积和规则违例信息
+ `snapshot/route_opt_icc.tim.max.rpt`: ICC 布线优化设计最长时序路径报告，如若出现时序裕量稍负的 setup 违例是可以接受的
+ `snapshot/route_opt_icc.ss.sum`: ICC 布线优化设计总结报告
+ `snapshot/route_opt_icc.phys.rpt`: ICC 布线优化资源利用率报告
+ `snapshot/route_opt_icc.clk.rpt`: ICC 布线优化设计时钟树报告、
+ `reports/route_icc.GR.png`: ICC 布线后的拥塞图，据此可以调整 blockage 设置
+ `reports/route_opt_icc.power`: ICC 布线优化设计功耗报告，可以查看设计的总功耗

### 3STA

在做 PrimeTime (PT) STA 修复时序之前，需要先完成 calibre 版图融合 和 starrc 寄生参数提取。

PT STA 这一阶段主要通过 ECO (Engineering Change Order) 工程变更单的方式修复版图中的 漏电电流/保持时间/最大转换时间/最大电容负载 问题。首先需要 PT 完成各种工艺角下的 STA 以获取当前版图的时序违例情况，而后通过 DMSA (Distributed Multi-Scenario Analysis) 自动修复输出 eco.tcl 用于 ICC 执行修改以完成修复。

[STA之RC Corner](https://www.cnblogs.com/lelin/p/11421362.html)
[STA之RC Corner拾遗](https://www.cnblogs.com/lelin/p/11421519.html)
[静态时序分析（一）](https://www.cnblogs.com/qianbinbin/p/17603907.html)
[对症下药，方能药到病除——如何修复drv?](https://blog.csdn.net/Tao_ZT/article/details/102456886)

时序路径未覆盖的原因可以参考 [STA及PT工具](https://blog.csdn.net/niannian18/article/details/116270282) 和 [sta时reset端没有min-pulse_width检查](https://bbs.eetop.cn/thread-414996-1-1.html)

#### 脚本调用关系

+ `5PV/run_wo_dummy_sta.sh` 修改 `tracy_block_name`
  + `5PV/merge_full_gds/gds_merge.sh` 版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_full_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `3EXTRACT_RC/wo_dummy/clear_starrc` 清理 starrc 运行目录 `3EXTRACT_RC/wo_dummy/rundir` 产生的文件
  + `3EXTRACT_RC/wo_dummy/wait_go` 寄生参数提取，修改 `block`
    + `3EXTRACT_RC/wo_dummy/script/ref_star_milkyway.cmd` 增加 LEF_FILE 以添加 sram macro lef 文件
    + `3EXTRACT_RC/wo_dummy/go` 启动 StarXtract 提取 RC
  + `4STA/pt_run.csh` 静态时序分析 PT STA，注意修改整个工作目录中的顶层设计名称 DGR_DUT
    + `4STA/wait.pl` 检查寄生参数 RC 提取是否完成
    + `4STA/run_pt` 启动 STA，修复时序 timing 主要分为 ⼿动修复 和 基于 DMSA ⾃动修复 两种情况
      + `4STA/scripts/pt_run.tcl` 静态时序分析执行的主要脚本命令文件
        + `4STA/scripts/setup.tcl` 在 `set netlistList` 中修改设计网表文件，在 `set sdcList(func)` 中修改设计约束文件，在 `set spefList()` 中修改 寄生参数 RC 文件
        + `4STA/scripts/pt_load_lib.tcl` 在 `set search_path` 中增加 sram macro NLDM 目录，在 `foreach mem` 中修改 sram macro 名称
        + `4STA/scripts/pt_constraints.tcl` 根据设计约束仅修改 `set_clock_uncertainty -setup` 和 `set_clock_uncertainty -hold`
      + `4STA/sub_pt.csh` 脚本自动生成以多线程执行 STA 
      + `4STA/sub_dmsa.csh` 脚本自动生成以自动修复时序
  + `4STA/gen_qor_html.tcl` 生成 html 格式易视化显示时序报告

+ `4STA/dmsa_run.csh` 基于 DMSA 自动修复 leakage/hold/max_tran/max_cap。
  + 基于 `4STA/fix.tcl` 中命令修复 leakage/hold/max_tran/max_cap。
    + `fix_eco_leakage -pattern_priority {P140HVT P140 P140LVT} -verbose` 修复漏电电流，将 LVT 单元替换为 SVT/HVT 单元。
    + `fix_eco_timing -type setup -slack_lesser_than 0.01 -setup_margin 0.01` 修复建立时间的少许违例。
    + `fix_eco_timing -type hold -buffer_list {DEL025MD1BWP40P140HVT DEL050MD1BWP40P140HVT DEL100MD1BWP40P140HVT DEL150MD1BWP40P140HVT BUFFD2BWP40P140HVT} -verbose` 插入 buffer 以修复保持时间违例，很有可能单一命令无法在 PT DMSA 修复全部的保持时间违例，可以多迭代几次 ICC/STA。
    + `fix_eco_timing -type hold -cell_type sequential -methods size_cell -verbose` 通过替换标准单元以修复保持时间违例。
    + `fix_eco_drc -type max_transition -methods size_cell -verbose` 通过替换为驱动能力更强的标准单元以修复最大转换时间违例。
    + `fix_eco_drc -type max_cap -methods insert_buffer -buffer_list BUFFD4BWP40P140HVT -verbose` 通过插入 buffer 以修复最大电容负载违例。
    + `remote_execute {write_changes -format icctcl -output /export4/Home/yvxinglong/Work/BackFlow28/4Backend/4STA/$tag/eco_.tcl}` 导出 PT 所做的时序修复改动 `eco_.tcl` 。

#### PT 流程自定义

在 28nm 工艺下执行如下操作即可：
+ 复制 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/cave` 至本地目录 `cave`，然后定制 数字设计顶层名称 相关 dummy 的 .lef 文件和 Milkyway 文件
  + 修改 `cave/dummy/LEF/dummy.lef` 中的 `DGR_DUT` 字段为 数字设计顶层名称
  + 修改 `cave/dummy/milkyway_work_dummy.cmd` 中的 `DGR_DUT` 字段和 `tsmcn28_10lm5X2Y2RUTRDL.tf` 文件路径
  + 在 `cave/dummy` 目录下 shell 终端执行 `Milkyway -load milkyway_work_dummy.cmd`
  + 将 Milkyway 工具 生成的文件夹名称 改为 MILKYWAY
+ 复制 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/3EXTRACT_RC` 至本地目录 `3EXTRACT_RC`，注意丢弃运行产生的目录 `3EXTRACT_RC/wh_dummy/rundir` 和 `3EXTRACT_RC/wo_dummy/rundir` 中的内容，但是名为 rundir 的空文件夹要保留
  + 在 `3EXTRACT_RC/wh_dummy/script/ref_star_milkyway.cmd` 和 `3EXTRACT_RC/wo_dummy/script/ref_star_milkyway.cmd` 修改增加 `LEF_FILE` 相应的 `.lef` 文件路径
+ 复制 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/4STA` 至本地目录 `4STA`，注意 仅要保留目录下的所有脚本文件 和 `4STA/scripts` 子目录，其余子目录均为运行时产生的
  + 修改 `4STA/scripts/setup.tcl` 中的 `set netlistList` 设计网表文件路径、`set sdcList(func)` 中的 设计约束文件路径、`set spefList()` 中的 寄生参数 RC 文件路径
  + 修改 `4STA/scripts/pt_load_lib.tcl` 中的 `set search_path` 增加 sram macro NLDM 目录，并在 `foreach mem` 中增加修改 sram macro 名称
  + 修改 `4STA/scripts/pt_constraints.tcl` 根据设计约束仅调整 `set_clock_uncertainty -setup` 和 `set_clock_uncertainty -hold`
  + 修改 `4STA/wait.pl` 中的 `my $TOP` 为 数字设计顶层名称
  + 修改 `4STA/write.tcl` 和 `4STA/fix.tcl` 中的 eco_.tcl 输出路径
  + 修改 `4STA/sdf_out.csh` 中的 `DGR_DUT` 字段为 数字设计顶层名称
+ 复制 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/5PV` 至本地目录 `5PV`
  + 修改 `5PV/run_ant_bond.sh`、`5PV/run_drc.sh`、`5PV/run_drc_lvs.sh`、`5PV/run_drc_lvs_sta.sh`、`5PV/run_full.sh`、`5PV/run_lvs.sh`、`5PV/run_wh_dummy_sta.sh`、`5PV/run_wo_dummy_sta.sh` 中的 `tracy_block_name` 赋值为 数字设计顶层名称
  + 在 `5PV/ant`、`5PV/ant_min`、`5PV/bond`、`5PV/drc`、`5PV/dummy` 目录下执行 ./clear_drc.sh，在 `5PV/lvs` 目录下执行 ./clear_lvs.sh，在 `5PV/v2lvs` 目录下执行 ./clear.sh，清理运行时产生的目录
  + 修改 `5PV/ant`、`5PV/ant_min`、`5PV/bond`、`5PV/drc`、`5PV/dummy` 目录下 `rundrc.sh` 中的 `RELEASE_BLOCK`，修改 `5PV/lvs` 目录下 `runlvs.sh` 中的 `RELEASE_BLOCK`，修改 `5PV/v2lvs` 目录下 `v2lvs.sh` 中的 `RELEASE_BLOCK`，修改 `5PV/merge_dummy_gds`、`5PV/merge_full_gds` 目录下 `gds_merge.sh` 中的 `RELEASE_BLOCK`，修改 `5PV/dummy` 目录下 `dummy_change_name.tcl` 中的 `RELEASE_BLOCK`
  + 在 `5PV/merge_dummy_gds`、`5PV/merge_full_gds` 目录下 `gds_merge.sh` 修改增加 set libs 中的 `.gds` 文件路径
  + 在 `5PV/v2lvs/spice_include_file/all_include` 修改增加 `.INCLUDE` 相应的 `.spi` 文件路径
+ 来到 `2ICC-RM` 目录导出布局布线后的 网表 和 gds
  + 修改 `2ICC-RM/to_calibre.tcl` 中的 `DRC_FIX_CEL_NAME` 名称 和 `DGR_DUT` 字段，首次需要解开注释 `copy_mw_cel -from route_opt_icc -to $DRC_FIX_CEL_NAME` 并加上注释 `verify_lvs -max_error 10000 -ignore_floating_port -ignore_floating_net -check_open_locator -check_short_locator`
  + 在 `2ICC-RM/XXXX` 目录下执行 `icc_shell -x "source ../to_calibre.tcl; quit"`，其中 `XXXX` 为 `./icc_go.csh` 产生的运行目录名称
+ 来到 `5PV` 目录开始融合版图、提取寄生参数、静态时序分析
  + 在 `5PV` 目录下执行 `./run_wo_dummy_sta.sh XXXX`，其中 `XXXX` 为 `$DRC_FIX_CEL_NAME` 命名
  + 待 PT 多线程执行完成 STA 后，在 `4STA/XXXX` 目录下执行 `../gen_qor_html.tcl`，随后即可在 `4STA/XXXX/html` 目录下用浏览器打开 `qor.html` 以查看静态时序分析报告，其中 `XXXX` 为 `$DRC_FIX_CEL_NAME` 命名
  + 在 `4STA` 目录下执行 `./dmsa_run.csh XXXX`，而后在 pt_shell 中分别输入 `4STA/fix.tcl` 中的命令 修复 漏电电流/保持时间/最大转换时间/最大电容负载 违例，注意及时在 pt_shell 中查看修复后的违例数量是否降至归零，最后需要输出 eco__.tcl 文件。倘若想避免逐行输入可用 `source ./wo_dummy_all_fix.tcl` 替代
+ 回到 `2ICC-RM` 目录执行修复并再次导出 布局布线后的 网表 和 gds
  + 修改 `2ICC-RM/legalize_and_route.tcl` 中的 `PREVIOUS_VERSION_CEL_NAME` 和 `NEXT_VERSION_CEL_NAME` 名称 和 `DGR_DUT` 字段
  + 在 `2ICC-RM/XXXX` 目录下执行 `icc_shell -x "source ../legalize_and_route.tcl; quit"`，其中 `XXXX` 为 `./icc_go.csh` 产生的运行目录名称
+ 回到 `5PV` 目录再次开始融合版图、提取寄生参数、静态时序分析、DMSA 修复违例，直到不剩或者剩余少许违例可以等到 添加 filler 和 dummy 后再反复迭代解决违例
  + 注意 output pad 可能会导致 setup 违例，在一些情况下等到最终 signoff 的 sta 时可以适当将 output delay 从 40% T 放松到 20% T 等
  + 注意 input pad 可能会导致 max capacitance 违例，这属于假性违例可以 waive 忽略掉

### 4LVS-ICC

#### 脚本调用关系

+ `2ICC-RM/lvs_init.tcl` 执行 ICC LVS 前的初始化操作，在这一步主要检查 短路/开路 问题，并提前通过 create_user_shape 在第 10 层金属添加 LOGO
  + `2ICC-RM/ref_ctrl.tcl` 添加 MILKYWAY 库路径，尤其要注意添加自主定制 dummy 的 MILKYWAY 库路径
  + `scripts/add_std_filler.tcl` 添加 filler 用于填充芯片内部的空白部分，用于连接芯片内部的扩散层和电源线，使阱保持连续，满足 DRC 的检查要求
  + `2ICC-RM/user_scripts/create_place_bondpads.tcl` 创建 bonding pad 用于封装时引线键合芯片引脚
  + `2ICC-RM/create_pad_label.tcl` 为 pad 添加标签以满足 LVS 的检查要求，尤其要注意 POC_CELL 的 text label
  + `2ICC-RM/add_egg.tcl` 在芯片版图原点添加 1um x 1um 的 egg dummy
  + `2ICC-RM/verify_lvs.tcl` ICC 检查芯片版图的 LVS 违例

+ `5PV/run_wh_dummy_sta.sh` 修改 `tracy_block_name`
  + `5PV/merge_full_gds/gds_merge.sh` 版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_full_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `5PV/dummy/clear_drc.sh` 清理 calibre 运行时在目录 `5PV/dummy` 产生的文件
  + `5PV/dummy/rundrc.sh` 将 dummy cell 融合入版图以改善金属密度
  + `3EXTRACT_RC/wh_dummy/clear_starrc` 清理 starrc 运行目录 `3EXTRACT_RC/wh_dummy/rundir` 产生的文件
  + `3EXTRACT_RC/wh_dummy/wait_go` 寄生参数提取，修改 `block`
    + `3EXTRACT_RC/wh_dummy/script/ref_star_milkyway.cmd` 增加 LEF_FILE 以添加 sram macro lef 文件
    + `3EXTRACT_RC/wh_dummy/go` 启动 StarXtract 提取 RC
  + `4STA/pt_run.csh` 静态时序分析 PT STA 
    + `4STA/run_pt` 启动 STA，修复时序 timing 主要分为 ⼿动修复 和 基于 DMSA ⾃动修复 两种情况
      + `4STA/scripts/pt_run.tcl` 静态时序分析执行的主要脚本命令文件
        + `4STA/scripts/setup.tcl` 在 `set netlistList` 中修改 DC 设计网表文件，在 `set sdcList(func)` 中修改 PT 设计约束文件，在 `set spefList()` 中修改 寄生参数 RC 文件
        + `4STA/scripts/pt_load_lib.tcl` 在 `set search_path` 中增加 sram macro NLDM 目录，在 `foreach mem` 中修改 sram macro 名称
        + `pt_constraints.tcl` 根据设计约束仅修改 `set_clock_uncertainty -setup` 和 `set_clock_uncertainty -hold`
      + `4STA/scripts/dmsa/dmsa_pwr_flow.tcl`
  + `4STA/gen_qor_html.tcl` 生成 html 格式易视化显示时序报告

+ 倘若有时序违例，则 `4STA/dmsa_run.csh` 基于 DMSA 自动修复 leakage/setup/hold/max_tran/max_cap，放心大概率仍会出现时序违例问题。

#### 添加 filler & dummy 的 ICC & PT 流程自定义

在 28nm 工艺下执行如下操作即可：
+ 在 `2ICC-RM/XXXX` 目录下执行 `icc_shell -x "source ../lvs_init.tcl; quit"` 
  + 注意提前修改 reference 工艺库在文件 `2ICC-RM/ref_ctrl.tcl` 内的 REFERENCE 条目中
  + 注意提前修改 `2ICC-RM/lvs_init.tcl` 文件中 create_user_shape 参数以添加定制 logo，提前修改 `PREVIOUS_VERSION_CEL_NAME` 和 `NEXT_VERSION_CEL_NAME` 名称 和 `DGR_DUT` 字段
  + 注意及时查看 LVS 报告而后解决 Short 短路和 Open 开路问题，其余 floating net / min area 问题可以靠后处理，floating port 问题可以 waive 忽略掉
  + Short 短路问题需要找到具体的走线，而后手动将版图上的短路走线删掉，将 Short 问题转换成 Open 问题，而后用 route_zrt_eco -open_net_driven true 命令修复 Open 问题，再导出 网表和 gds
+ 来到 `5PV` 目录开始 添加 dummy、融合版图、提取寄生参数、静态时序分析，直到解决所有违例
  + 在 `5PV` 目录下执行 `./run_wh_dummy_sta.sh XXXX`，其中 `XXXX` 为 `$NEXT_VERSION_CEL_NAME` 命名
  + 待 PT 多线程执行完成 STA 后，在 `4STA/XXXX` 目录下执行 `../gen_qor_html.tcl`，随后即可在 `4STA/XXXX/html` 目录下用浏览器打开 `qor.html` 以查看静态时序分析报告，其中 `XXXX` 为 `$NEXT_VERSION_CEL_NAME` 命名
  + 在 `4STA` 目录下执行 `./dmsa_run.csh XXXX`，而后在 pt_shell 中分别输入 `4STA/fix.tcl` 中的命令 修复 漏电电流/保持时间/最大转换时间/最大电容负载 违例，注意及时在 pt_shell 中查看修复后的违例数量是否降至归零，最后需要输出 eco__.tcl 文件。倘若想避免逐行输入可用 `source ./wh_dummy_all_fix.tcl` 替代
+ 在 `2ICC-RM/XXXX` 目录下执行 `icc_shell -x "source ../legalize_and_route_filler.tcl; quit"` 修复违例
  + 注意提前修改 `2ICC-RM/legalize_and_route_filler.tcl` 文件中 `PREVIOUS_VERSION_CEL_NAME` 和 `NEXT_VERSION_CEL_NAME` 名称 和 `DGR_DUT` 字段
+ 回到 `5PV` 目录再次 添加 dummy、融合版图、提取寄生参数、静态时序分析、DMSA 修复违例，直到解决所有违例

### 5PV

是否填充 filler，是否融入 dummy，LVS 检查是否存在开路和短路，这些情况都将影响 calibre 生成 drc 的数量。calibre lvs 与 calibre drc 解耦，可以同时进行以尽快排查芯片版图的违例。

Physical Only Cells 可以参考 [contact CO](https://bbs.eetop.cn/thread-422944-1-1.html)，[TapCell](https://blog.csdn.net/weixin_37584728/article/details/142459505)，[well tap cells的结构](https://blog.csdn.net/weixin_41464428/article/details/119064362)，[cell orientation](https://bbs.eetop.cn/thread-769601-1-1.html)，[Nwell floating when doing ERC check](https://www.edaboard.com/threads/nwell-floating-when-doing-erc-check.410768/)

#### EDA 工具目录

实验室的 EDA 软件工具目录集中于 `/export/Apps`，`/export/Apps/Cadence` 目录包含 Cadence 家的 ASSURA 芯片后端验证工具、GENUS 逻辑综合工具、virtuoso 集成电路设计工具 和 Spectre 电路仿真工具，`/export/Apps/Mentor` 目录包含 Mentor 家的 Calibre 芯片后端验证工具， `/export/Apps/Synopsys` 目录主要包含 Synopsys 家的 Formality 形式化验证工具、IC Compiler 与 IC Compiler II 布局布线工具、Library compiler 库转换工具、Milkyway 物理数据库工具、PrimeTime 静态时序分析和功耗分析工具、StarRC 寄生参数提取工具、Design Compiler 逻辑综合工具、VCS 仿真验证工具、Verdi 仿真波形查看工具 等。

#### 28nm 工艺库常用目录

+ 工艺库 RTL 模型
  + HVT 标准库 RTL 模型 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/verilog/tcbn28hpcplusbwp40p140hvt_110a/tcbn28hpcplusbwp40p140hvt.v`
  + LVT 标准库 RTL 模型 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/verilog/tcbn28hpcplusbwp40p140lvt_110a/tcbn28hpcplusbwp40p140lvt.v`
  + SVT 标准库 RTL 模型 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/verilog/tcbn28hpcplusbwp40p140_110a/tcbn28hpcplusbwp40p140.v`
  + 标准 IO 库 RTL 模型 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Front_End/verilog/tphn28hpcpgv18_110a/tphn28hpcpgv18.v`
+ 逻辑库文件 `.db`
  + HVT 标准库文件目录 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140hvt_180a`
  + LVT 标准库文件目录 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140lvt_180a`
  + SVT 标准库文件目录 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn28hpcplusbwp40p140_180a`
  + 标准 IO 库文件目录 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Front_End/timing_power_noise/NLDM/tphn28hpcpgv18_170a`
+ 物理库文件 MILKYWAY
  + HVT 标准库 MILKYWAY 物理库文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140hvt_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140hvt`
  + LVT 标准库 MILKYWAY 物理库文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140lvt_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140lvt`
  + SVT 标准库 MILKYWAY 物理库文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/milkyway/tcbn28hpcplusbwp40p140_110a/cell_frame_VHV_0d5_0/tcbn28hpcplusbwp40p140`
  + 标准 IO 库 MILKYWAY 物理库文件 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Back_End/milkyway/tphn28hpcpgv18_110a/mt_2/8lm/frame_only/tphn28hpcpgv18`
  + Bonding PAD 库 MILKYWAY 物理库文件 `/export/Library/TN28HPC+/IO_ext/tpbn28v_140b/TSMCHOME/digital/Back_End/milkyway/tpbn28v_140a/cup/10m/10M_5X2Y2R/cell_frame/tpbn28v`
+ 库交换格式文件 `.lef`
  + HVT 标准库 `.lef` 库交换格式文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/lef/tcbn28hpcplusbwp40p140hvt_110a/lef/tcbn28hpcplusbwp40p140hvt.lef`
  + LVT 标准库 `.lef` 库交换格式文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/lef/tcbn28hpcplusbwp40p140lvt_110a/lef/tcbn28hpcplusbwp40p140lvt.lef`
  + SVT 标准库 `.lef` 库交换格式文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/lef/tcbn28hpcplusbwp40p140_110a/lef/tcbn28hpcplusbwp40p140.lef`
  + 标准 IO 库 `.lef` 库交换格式文件 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Back_End/lef/tphn28hpcpgv18_110a/mt_2/8lm/lef/tphn28hpcpgv18_8lm.lef`
  + Bonding PAD 库 `.lef` 库交换格式文件 `/export/Library/TN28HPC+/IO_ext/tpbn28v_140b/TSMCHOME/digital/Back_End/lef/tpbn28v_140b/cup/10m/10M_5X2Y2R/lef/tpbn28v_10lm.lef`
+ 版图文件 `.gds`
  + HVT 标准库 `.gds` 版图文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/gds/tcbn28hpcplusbwp40p140hvt_110a/tcbn28hpcplusbwp40p140hvt.gds`
  + LVT 标准库 `.gds` 版图文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/gds/tcbn28hpcplusbwp40p140lvt_110a/tcbn28hpcplusbwp40p140lvt.gds`
  + SVT 标准库 `.gds` 版图文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/gds/tcbn28hpcplusbwp40p140_110a/tcbn28hpcplusbwp40p140.gds`
  + 标准 IO 库 `.gds` 版图文件 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Back_End/gds/tphn28hpcpgv18_110a/mt_2/8lm/tphn28hpcpgv18.gds`
  + Bonding PAD 库 `.gds` 版图文件 `/export/Library/TN28HPC+/IO_ext/tpbn28v_140b/TSMCHOME/digital/Back_End/gds/tpbn28v_140a/cup/10m/10M_5X2Y2R/tpbn28v.gds`
+ SPICE 仿真电路文件 `.spi`
  + HVT 标准库 SPICE 仿真电路文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/spice/tcbn28hpcplusbwp40p140hvt_110a/tcbn28hpcplusbwp40p140hvt_110a.spi`
  + LVT 标准库 SPICE 仿真电路文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/spice/tcbn28hpcplusbwp40p140lvt_110a/tcbn28hpcplusbwp40p140lvt_110a.spi`
  + SVT 标准库 SPICE 仿真电路文件 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Back_End/spice/tcbn28hpcplusbwp40p140_110a/tcbn28hpcplusbwp40p140_110a.spi`
  + 标准 IO 库 SPICE 仿真电路文件 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Back_End/spice/tphn28hpcpgv18_110a/tphn28hpcpgv18.spi`
+ virtuoso 相关文件
  + virtuoso 映射文件 `/export/Library/TN28HPC+/Virtuoso_Technology_File/tn28clle002_2_0a/TechnologyFile_Virtuoso_28nm_V20a/mapfile/virtuoso_N28_1P10M_5X2Y2R.20a.map`
  + virtuoso 技术文件 `/export/Library/TN28HPC+/Virtuoso_Technology_File/tn28clle002_2_0a/TechnologyFile_Virtuoso_28nm_V20a/techfile/virtuoso_N28_1P10M_5X2Y2R.20a.tf`
  + virtuoso 展示格式文件 `/export/Library/TN28HPC+/Virtuoso_Technology_File/tn28clle002_2_0a/TechnologyFile_Virtuoso_28nm_V20a/display.drf`
  + virtuoso 建库文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/cds.lib`
+ 工艺库相关文件
  + Synopsys 技术文件 `/export/Library/TN28HPC+/Technology_File/N28_PRTF_Syn_v1d5a/PR_tech/Synopsys/TechFile/VHV/tsmcn28_10lm5X2Y2RUTRDL.tf`
  + Cadence 技术文件 `/export/Library/TN28HPC+_Package/HX-200_TF/N28HP_N28HPL_N28HPM_N28LP_PRTF_Cad_v1d4a/PR_tech/Cadence/LefHeader/VHV/tsmcn28_10lm5X2Y2RUTRDL.tlef`
  + GDS 输出层映射文件 `/export/Library/TN28HPC+/Technology_File/N28_PRTF_Syn_v1d5a/PR_tech/Synopsys/GdsOutMap/gdsout_5X2Y2R.map`
  + 天线效应规则文件 `/export/Library/TN28HPC+/Technology_File/N28_PRTF_Syn_v1d5a/PR_tech/Synopsys/SCM/antennaRule_n28_10lm.tcl`
  + StarRC 的映射文件 `/export/Library/TN28HPC+/PEXT_rules/Star_RCXT/1p10m_5x2y2r/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_typical/Reference/MAP/star.map_icc_cln28hpc+_1p10m_5x2y2r_ut-alrdl`
  + StarRC 的映射文件 `/export/Library/TN28HPC+/PEXT_rules/Star_RCXT/1p10m_5x2y2r/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_typical/Reference/MAP/star.map_lefdef_cln28hpc+_1p10m_5x2y2r_ut-alrdl`
  + StarRC 的映射文件 `/export/Library/TN28HPC+/PEXT_rules/Star_RCXT/1p10m_5x2y2r/RC_Star-RCXT_cln28hpc+_1p10m+ut-alrdl_5x2y2r_typical/Reference/MAP/star.map_dummy_gds_cln28hpc+_1p10m_5x2y2r_ut-alrdl`
  + 互连线工艺 `itf` 文件和 nxtgrd 文件目录 `/export/Library/TN28HPC+/PEXT_rules/Star_RCXT/1p10m_5x2y2r`
  + Dummy TCD 文件 `/export/Library/TN28HPC+_Package/Foundry_Design_Documents/tn28cldr002_1_5/N28_TCD_library_kits_20110323.tar.gz`
  + ICOVL 文件 `/export/Library/TN28HPC+_Package/Foundry_Design_Documents/tn28cldr002_1_5/N28_OVL_library_kits.tar.gz`
+ PDK 规则文件
  + LVS 过程 SPICE 网表生成所需文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/lvs/source.added`
  + LVS 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/lvs/calibre.lvs`
  + DRC 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/drc/calibre.drc`
  + BOND 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/drc/CN28_WIRE_BOND_10M_5X2Y2R.15a`
  + ANT 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/drc/CLN28HP_10M.ANT_002.17b`
  + DUMMY 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/dummy_util/Dummy_Metal_Via_Calibre_28nm_13a_nopdf.tar.gz`
  + DUMMY 规则文件 `/export/Library/TN28HPC+/PDK_installed/TN28CRSP025W1_1_0_2P2A/iPDK_CRN28HPC+_v1.0_2p2a_20170531_all/iPDK_CRN28HPC+_v1.0_2p2a_20170531/Calibre/dummy_util/Dummy_OD_PO_Calibre_28nm_HP_13a_nopdf.tar.gz`
  + (新) LVS 过程 SPICE 网表生成所需文件 `/export/Library/TN28HPC+/PDK_installed/tn28clsp079w1_1_0_2p2a/iPDK_CLN28HPC+_v1.0_2p2a_20150612_all/iPDK_CLN28HPC+_v1.0_2p2a_20150612_LO_1P10M_5X2Y2R/Calibre/lvs/source.added`
  + (新) LVS 规则文件 `/export/Library/TN28HPC+/UseLatestRules/lvs_xrc/CALIBRE_FLOW/DFM_LVS_RC_CALIBRE_N28HP_1p10M_5X2Y2R_ALRDL.v1.0_3p`
  + (新) DRC 规则文件 `/export/Library/TN28HPC+/UseLatestRules/drc/CLN28HP_10M_5X2Y2R_002.20b.encrypt.options`
  + (新) ANT 规则文件 `/export/Library/TN28HPC+/UseLatestRules/ant/CLN28HP_10M_002_ANT.20b`
  + (新) BOND 规则文件 `/export/Library/TN28HPC+/UseLatestRules/package/CN28_WIRE_BOND_10M_5X2Y2R.15b`
  + (新) BOND 规则文件 `/export/Library/TN28HPC+/UseLatestRules/package/CN28_WIRE_BOND_10M_5X2Y2R.16a`
  + (新，未用) DUMMY 规则文件 `/export/Library/TN28HPC+/DUMMY_rules/ODPO/Dummy_OD_PO_Calibre_28nm_HP_19_1a/Dummy_OD_PO_Calibre_28nm_HP.19_1a.encrypt`
  + (新，未用) DUMMY 规则文件 `/export/Library/TN28HPC+/DUMMY_rules/MetalVia/Dummy_Metal_Via_Calibre_28nm.19_1a/Dummy_Metal_Via_Calibre_28nm.19_1a.encrypt`
+ 文档
  + signoff STA 参考文件 `/export/Library/TN28HPC+_Package/[2016.09.30]_N28HPC+_Sign-off_Recommendation.pdf`
  + LVS 规则文档 `/export/Library/TN28HPC+_Package/Foundry_Design_Documents/tn28clls002_1_0_3/TN28CLLS002_1_0_3.pdf`
  + DRC 规则文档 `/export/Library/TN28HPC+_Package/Foundry_Design_Documents/tn28cldr002_1_5/TN28CLDR002_1_5.pdf`
  + BOND 规则文档 `/export/Library/TN28HPC+/Package_rules/bump_rules/TN28CLDR017_1_5.pdf`
  + HVT 标准库 cell 文档 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/documents/tcbn28hpcplusbwp40p140hvt_110c/DB_TCBN28HPCPLUSBWP40P140HVT_TT0P9V0P9V25C.pdf`
  + LVT 标准库 cell 文档 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/documents/tcbn28hpcplusbwp40p140lvt_110c/DB_TCBN28HPCPLUSBWP40P140LVT_TT0P9V0P9V25C.pdf`
  + SVT 标准库 cell 文档 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/documents/tcbn28hpcplusbwp40p140_110c/DB_TCBN28HPCPLUSBWP40P140_TT0P9V0P9V25C.pdf`
  + HVT 标准库 发行说明 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/release_note/RN_TCBN28HPCPLUSBWP40P140HVT_190A.pdf`
  + LVT 标准库 发行说明 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/release_note/RN_TCBN28HPCPLUSBWP40P140LVT_190A.pdf`
  + SVT 标准库 发行说明 `/export1/Library/TN28HPC+/STD_CELL_ext/track9_200824/TSMCHOME/digital/Documentation/release_note/RN_TCBN28HPCPLUSBWP40P140_190A.pdf`
  + 标准 IO 库 cell 文档 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Documentation/documents/tphn28hpcpgv18_170a/DB_TPHN28HPCPGV18_TT0P9V1P8V25C.pdf`
  + 标准 IO 库 发行说明 `/export/Library/TN28HPC+/IO_ext/tphn28hpcpgv18_170c/TSMCHOME/digital/Documentation/release_note/RN_TPHN28HPCPGV18_170C.pdf`
  + 标准 IO 库 应用手册 `/export/Library/TN28HPC+_Package/IO/IO_application_note/an_n28_general_io_applicationnote_v1d4.pdf`

#### 22nm 工艺库常用目录

+ 工艺库 RTL 模型
  + HVT 标准库 RTL 模型 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/verilog/tcbn22ullbwp7t40p140hvt_110a/tcbn22ullbwp7t40p140hvt.v`
  + LVT 标准库 RTL 模型 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/verilog/tcbn22ullbwp7t40p140lvt_110a/tcbn22ullbwp7t40p140lvt.v`
  + SVT 标准库 RTL 模型 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/verilog/tcbn22ullbwp7t40p140_110a/tcbn22ullbwp7t40p140.v`
  + 标准 IO 库 RTL 模型 `/export4/Library/T22N/IO/TSMCHOME/digital/Front_End/verilog/tphn22ullgv2od3_c171206_110b/tphn22ullgv2od3_c171206.v`
+ 逻辑库文件 `.db`
  + HVT 标准库文件目录 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn22ullbwp7t40p140hvt_110b`
  + LVT 标准库文件目录 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn22ullbwp7t40p140lvt_110b`
  + SVT 标准库文件目录 `/export4/Library/T22N/SC/TSMCHOME/digital/Front_End/timing_power_noise/CCS/tcbn22ullbwp7t40p140_110b`
  + 标准 IO 库文件目录 `/export4/Library/T22N/IO/TSMCHOME/digital/Front_End/timing_power_noise/NLDM/tphn22ullgv2od3_c171206_120a`
+ 物理库文件 MILKYWAY
  + HVT 标准库 MILKYWAY 物理库文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/milkyway/tcbn22ullbwp7t40p140hvt_110a/cell_frame_VHV_0d5_0/tcbn22ullbwp7t40p140hvt`
  + LVT 标准库 MILKYWAY 物理库文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/milkyway/tcbn22ullbwp7t40p140lvt_110a/cell_frame_VHV_0d5_0/tcbn22ullbwp7t40p140lvt`
  + SVT 标准库 MILKYWAY 物理库文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/milkyway/tcbn22ullbwp7t40p140_110a/cell_frame_VHV_0d5_0/tcbn22ullbwp7t40p140`
  + 标准 IO 库 MILKYWAY 物理库文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/milkyway/tphn22ullgv2od3_c171206_120a/mt_2/8m/8M_6X1Z/frame_only/tphn22ullgv2od3_c171206`
  + Bonding PAD 库 MILKYWAY 物理库文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/milkyway/tpbn22v_010a/cup/8m/8M_5X1Z1U/cell_frame/tpbn22v`
+ 库交换格式文件 `.lef`
  + HVT 标准库 `.lef` 库交换格式文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/lef/tcbn22ullbwp7t40p140hvt_110a/lef/tcbn22ullbwp7t40p140hvt.lef`
  + LVT 标准库 `.lef` 库交换格式文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/lef/tcbn22ullbwp7t40p140lvt_110a/lef/tcbn22ullbwp7t40p140lvt.lef`
  + SVT 标准库 `.lef` 库交换格式文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/lef/tcbn22ullbwp7t40p140_110a/lef/tcbn22ullbwp7t40p140.lef`
  + 标准 IO 库 `.lef` 库交换格式文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/lef/tphn22ullgv2od3_c171206_120a/mt_2/8m/8M_6X1Z/lef/tphn22ullgv2od3_c171206_8lm.lef`
  + Bonding PAD 库 `.lef` 库交换格式文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/lef/tpbn22v_010a/cup/8m/8M_5X1Z1U/lef/tpbn22v_8lm.lef`
+ 版图文件 `.gds`
  + HVT 标准库 `.gds` 版图文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/gds/tcbn22ullbwp7t40p140hvt_110a/tcbn22ullbwp7t40p140hvt.gds`
  + LVT 标准库 `.gds` 版图文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/gds/tcbn22ullbwp7t40p140lvt_110a/tcbn22ullbwp7t40p140lvt.gds`
  + SVT 标准库 `.gds` 版图文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/gds/tcbn22ullbwp7t40p140_110a/tcbn22ullbwp7t40p140.gds`
  + 标准 IO 库 `.gds` 版图文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/gds/tphn22ullgv2od3_c171206_120a/mt_2/8m/8M_6X1Z/tphn22ullgv2od3_c171206.gds`
  + Bonding PAD 库 `.gds` 版图文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/gds/tpbn22v_010a/cup/8m/8M_5X1Z1U/tpbn22v.gds`
+ SPICE 仿真电路文件 `.spi`
  + HVT 标准库 SPICE 仿真电路文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/spice/tcbn22ullbwp7t40p140hvt_110a/tcbn22ullbwp7t40p140hvt_110a.spi`
  + LVT 标准库 SPICE 仿真电路文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/spice/tcbn22ullbwp7t40p140lvt_110a/tcbn22ullbwp7t40p140lvt_110a.spi`
  + SVT 标准库 SPICE 仿真电路文件 `/export4/Library/T22N/SC/TSMCHOME/digital/Back_End/spice/tcbn22ullbwp7t40p140_110a/tcbn22ullbwp7t40p140_110a.spi`
  + 标准 IO 库 SPICE 仿真电路文件 `/export4/Library/T22N/IO/TSMCHOME/digital/Back_End/spice/tphn22ullgv2od3_c171206_120a/tphn22ullgv2od3_c171206.spi`
+ virtuoso 相关文件
  + virtuoso 映射文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Virtuoso/tn22clle002_1_5a/TechnologyFile_Virtuoso_22nm_V15a/mapfile/virtuoso_N22_1P8M_5X1Z1U.15a.map`
  + virtuoso 技术文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Virtuoso/tn22clle002_1_5a/TechnologyFile_Virtuoso_22nm_V15a/techfile/virtuoso_N22_1P8M_5X1Z1U.15a.tf`
  + virtuoso 展示格式文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Virtuoso/tn22clle002_1_5a/TechnologyFile_Virtuoso_22nm_V15a/display.drf`
  + virtuoso 建库文件 `/export4/Library/T22N/iPDK_installed/tn22crsp004w1_1_3_1p1a/iPDK_CRN22ULL_shrink_T_v1.3_1p1a_20211230_all/iPDK_CRN22ULL_shrink_20211230/cds.lib`
+ 工艺库相关文件
  + Synopsys 技术文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/PRTF_ICC_22nm_001_Syn/tn22clpr001s1_1_0a_ICC/PRTF_ICC_22nm_001_Syn_V10a/PRTF_ICC_22nm_001_Syn_V10a/PR_tech/Synopsys/TechFile/VHV/PRTF_ICC_22nm_8M_5X1Z1UUTRDL.10a.tf`
  + Cadence 技术文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/PRTF_Innovus_22nm_001_Cad/innovus/tn22clpr001e2_1_1_1a/PRTF_Innovus_22nm_001_Cad_V11_1a/PRTF_Innovus_22nm_001_Cad_V11_1a/PR_tech/Cadence/LefHeader/VHV/PRTF_Innovus_22nm_8M_5X1Z1UUTRDL_9T.11_1a.tlef`
  + GDS 输出层映射文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/PRTF_ICC_22nm_001_Syn/tn22clpr001s1_1_0a_ICC/PRTF_ICC_22nm_001_Syn_V10a/PRTF_ICC_22nm_001_Syn_V10a/PR_tech/Synopsys/GdsOutMap/PRTF_ICC_22nm_8M_5X1Z1U.10a.map`
  + 天线效应规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/PRTF_ICC_22nm_001_Syn/tn22clpr001s1_1_0a_ICC/PRTF_ICC_22nm_001_Syn_V10a/PRTF_ICC_22nm_001_Syn_V10a/PR_tech/Synopsys/SCM/PRTF_ICC_22nm_8M_Antenna.10a.tcl`
  + StarRC 的映射文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/RC_Star-RCXT/tn22clbl007b1_1_0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p08m+ut-alrdl_5x1z1u_typical/Reference/MAP/star.map_icc_cln22ulp_1p8m_5x1z1u_ut-alrdl`
  + StarRC 的映射文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/RC_Star-RCXT/tn22clbl007b1_1_0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p08m+ut-alrdl_5x1z1u_typical/Reference/MAP/star.map_lefdef_cln22ulp_1p8m_5x1z1u_ut-alrdl`
  + StarRC 的映射文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/RC_Star-RCXT/tn22clbl007b1_1_0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p08m+ut-alrdl_5x1z1u_typical/Reference/MAP/star.map_dummy_gds_cln22ulp_1p8m_5x1z1u_ut-alrdl`
  + 互连线工艺 `itf` 文件和 nxtgrd 文件目录 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/RC_Star-RCXT/tn22clbl007b1_1_0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a/RC_Star-RCXT_cln22ulp_1p8m_5x1z1u_ut-alrdl_9corners_shrink_1.0p2a`
  + Dummy TCD 文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DRC_Calibre/tn22cldr001_1_6/N22_DTCD_library_kit_20180409.tar.gz`
  + ICOVL 文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DRC_Calibre/tn22cldr001_1_6/N22_ICOVL_library_kit_20180212.tar.gz`
+ PDK 规则文件
  + LVS 过程 SPICE 网表生成所需文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DFM_LVS_RC_CALIBRE/tn22clls005c1_1_2f/DFM_LVS_RC_CALIBRE_N22_1p13m_ALRDL.v1.2f_all/DFM_LVS_RC_CALIBRE_N22_1p13m_ALRDL.v1.2f.tar.gz/source.added`
  + LVS 规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DFM_LVS_RC_CALIBRE/tn22clls005c1_1_2f/DFM_LVS_RC_CALIBRE_N22_1p13m_ALRDL.v1.2f_all/DFM_LVS_RC_CALIBRE_N22_1p13m_ALRDL.v1.2f.tar.gz/profile/CALIBRE_FLOW/DFM_LVS_RC_CALIBRE_N22_1p8m_ALRDL.v1.2f`
  + DRC 规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Design_Rule/tn22cldr001c1_1_6a/DRC_Calibre_22nm_ULP_1P10M_001_V16a/LOGIC_TopMz+Mu_DRC/CLN22ULP_8M_5X1Z1U_001.16a.encrypt`
  + BOND 规则文件 `/export4/Home/yvxinglong/Work/BackFlow22/4Backend/wirebond/tn22bbdr001c1_1_1a/DRC_Calibre_22nm_WB_V11a/WIRE_BOND_TopMz+Mu_DRC/CN22_WIRE_BOND_8M_5X1Z1U_001.11a`
  + ANT 规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Design_Rule/tn22cldr001c1_1_6a/DRC_Calibre_22nm_ULP_1P10M_001_V16a/ANTENNA_DRC/CLN22ULP_8M_001_ANT.16a`
  + DUMMY 规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Dummy_BEOL_Calibre/tn22cldr001c3_1_5a/Dummy_BEOL_Calibre_22nm_001_V15a/Dummy_BEOL_Calibre_22nm_001.15a`
  + DUMMY 规则文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/Dummy_FEOL_Calibre/tn22cldr001c2_1_3a/Dummy_FEOL_Calibre_22nm_001_V13a/Dummy_FEOL_Calibre_22nm_001.13a`
+ 文档
  + signoff STA 参考文件 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/signoff/N22ULL v1.1 Sign-off Reference.pdf`
  + LVS 规则文档 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DFM_LVS_RC_CALIBRE/tn22clls005c1_1_2f/TN22CLLS005C1_1_2f_wo.pdf`
  + DRC 规则文档 `/export4/Library/T22N/Foundry_Design_Documents/Document-wo/DRC_Calibre/tn22cldr001_1_6/TN22CLDR001_1_6-wo.pdf`
  + HVT 标准库 cell 文档 `/export4/Library/T22N/SC/TSMCHOME/digital/Documentation/documents/tcbn22ullbwp7t40p140hvt_110b/DB_TCBN22ULLBWP7T40P140HVT_TT0P9V25C.pdf`
  + LVT 标准库 cell 文档 `/export4/Library/T22N/SC/TSMCHOME/digital/Documentation/documents/tcbn22ullbwp7t40p140lvt_110b/DB_TCBN22ULLBWP7T40P140LVT_TT0P9V25C.pdf`
  + SVT 标准库 cell 文档 `/export4/Library/T22N/SC/TSMCHOME/digital/Documentation/documents/tcbn22ullbwp7t40p140_110b/DB_TCBN22ULLBWP7T40P140_TT0P9V25C.pdf.pdf`
  + 标准 IO 库 cell 文档 `/export4/Library/T22N/IO/TSMCHOME/digital/Documentation/documents/tphn22ullgv2od3_c171206_120a/DB_TPHN22ULLGV2OD3_C171206_TT0P9V3P3V25C.pdf`
  + 标准 IO 库 发行说明 `/export4/Library/T22N/IO/TSMCHOME/digital/Documentation/release_note/RN_TPHN22ULLGV2OD3_C171206_120A.pdf`
  + 标准 IO 库 应用手册 `/export4/Library/T22N/IO/io_app_note/an_n22_2p5v_general_purpose_io_applicationnote_v1d1_wo.pdf`

#### 脚本调用关系

+ `5PV/run_drc.sh` 修改 `tracy_block_name`
  + `5PV/merge_full_gds/gds_merge.sh` 版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_full_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `5PV/dummy/clear_drc.sh` 清理 calibre 运行时在目录 `5PV/dummy` 产生的文件
  + `5PV/dummy/rundrc.sh` 将 dummy cell 融合入版图以改善金属密度，修改 `RELEASE_BLOCK`
  + `5PV/merge_dummy_gds/gds_merge.sh` 融入 dummy 的版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_dummy_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `5PV/drc/clear_drc.sh` 清理 calibre 运行时在目录 `5PV/drc` 产生的文件
  + `5PV/drc/rundrc.sh` 运行 calibre 检查 DRC 问题，修改 `RELEASE_BLOCK`
    + `5PV/drc/drc.rsf_tmp` 其中定义 calibre drc 规则文件路径

+ `5PV/run_lvs.sh` 修改 `tracy_block_name`
  + `5PV/merge_full_gds/gds_merge.sh` 版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_full_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `5PV/v2lvs/clear.sh` 清理 v2lvs 运行时在目录 `5PV/v2lvs` 产生的文件
  + `5PV/v2lvs/v2lvs.sh` 执行 v2lvs 命令将 verilog 网表转换为 spice 网表，修改 `RELEASE_BLOCK`
    + `v2lvs/spice_include_file/all_include` 在 `set libs` 中增加 .INCLUDE 以添加 sram macro spi 文件
  + `5PV/dummy/clear_drc.sh` 清理 calibre 运行时在目录 `5PV/dummy` 产生的文件
  + `5PV/dummy/rundrc.sh` 将 dummy cell 融合入版图以改善金属密度，修改 `RELEASE_BLOCK`
  + `5PV/merge_dummy_gds/gds_merge.sh` 融入 dummy 的版图融合，修改 `RELEASE_BLOCK`
    + `5PV/merge_dummy_gds/merge.tcl` 在 `set libs` 中增加 sram macro gds 文件
  + `5PV/lvs/clear_lvs.sh` 清理 calibre 运行时在目录 `5PV/lvs` 产生的文件
  + `5PV/lvs/runlvs.sh` 运行 calibre 检查 LVS 问题，修改 `RELEASE_BLOCK`
    + `5PV/lvs/lvs.rsf_tmp` 其中定义 calibre lvs 规则文件路径

#### PV 流程自定义

+ 在 `5PV` 目录下 shell 终端执行 `./run_drc_lvs_sta.sh XXXX`，其中 `XXXX` 为 `$DRC_FIX_CEL_NAME` 命名
+ 报告查看 `5PV/lvs/lvs.rep` 中的笑脸 和 `5PV/drc/DRC.rep` 中的各行 RULECHECK 及末尾 TOTAL DRC Results Generated: 435 (14691)，其中 435 为 DRC 违例数量
  + DRC 违例 [DIODMY_L:WARNING]、[MATCH.WARN.1]、[MOM.R.2]、[ESD.18g]、[ESD.19g] 可以 waive 忽略掉
+ 在 ICC layout 界面点击 Verification -> Read Third-party DRC Error File -> Error file = 4Backend/5PV/drc/DRC_RES.db -> OK
+ 对照 `/export/Library/TN28HPC+_Package/Foundry_Design_Documents/tn28cldr002_1_5/TN28CLDR002_1_5.pdf` 的 DRC 规则文档检查修改错误
  + 常见问题：M1 vddcore/vsscore 伸向电源环金属线 与 M1 伸展出 core 的 vdd/vss 金属线距离过紧，将 M1 伸展出 core 的 vdd/vss 金属线缩回 core 内，而后删除其上原有通孔并执行命令 `derive_pg_connection -power_net VDD -power_pin VDD -ground_net VSS -ground_pin VSS;` 重连电源金属线 解决 电源线 和 NULL Short 问题
  + 常见问题：left endcap 放在应为 right endcap 的位置上，将这一 left endcap 及其最近的 right endcap 右键选中 Unfix Edit，left endcap 右键选中 delete 删除，right endcap 右键选中 copy 到原有 left endcap 位置，最后再将两者 endcap 右键选中 Fix Edit
  + [EFP.VIAx.S.3] 两通孔过近，调整通孔上层的金属宽度等方式
+ 手动修复 DRC 违例比较费时，一些常见错误可以参见 DRC vio fig.pptx 做的总结。手动修完一轮后，记得要及时保存 `save_mw_cel -as $DRC_FIX_CEL_NAME`
+ 而后 exit 退出 icc_shell 并在 shell 终端执行修改 DRC_FIX_CEL_NAME 后的 `icc_shell -x "source ../to_calibre.tcl; quit"`，其中要加上注释 copy_mw_cel 并解开注释 verify_lvs，这样可以避免导出版图为旧版而误做 Calibre DRC

### 6VIRTUOSO

+ 创建 virtuoso 文件夹并将 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/virtuoso` 目录下的 `cds.lib`、`display.drf`、`virtuoso_N28_1P10M_5X2Y2R.20a.map`、`virtuoso_N28_1P10M_5X2Y2R.20a.tf `文件复制到 `virtuoso` 目录下
+ 在 `virtuoso` 目录下 shell 终端执行 virtuoso 打开 EDA 软件工具界面
+ 依次点击 Virtuoso 界面中的 File -> New -> Library... -> Name = XXXX, 勾选 Attach to an existing technology library -> OK -> tsmcN28 -> OK，新建 lib 并 attach 到 tsmcN28 工艺库
+ 依次点击 Virtuoso 界面中的 File -> Import -> Stream... -> Stream File 选择 DGR_DUT_full_with_dummy.gds.gz，library 选择 XXXX -> Number of Threads 设置 16 -> Translate
  + 注意 Generate Technology Information From Stream File 不勾选，其他空处不填
+ 依次点击 Virtuoso 界面中的 Tools -> Library Manager... -> Library 选择 XXXX, Cell 搜索并选择顶层设计名称如 DGR_DUT -> 双击打开 View layout
+ 按下小写 F 键版图居中，按下 shift + F 键 版图图层根据 `display.drf` 设置上色，这一步可能会卡顿
+ 依次点击 Layout 界面中的 Options -> Display... -> 将右侧 Grid Controls 中的 X Snap Spacing 和 Y Snap Spacing 设置为 0.001 -> OK，更改格点精度
+ 依次点击 Layout 界面中的 Edit -> Advanced -> Move Origin -> 将原点设置为 bonding pad 第 10 层金属 凸出的 (-11.66, -11.66) 位置，调整原点坐标以将版图原点移到坐标原点，这一步可能会卡顿，然后保存
+ 依次点击 Virtuoso 界面中的 File -> Export -> Stream... -> Stream File 设置为 DGR_DUT.gds, Library 设置为 XXXX -> Show Options -> Summary File 设置为 summary.log -> Translate，导出版图 .gds 并打印关键信息 strmOut.log 和 summary.log

### 7FM

+ 复制工程 `/export4/Home/yvxinglong/Work/BackFlow28/4Backend/6FM` 至本地目录 `6FM`，注意仅要保留目录下的 `run_fm.csh` 文件、`clear_fm.csh` 文件 和 `6FM/scripts` 子目录，其余子目录均为运行时产生的
+ 这一步的形式化验证对比对象为 逻辑综合后网表 和 布局布线后网表，修改 `run_fm.csh` 中的 block 赋值为顶层设计名称如 DGR_DUT、dc_tag 为 `./dc_go.csh` 产生的运行目录名称，tag 为 tap_icc_release 产生的运行目录名称
+ 在 `6FM` 目录下执行 `./run_fm.csh` 即可，报告查看 `XXXX/logs/formality.log` 末尾的 Verification Results 即可，其中 XXXX 为 tap_icc_release 产生的运行目录名称

## VCS 仿真验证

+ 复制 `/export4/Home/yvxinglong/Work/BackFlow28/5PostSyn/1-behavior/ecpu_2lane_hvt` 目录下的 Makefile、vcs_go.csh、verdi_go.csh、vfile.f 至本地目录 `5PostSyn/1-behavior/XXXX`
+ 修改 vfile.f 中的 `.v` 文件列表，修改 Makefile 中的 OUTPUT 赋值，注意理解 VCS 设置标志，参考 [VCS常用命令详解](https://blog.csdn.net/gsjthxy/article/details/104660557)
  + -timescale=1ns/1ps 定义模块仿真时的时间单位和时间精度，TestBench 定义时钟周期时要采用 parameter real 类型即 `parameter real CYCLE = 5.0;  // 5ns, 200MHz`，以避免 VCS 编译运行 TestBench 时调用整数除法 `always #(CYCLE/2) clock = ~clock`
  + -R 编译完成后立刻运行
  + -full64 支持 64 位模式运行
  + +vc 使用 DirectC 接口时，使能 verilog 直接调用 C/C++ 函数
  + +v2k 支持 Verilog-2001 标准
  + -sverilog 支持 SystemVerilog 语法
  + -debug_access+all 在编译时不用再加 -p novas.tab pli.a 来定 verdi 路径。直接在 simv 的 ucli 脚本里面 call $fsdbdumpfile，需要 dump vcd 的时候直接换 dump file 不需要重新编译
  + -debug_acc+dmptf 支持在 TestBench 中通过 `ifdef DUMP 宏定义实现 dump 波形图
  + -debug_region+cell+encrypt 功能暂且未知，参考脚本历史遗留
  + +define+UNIT_DELAY+TSMC_INITIALIZE_MEM+no_warning 定义 28nm sram verilog 模型中使用的宏变量，其中 UNIT_DELAY 加快仿真速度，TSMC_INITIALIZE_MEM 初始化 sram 存储值，no_warning 屏蔽 sram verilog 运行过程中的警告信息
  + +vcs+initreg+config+var_init.cfg 初始化寄存器的声明文件，`modtree Tb 0 0` 第一个 0 代表层次关系，0 代表当前 level 以及下面的所有 level; 1 代表当前 level；2 代表当前 level 以及下一级 level，第二个 0 代表初始值为 0，可以是 0|1|x|z
  + +define+DUMP 定义 TestBench 中使用的宏变量实现 dump 波形图
  + -o 指定编译生成的可执行文件的名称，默认是 simv
  + -l 指定记录 VCS 编译和运行信息的 log 文件名
  + -f 指定文件列表的文件名，文件中可包括源代码文件的路径和名称，也可以包括编译选项参数
  + -add_seq_delay 0.1 逻辑综合后仿 增加寄存器延迟以避免保持时间违例
  + +lint=TFIPC-L 启用或禁用有关 Verilog 代码的 Lint 消息，如果有的模块的端口定义了但是没有连接，使用这个选项后编译器会给出哪些端口没有连接
  + +no_notifier 屏蔽一些时序检查系统任务中定义的 notifier 寄存器的翻转（toggling），但不影响时序违例的报告。通过这个命令参数可以使时序检查任务中检测到时序违例后，不影响其参数列表中的notifier的值，从而避免了notifier变化引起udp输出不定态的情况，该命令仅对notifier的值有影响，对于时序检查任务检测到的时序违例不产生任何影响
  + +nospecify 屏蔽 specify 块中的路径延时
  + +delay_mode_zero 去除仿真中的延迟信息，所有的延时为 0，当使用了此选项，specify 中的 $setup() 检测失败，似乎把 specparam 设置的变量改为 0 
  + +notimingcheck 屏蔽 specify 块中的时序检查
  + -sdf 作为 elaboration 的选项反标 SDF 文件，-sdf min|typ|max:instance_name:file.sdf
  + +maxdelays 使用 SDF 文件中的 max 值，类似还有标志 +mindelays 和 +typdelays
  + +sdfverbose 打印出全部详细的 SDF 反标信息
  + +neg_tchk 使能时序检查中的负延时
  + -negdelay 允许 sdf 文件中 iopath 和 interconnect 的负延时反标，布局布线后仿需要加上此选项，很多工艺库的 holdtime 为负值
  + -sdfretain 在波形图上体现出 RETAIN 信息，RETAIN TIME 指的是从输入变化后开始计算，输出保持的时间。过了 RETAIN TIME 后，输出会出现一段 X 态，直到最终稳定。网表的 sdf 文件里面会标注路径的 RETAIN 信息
  + -diag=sdf:verbose 诊断 sdf 反标率信息，编译结束后会在当前路径下生成内含反标率的名为 sdfAnnotateInfo 的文件
+ 在 `5PostSyn/1-behavior/XXXX` 目录下执行 `./vcs_go.csh XX` 即可执行仿真，其中 XX 为任意定义的名称
  + 注意生成的波形文件 `.fstb` 在 testbench.v 末尾处有 `ifdef DUMP 的声明
  + VCS 运行的打印输出在 `5PostSyn/1-behavior/XXXX/XX/compile.log` 文件内
+ 在 `5PostSyn/1-behavior/XXXX` 目录下执行 `./verdi_go.csh XX` 即可查看仿真波形图
+ 逻辑综合后仿 和 版图综合后仿 可参考 `5PostSyn/1-behavior`，`5PostSyn/2-netlist` 和 `5PostSyn/3-layout`

## 一些助记

+ 22nm M7 布线 会导致 A.R.4:VIA6 ANT 问题和 LUP DRC 问题
+ 22nm M8 电源带宽度过大、密度过大 会导致 M8.DN.2 DRC 问题
+ ChipWindowUsed 设置错误会导致 CSR DRC 问题和 LUP DRC 问题
+ 22nm FA1Dx standard cell 走线容易导致 EFP.M2.S1 问题
+ 22nm Memory 边沿附近容易出现 M2.S.7/M2.S.8 和 M3.S.7/M3.S.8 问题
+ 修理 DRC 手动调整走线 无心使得走线断开 会导致 PO.R.19 DRC 问题
+ DRC 违例修复小技巧
  + EFP.VIA1.S.3 补填 M2 金属
  + EFP.VIAx.S.3 增加 双孔 间距

+ 存在时钟 mux 结构，在 mux 输出端设置 create_generated_clock，并将 mux 输出端时钟 与 mux 输入端时钟设置为 set_clock_groups -asynchronous
+ 存在两级寄存器解决亚稳态问题的异步桥，sdc 约束要将两个时钟域设置为 set_clock_groups -asynchronous
+ 存在双端口 Memory，sdc 约束要将两个时钟的时序弧双向打断 set_disable_timing

+ icc_shell -x "source ../to_calibre.tcl; quit"
+ icc_shell -x "source ../legalize_and_route.tcl; quit"
+ icc_shell -x "source ../lvs_init.tcl; quit"
+ icc_shell -x "source ../legalize_and_route_filler.tcl; quit"
+ icc_shell -x "source ../check.tcl; quit"

+ 修改 Milkyway 经过 Windows 系统篡改的文件名
bash # 启用 linux bash shell
for file in *_1; do mv "$file" "${file/_1/:1}"; done
for file in *_2; do mv "$file" "${file/_2/:2}"; done
