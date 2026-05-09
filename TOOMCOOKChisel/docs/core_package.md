# 独立 `package core` 参数化 core 实验框架

## 设计目的

本目录新增的 `package core` 是一个与既有 `package poly_mult` 隔离的 core 级实验框架，用于扫描不同 core 阶数和不同输入位宽对多项式乘法器 core 的面积、频率、时延和周期数的影响。本次实现不接入 `ToomCook1024` 顶层，也不修改既有 `poly_mult` 主设计逻辑。

## 支持的参数

`CoreConfig.derive` 当前只接受以下 60 种组合：

- core 阶数：`4`、`16`、`64`
- `a` 输入位宽：`24`、`28`、`32`、`36`
- `b` 输入位宽：`8`、`10`、`12`、`14`、`16`

非法参数会通过 `require(...)` 在 elaboration 阶段报错。

## `CoreConfig` 位宽推导规则

`CoreConfig.derive(coreN, aInW, bInW)` 集中推导 core 内部位宽：

- `aEvalW = aInW + 4`
- `bEvalW = bInW + 4`
- `coreOutW = aInW + bInW + log2Ceil(coreN) + 4`

其中 4 个 guard bit 是保守设置：evaluation 点可能包含加法、减法和移位后的系数和，后续 Toom-Cook 插值也可能产生临时增长。当前 schoolbook baseline 的每个输出系数最多累加 `coreN` 个 signed product，因此 `aInW + bInW + log2Ceil(coreN)` 是基础需求，额外 guard bit 用于保持实验框架安全并方便后续替换为 Toom-Cook datapath。

## `CoreModConst` 模逆魔术数

旧实现中的固定十六进制“魔术数”被集中替换为 elaboration-time 的 `BigInt` 计算：

- `CoreModConst.invModPow2Odd(x, width)` 计算奇数 `x` 在模 `2^width` 下的乘法逆元。
- `CoreModConst.inv3(width)` 生成 `3^-1 mod 2^width`。
- `CoreModConst.inv9(width)` 生成 `9^-1 mod 2^width`。

偶数在模 `2^m` 下没有乘法逆元，因此工具函数会拒绝偶数。若后续插值公式需要除以 6 或 18，应保留“先右移除以 2，再乘以 3 或 9 的模逆”的算法结构。

## 当前硬件实现

`ParamCore4`、`ParamCore16` 和 `ParamCore64` 当前都采用参数化 signed schoolbook negacyclic baseline：

```text
c(x) = a(x) * b(x) mod (x^N + 1)
```

输入端口仍为 `UInt`，但每个系数按二进制补码解释为 signed 值参与乘法和累加；输出系数按 `2^coreOutW` 截断后以 `UInt` 输出。baseline 目前是组合式系数计算加一拍输出寄存，`valid_out` 相对 `valid_in` 固定延迟一拍。

该结构优先保证正确性、参数化、可测试和可生成 Verilog。后续可以在不改变 `ParamCore` 对外 IO 的前提下，将 `ParamCore16` 或 `ParamCore64` 内部替换为 Toom-Cook 优化结构。

## 测试 reference

测试使用 `CoreSchoolbookRef.schoolbookNegacyclic` 作为独立参考模型。reference 不复用硬件公式，而是直接双重循环实现 schoolbook negacyclic convolution：当 `i + j >= n` 时根据 `x^n = -1` 将乘积减到 `i + j - n` 位置。reference 与硬件使用同一 signed/unsigned 约定：输入先按对应位宽转换为二进制补码 signed 整数，最终输出再按 `2^outW` 截断。

## 运行测试

从 Chisel 子项目目录运行：

```bash
cd TOOMCOOKChisel
sbt "testOnly core.ParamCoreTester"
```

默认测试覆盖：

- `core4,  a24, b8`
- `core16, a24, b8`
- `core64, a24, b8`
- `core16, a36, b16`
- `core64, a36, b16`

测试 case 包含全 0、one-hot、位移、negacyclic wrap、小随机、完整随机、MSB 符号压力、全最大值和交替 bit pattern。完整 60 组扫描以 ignored long test 形式保留，避免默认测试过慢。

## 生成 60 组 Verilog

从 Chisel 子项目目录运行：

```bash
cd TOOMCOOKChisel
sbt "runMain core.GenerateCoreSweep"
```

生成器会遍历 60 种配置，并将每组输出到独立目录，例如：

```text
generated/core/core4_a24_b8
generated/core/core16_a24_b8
generated/core/core64_a36_b16
```

顶层模块名包含参数，例如：

```text
ParamCore4_A24_B8
ParamCore16_A24_B8
ParamCore64_A36_B16
```

生成日志会打印 `coreN`、`aInW`、`bInW`、`aEvalW`、`bEvalW`、`coreOutW` 和输出目录。
