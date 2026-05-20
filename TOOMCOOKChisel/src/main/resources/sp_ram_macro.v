module sp_ram #(
    parameter WIDTH = 4,
    parameter DEPTH = 16
)(
    input                       clk,
    input                       en,
    input                       we,
    input  [$clog2(DEPTH)-1:0]  addr,
    input  [WIDTH-1:0]          din,
    output [WIDTH-1:0]          dout
);

generate
    if (WIDTH == 1536 && DEPTH == 16) begin : GEN_16x1536
        sp_ram_16x1536 u_ram (
            .clk  (clk),
            .en   (en),
            .we   (we),
            .addr (addr),
            .din  (din),
            .dout (dout)
        );
    end
    else if (WIDTH == 512 && DEPTH == 16) begin : GEN_16x512
        sp_ram_16x512 u_ram (
            .clk  (clk),
            .en   (en),
            .we   (we),
            .addr (addr),
            .din  (din),
            .dout (dout)
        );
    end
    else if (WIDTH == 624 && DEPTH == 172) begin : GEN_172x624
        sp_ram_172x624 u_ram (
            .clk  (clk),
            .en   (en),
            .we   (we),
            .addr (addr),
            .din  (din),
            .dout (dout)
        );
    end
    else if (WIDTH == 464 && DEPTH == 172) begin : GEN_172x464
        sp_ram_172x464 u_ram (
            .clk  (clk),
            .en   (en),
            .we   (we),
            .addr (addr),
            .din  (din),
            .dout (dout)
        );
    end
    else if (WIDTH == 576 && DEPTH == 25) begin : GEN_25x576
        sp_ram_25x576 u_ram (
            .clk  (clk),
            .en   (en),
            .we   (we),
            .addr (addr),
            .din  (din),
            .dout (dout)
        );
    end
    else begin : GEN_UNSUPPORTED
        initial begin
            $error("Unsupported sp_ram size: WIDTH=%0d DEPTH=%0d", WIDTH, DEPTH);
        end
    end
endgenerate

endmodule
module sp_ram_16x1536 (
    input          clk,
    input          en,
    input          we,
    input  [3:0]   addr,
    input  [1535:0] din,
    output [1535:0] dout
);

wire [4:0] sram_addr;
assign sram_addr = {1'b0, addr};   // 32-depth SRAM，只使用 0~15 行

genvar i;
generate
    for (i = 0; i < 16; i = i + 1) begin : GEN_SRAM_32x96
        TS1N28HPCPHVTB32X96M4S u_sram (
            .CLK (clk),
            .CEB (~en),
            .WEB (~we),
            .A   (sram_addr),
            .D   (din [i*96 +: 96]),
            .Q   (dout[i*96 +: 96])
        );
    end
endgenerate

endmodule
module sp_ram_16x512 (
    input          clk,
    input          en,
    input          we,
    input  [3:0]   addr,
    input  [511:0] din,
    output [511:0] dout
);

wire [4:0] sram_addr;
assign sram_addr = {1'b0, addr};   // 只使用 0~15 行

wire [575:0] din_full;
wire [575:0] dout_full;

assign din_full = {64'b0, din};
assign dout     = dout_full[511:0];

genvar i;
generate
    for (i = 0; i < 6; i = i + 1) begin : GEN_SRAM_32x96
        TS1N28HPCPHVTB32X96M4S u_sram (
            .CLK (clk),
            .CEB (~en),
            .WEB (~we),
            .A   (sram_addr),
            .D   (din_full [i*96 +: 96]),
            .Q   (dout_full[i*96 +: 96])
        );
    end
endgenerate

endmodule
module sp_ram_172x624 (
    input          clk,
    input          en,
    input          we,
    input  [7:0]   addr,
    input  [623:0] din,
    output [623:0] dout
);

wire [671:0] din_full;
wire [671:0] dout_full;

assign din_full = {48'b0, din};
assign dout     = dout_full[623:0];

genvar i;
generate
    for (i = 0; i < 7; i = i + 1) begin : GEN_SRAM_256x96
        TS1N28HPCPHVTB256X96M4S u_sram (
            .CLK (clk),
            .CEB (~en),
            .WEB (~we),
            .A   (addr),
            .D   (din_full [i*96 +: 96]),
            .Q   (dout_full[i*96 +: 96])
        );
    end
endgenerate

endmodule
module sp_ram_172x464 (
    input          clk,
    input          en,
    input          we,
    input  [7:0]   addr,
    input  [463:0] din,
    output [463:0] dout
);

wire [479:0] din_full;
wire [479:0] dout_full;

assign din_full = {16'b0, din};
assign dout     = dout_full[463:0];

genvar i;
generate
    for (i = 0; i < 5; i = i + 1) begin : GEN_SRAM_256x96
        TS1N28HPCPHVTB256X96M4S u_sram (
            .CLK (clk),
            .CEB (~en),
            .WEB (~we),
            .A   (addr),
            .D   (din_full [i*96 +: 96]),
            .Q   (dout_full[i*96 +: 96])
        );
    end
endgenerate

endmodule
module sp_ram_25x576 (
    input          clk,
    input          en,
    input          we,
    input  [4:0]   addr,
    input  [575:0] din,
    output [575:0] dout
);

genvar i;
generate
    for (i = 0; i < 6; i = i + 1) begin : GEN_SRAM_32x96
        TS1N28HPCPHVTB32X96M4S u_sram (
            .CLK (clk),
            .CEB (~en),
            .WEB (~we),
            .A   (addr),
            .D   (din [i*96 +: 96]),
            .Q   (dout[i*96 +: 96])
        );
    end
endgenerate

endmodule