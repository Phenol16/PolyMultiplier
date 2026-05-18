module sram_1536x16 (
  input              clk,
  input              en,
  input              we,
  input      [3:0]   addr,
  input      [1535:0] din,
  output reg [1535:0] dout
);
  reg [1535:0] mem [0:15];
  always @(posedge clk) begin
    if (en) begin
      if (we) mem[addr] <= din;
      else    dout <= mem[addr];
    end
  end
endmodule

module sram_512x16 (
  input             clk,
  input             en,
  input             we,
  input      [3:0]  addr,
  input      [511:0] din,
  output reg [511:0] dout
);
  reg [511:0] mem [0:15];
  always @(posedge clk) begin
    if (en) begin
      if (we) mem[addr] <= din;
      else    dout <= mem[addr];
    end
  end
endmodule

module sram_624x172 (
  input             clk,
  input             en,
  input             we,
  input      [7:0]  addr,
  input      [623:0] din,
  output reg [623:0] dout
);
  reg [623:0] mem [0:171];
  always @(posedge clk) begin
    if (en) begin
      if (we) mem[addr] <= din;
      else    dout <= mem[addr];
    end
  end
endmodule

module sram_464x172 (
  input             clk,
  input             en,
  input             we,
  input      [7:0]  addr,
  input      [463:0] din,
  output reg [463:0] dout
);
  reg [463:0] mem [0:171];
  always @(posedge clk) begin
    if (en) begin
      if (we) mem[addr] <= din;
      else    dout <= mem[addr];
    end
  end
endmodule

module sram_576x25 (
  input             clk,
  input             en,
  input             we,
  input      [4:0]  addr,
  input      [575:0] din,
  output reg [575:0] dout
);
  reg [575:0] mem [0:24];
  always @(posedge clk) begin
    if (en) begin
      if (we) mem[addr] <= din;
      else    dout <= mem[addr];
    end
  end
endmodule
