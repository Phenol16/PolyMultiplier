module sp_ram # (
    parameter     WIDTH = 4,
    parameter     DEPTH = 16
    )(
    input           clk,
    input           en,
    input           we,
    input  [$clog2(DEPTH)-1:0]  addr,
    input       [WIDTH-1:0]     din,
    output  reg [WIDTH-1:0]     dout
    );

    reg   [WIDTH-1:0]     ram_sp [DEPTH-1:0];

    always @ (posedge clk) begin
        if(en) begin
            if(we) begin
                ram_sp[addr] <= din;
            end
            else begin
                ram_sp[addr] <= ram_sp[addr]; 
                dout <= ram_sp[addr];
            end
        end
    end

endmodule
