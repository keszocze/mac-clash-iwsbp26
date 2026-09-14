`timescale 1ns / 1ps
/* Parametrized top-level module for
 * - operands in registers with muxes on output
 * - one-hot counters
 *
 * Synthesis tool flow does not allow parametrized top-level modules.
 * For synthesis, this module is instantiated from
 * verilog_BITWIDTH_Indexing_OneHotCounter.v.
 *
 * Usage: Cite associated publication: Keszocze et al., Low-level
 * optimizations in high-level HDLs: Is there a benefit?, International
 * Workshop on Boolean Problems, 2026.
 */

module mac_u2_u2_Indexing_OneHotCounter #(parameter WIDTH = 2)
    (
    input clk,
    input rst,
    input ena,
    input [0:0] start,
    input [WIDTH-1:0] x_in,
    input [WIDTH-1:0] y_in,
    input [0:0] set_acc,
    input [2*WIDTH-1:0] set_acc_value,
    output [0:0] p_valid,
    output [2*WIDTH-1:0] p_out,
    output [0:0] a_valid,
    output [2*WIDTH-1:0] a_out
    );
    
    parameter IDLE = 0,
              ROW = 1,
              CARRY = 2,
              ACCUM = 3;
              
    reg [1:0] cur_state, next_state;
    
    reg [WIDTH-1:0] x, y;
    reg [2*WIDTH-1:0] p, acc;
    
    reg [WIDTH-1:0] x_cnt, y_cnt;
    reg [2*WIDTH-1:0] p_cnt, p_next_row;
    
    reg carry, acc_carry;
    
    wire xi, yi, xy, pi, s, c_in, c_out, acc_s, acc_c_in, acc_c_out, acci, accp;
    
    integer i;
    
    assign xi = |(x & x_cnt);
    assign yi = |(y & y_cnt);
    assign xy = cur_state == CARRY ? 0 : xi & yi;
    assign pi = |(p & p_cnt);
    assign c_in = carry;
    assign {c_out, s} = xy + pi + c_in;
    assign p_out = p;
    //assign p_valid = (cur_state == IDLE && !start) ? 1 : 0;
    //assign a_valid = (cur_state == ACCUM) ? 0 : 1;
    assign acci = |(acc & p_cnt);
    assign accp = |(p & p_cnt);
    assign acc_c_in = acc_carry;
    assign {acc_c_out, acc_s} = acci + accp + acc_c_in;
    assign a_out = (cur_state == ACCUM && p_cnt[2*WIDTH-1]) ? {acc_s, acc[2*WIDTH-2:0]} : acc;
    
    assign p_valid = ((cur_state == IDLE && !start) || (cur_state == ACCUM && p_cnt[2*WIDTH-1])) ? 1 : 0;
    assign a_valid = ((cur_state == ACCUM && !p_cnt[2*WIDTH-1]) || (cur_state == CARRY && y_cnt[WIDTH-1])) ? 0 : 1;

    // Operand registers
    always @(posedge clk)
    begin
        if (rst) begin
            x <= 0;
            y <= 0;
        end else if (start) begin
            x <= x_in;
            y <= y_in;
        end
    end
    
    // Product register
    always @(posedge clk)
    begin
        if (rst) begin
            p <= 0;
        end else if (start) begin
            p <= 0;
        end else if (cur_state == ROW || cur_state == CARRY) begin
            for (i = 0; i < 2*WIDTH; i = i + 1) begin
                if (p_cnt[i]) begin
                    p[i] <= s;
                end
            end
        end
    end
    
    // Accumulator register
    always @(posedge clk)
    begin
        if (rst) begin
            acc <= 0;
        end else if (set_acc) begin
            acc <= set_acc_value;
        end else if (cur_state == ACCUM) begin
            for (i = 0; i < 2*WIDTH; i = i + 1) begin
                if (p_cnt[i]) begin
                    acc[i] <= acc_s;
                end
            end
        end
    end
    
    // Carry register
    always @(posedge clk)
    begin
        if (rst) begin
            carry <= 0;
            acc_carry <= 0;
        end else if (cur_state == ROW) begin
            acc_carry <= 0;
            carry <= c_out;
        end else if (cur_state == CARRY) begin
            acc_carry <= 0;
            carry <= 0;
        end else if (cur_state == ACCUM) begin
            acc_carry <= acc_c_out;
            carry <= 0;
        end
    end
    
    // Operand counters
    always @(posedge clk)
    begin
        if (rst) begin
            x_cnt <= 1;
            y_cnt <= 1;
        end else if (cur_state == ROW) begin
            x_cnt <= {x_cnt[WIDTH-2:0], x_cnt[WIDTH-1]};
        end else if (cur_state == CARRY) begin
            x_cnt <= 1;
            y_cnt <= {y_cnt[WIDTH-2:0], y_cnt[WIDTH-1]};
        end else if (cur_state == ACCUM) begin
            x_cnt <= 1;
            y_cnt <= 1;
        end
    end
    
    // Product counter
    always @(posedge clk)
    begin
        if (rst) begin
            p_cnt <= 1;
            p_next_row <= 2;
        end else if (cur_state == ROW || cur_state == ACCUM) begin
            p_cnt <= {p_cnt[2*WIDTH-2:0], p_cnt[2*WIDTH-1]};
        end else if (cur_state == CARRY) begin
            if (y_cnt[WIDTH-1]) begin
                p_cnt <= 1;
                p_next_row <= 2;
            end else begin
                p_cnt <= p_next_row;
                p_next_row <= {p_next_row[2*WIDTH-2:0], 1'b0};
            end
        end
    end
    
    // Next state logic
    always @(cur_state, start, x_cnt, y_cnt, p_cnt)
    begin
        next_state <= cur_state;
        case (cur_state)
        IDLE: begin
            if (start) next_state <= ROW;
        end
        
        ROW: begin
            if (x_cnt[WIDTH-1]) next_state <= CARRY;
        end
        
        CARRY: begin
            if (!y_cnt[WIDTH-1]) next_state <= ROW;
            else next_state <= ACCUM;
        end
        
        ACCUM: begin
            if (p_cnt[2*WIDTH-1]) next_state <= IDLE;
        end
        
        default: begin
            next_state <= cur_state;
        end
        endcase
    end
    
    // State update
    always @(posedge clk)
    begin
        if (rst)
            cur_state <= IDLE;
        else
            cur_state <= next_state;
    end
    
endmodule
