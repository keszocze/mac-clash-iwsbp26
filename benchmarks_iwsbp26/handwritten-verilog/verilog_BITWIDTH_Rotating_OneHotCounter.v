`timescale 1ns / 1ps
/* Generic top-level module for
 * - operands in rotating shift-registers
 * - one-hot counters
 *
 * Synthesis tool flow does not allow generic top-level modules.  This
 * module is converted to a bit-width specific top-level module by
 * replacing the substring "BITWIDTH" with the desired integer width.
 * This conversion is done by the included script generate.sh.
 *
 * Usage: Cite associated publication: Keszocze et al., Low-level
 * optimizations in high-level HDLs: Is there a benefit?, International
 * Workshop on Boolean Problems, 2026.
 */

module verilog_BITWIDTH_Rotating_OneHotCounter
    (
    input clk,
    input rst,
    input ena,
    input [0:0] start,
    input [BITWIDTH-1:0] x_in,
    input [BITWIDTH-1:0] y_in,
    input [0:0] set_acc,
    input [2*BITWIDTH-1:0] set_acc_value,
    output [0:0] p_valid,
    output [2*BITWIDTH-1:0] p_out,
    output [0:0] a_valid,
    output [2*BITWIDTH-1:0] a_out
    );

    mac_u2_u2_Rotating_OneHotCounter #(.WIDTH(BITWIDTH)) mac
    (
        .clk(clk),
        .rst(rst),
        .ena(ena),
        .start(start),
        .x_in(x_in),
        .y_in(y_in),
        .set_acc(set_acc),
        .set_acc_value(set_acc_value),
        .p_valid(p_valid),
        .p_out(p_out),
        .a_valid(a_valid),
        .a_out(a_out)
    );


endmodule // verilog_BITWIDTH_Rotating_OneHotCounter
