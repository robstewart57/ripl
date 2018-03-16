`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 18.07.2017 10:30:15
// Design Name: 
// Module Name: FPS_Testbench
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module FPS_10;

integer i_count;
integer o_count;


//inputs
reg         CLK;
reg         RST;

reg enable;

//outputs
wire    In_ack1, In_ack2, In_ack3;
wire    In_rdy1, In_rdy2, In_rdy3;
wire    Out_send1, Out_send2, Out_send3;

wire all_rdy, all_ack;

ProgNetwork uut
(
	 //-- XDF Network Input(s)
	 .In1_data (8'h0),
     .In1_send (In_rdy1 & enable),
     .In1_ack (In_ack1),
     .In1_rdy (In_rdy1),
     .In1_count (),
     .In2_data (8'h0),
     .In2_send (In_rdy2 & enable),
     .In2_ack (In_ack2),
     .In2_rdy (In_rdy2),
     .In2_count (),
     .In3_data (8'h0),
     .In3_send (In_rdy3 & enable),
     .In3_ack (In_ack3),
     .In3_rdy (In_rdy3),
     .In3_count (),
	
	 //-- XDF Network Output(s)
    .Out1_data (),
    .Out1_send  (Out_send1),
    .Out1_ack  (Out_send1),
    .Out1_rdy (enable),
    .Out1_count (),
    .Out2_data (),
    .Out2_send  (Out_send2),
    .Out2_ack  (Out_send2),
    .Out2_rdy (enable),
    .Out2_count (),
    .Out3_data (),
    .Out3_send  (Out_send3),
    .Out3_ack  (Out_send3),
    .Out3_rdy (enable),
    .Out3_count (),
	 //-- Clock(s) and Reset
	 .CLK      (CLK),
	 .RESET    (RST)
	 );


initial begin
    
        CLK = 0;
        RST = 1;
        enable = 0;
        
        i_count = 0;
        o_count = 0;

        #500;
        
        @(posedge CLK);
        RST = 0;
        
        #500;
        @(posedge CLK);
        enable=1;


end

assign all_rdy = In_rdy1 & In_rdy2 & In_rdy3;
assign all_ack = In_ack1 & In_ack2 & In_ack3;

always CLK = #1 ~CLK;

always@(posedge CLK)
begin
    if(i_count == 10*10)
    begin
        $display("input frame at %d",$time);
        i_count <= 0;
    end
    else
    begin
        if(all_rdy & all_ack & enable)
             i_count <=  i_count + 1;
    end 
end



endmodule

