-- VIDEO_MIPS module
--
-- Cyclone - MIPS Implementation
-- Uses VGA to Display Data 
-- PBSWITCH_7 toggles the clock for the MIPS
-- SW8 is synchronous reset for MIPS
-- i.e. must clock (hit PBSWITCH_7) while holding down SW8 for reset
-- PC is also displayed on LCD Display
-- DIP Switch 3 is reverse video
--
-- This Module uses UP3core functions and they must be in the user library path
--
-- VHDL synthesis and simulation model of MIPS single clock cycle machine
-- as described in chapter 5 of Patterson and Hennessey
-- VHDL Submodules Ifetch20,Control,Idecode,Execute and Dmemory
-- become different pipeline stages in chapter 6.  The code 
-- for each of these VHDL modules in *.VHD files
--
--
-- Register contain register address on reset.  Data memory
-- is initialized to 55 AA for program on reset
-- Use MIPS module to simulate MIPS without video display hardware
--
--
-- UP3PACK - UP3core package
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.all;
USE  IEEE.STD_LOGIC_ARITH.all;
USE  IEEE.STD_LOGIC_UNSIGNED.all;
LIBRARY lpm;
USE lpm.lpm_components.ALL;
PACKAGE up3core IS

	COMPONENT LCD_Display
	GENERIC(Num_Hex_Digits: Integer:= 11); 		
	PORT(reset, clk_48Mhz: IN	STD_LOGIC;
		 	Hex_Display_Data			: IN    STD_LOGIC_VECTOR((Num_Hex_Digits*4)-1 DOWNTO 0);
		 	LCD_RS, LCD_E				: OUT	STD_LOGIC;
		 	LCD_RW 						: OUT   STD_LOGIC;
		 	DATA_BUS					: INOUT	STD_LOGIC_VECTOR(7 DOWNTO 0));
	END COMPONENT;
	COMPONENT debounce
		PORT(pb, clock_100Hz 	: IN	STD_LOGIC;
	         pb_debounced		: OUT	STD_LOGIC);
	END COMPONENT;
	COMPONENT onepulse
		PORT(pb_debounced, clock: IN	STD_LOGIC;
		 	 pb_single_pulse	: OUT	STD_LOGIC);
	END COMPONENT;
	COMPONENT clk_div
		PORT(clock_48Mhz		: IN	STD_LOGIC;
			clock_1MHz			: OUT	STD_LOGIC;
			clock_100KHz		: OUT	STD_LOGIC;
			clock_10KHz			: OUT	STD_LOGIC;
			clock_1KHz			: OUT	STD_LOGIC;
			clock_100Hz			: OUT	STD_LOGIC;
			clock_10Hz			: OUT	STD_LOGIC;
			clock_1Hz			: OUT	STD_LOGIC);
	END COMPONENT;
	COMPONENT vga_sync
 		PORT(clock_25Mhz, red, green, blue		: IN	STD_LOGIC;
         	red_out, green_out, blue_out		: OUT 	STD_LOGIC;
			horiz_sync_out, vert_sync_out,
			video_blank_out, video_clock_out		: OUT 	STD_LOGIC;
			pixel_row, pixel_column		: OUT STD_LOGIC_VECTOR(9 DOWNTO 0));
	END COMPONENT;
	COMPONENT video_PLL
	PORT(
		inclk0		: IN STD_LOGIC  := '0';
		c0			: OUT STD_LOGIC );
	end component;
	COMPONENT char_rom
		PORT(clock				: IN	STD_LOGIC;	
			character_address	: IN	STD_LOGIC_VECTOR(5 DOWNTO 0);
			font_row, font_col	: IN 	STD_LOGIC_VECTOR(2 DOWNTO 0);
			rom_mux_output		: OUT	STD_LOGIC);
	END COMPONENT;
	COMPONENT keyboard
		PORT(	keyboard_clk, keyboard_data, clock_25Mhz , 
			reset, read				: IN	STD_LOGIC;
			scan_code				: OUT	STD_LOGIC_VECTOR(7 DOWNTO 0);
			scan_ready				: OUT	STD_LOGIC);
	END COMPONENT;
	COMPONENT mouse
		PORT( 	clock_25Mhz, reset 	: IN std_logic;
         		mouse_data			: INOUT std_logic;
        		mouse_clk 			: INOUT std_logic;
        		left_button, right_button : OUT std_logic;
        		mouse_cursor_row, mouse_cursor_column : OUT std_logic_vector(9 DOWNTO 0));       
		END COMPONENT;
END up3core;

LIBRARY IEEE;
USE  IEEE.STD_LOGIC_1164.all;
USE IEEE.STD_LOGIC_ARITH.all;
USE  IEEE.STD_LOGIC_UNSIGNED.all;
LIBRARY lpm;
USE lpm.lpm_components.ALL;
LIBRARY work;
USE work.up3core.all;
USE work.definitions.all;





ENTITY mips_pipe IS


 PORT( PBSWITCH_7, SW8, Clock_48Mhz 				: IN std_logic;
         VGA_Red,VGA_Green,VGA_Blue 		: OUT std_logic;
         VGA_Hsync,VGA_Vsync,
		 Video_blank_out, Video_clock_out 	: OUT std_logic;
         DIPSwitch_1, DIPSwitch_2, 
		 DIPSwitch_3, DIPSwitch_4			: IN std_logic;
		 LCD_RS, LCD_E				: OUT	STD_LOGIC;
		 LCD_RW, LCD_ON						: OUT   STD_LOGIC;
		 DATA_BUS					: INOUT	STD_LOGIC_VECTOR(7 DOWNTO 0));
		
--		
--PC : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
----PC_Plus_4 : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
----Next_PC : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--Instruction_out : OUT STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
----ID
--Read_Data1_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--Read_Data2_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
----EX
--ALU_Input_1_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--ALU_Input_2_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--ALU_Result_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
----Add_Result_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
--Branch_out : OUT STD_LOGIC;
--Branch_NE_out : OUT STD_LOGIC;
--Zero_out : OUT STD_LOGIC;
----MEM
--MemRead_out : OUT STD_LOGIC;
--MemReadData_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--MemWrite_out : OUT STD_LOGIC;
--Mem_Address_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--MemWrite_Data_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
----WB
--RegWrite_out : OUT STD_LOGIC;
--WriteRegister_out : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
--RegWriteData_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
----FORWARDING UNIT LINES
--ForwardA_out : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
--ForwardB_out : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
----HAZARD DETECTION LINES
--STALL_out : OUT STD_LOGIC;
----BRACH HAZARD
--IF_Flush_out : OUT STD_LOGIC;
--IF_ReadData1_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--IF_ReadData2_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--IF_SignExtend_out : OUT STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
--IF_Branch_out : OUT STD_LOGIC;
--IF_BranchNE_out : OUT STD_LOGIC;
--IF_PCPlus4_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--IF_AddResult_out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--IF_Zero_out : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--clock, reset : IN STD_LOGIC);
END mips_pipe;
ARCHITECTURE structure of mips_pipe IS
--Declare all components/units/modules used that
--makeup the MIPS Pipelined processor.
COMPONENT instrfetch
PORT( PC_Out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
Instruction_p : OUT STD_LOGIC_VECTOR(INST_WIDTH-1 DOWNTO 0);
PC_plus_4_p : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
NXT_PC : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
Stall : IN STD_LOGIC;
--BRANCH HAZARD
Read_Data_1 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Sign_Extend : IN STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
Branch : IN STD_LOGIC;
Branch_NE : IN STD_LOGIC;
PC_plus_4 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
IFFlush : IN STD_LOGIC;
IFFlush_p : OUT STD_LOGIC;
IFFlush_pp : IN STD_LOGIC;
IFFlush_ppp : OUT STD_LOGIC;
--OUTPUTS FOR branch hazard DEBUG--
IF_ReadData1 : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
IF_ReadData2 : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
IF_SignExtend : OUT STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
IF_Branch : OUT STD_LOGIC;
IF_BranchNE : OUT STD_LOGIC;
IF_PCPlus4 : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
IF_AddResult : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
IF_Zero : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
clock, reset : IN STD_LOGIC);
END COMPONENT;
COMPONENT operandfetch
PORT ( 
Read_Data_1_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Write_Data : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
RegWrite_ppp : IN STD_LOGIC;
RegWriteOut : OUT STD_LOGIC;
Write_Address_pp : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_ppp : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
Read_Data_p : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
MemtoReg_ppp : IN STD_LOGIC;
ALU_Result_pp : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Instruction_p : IN STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
Sign_Extend_p : OUT STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
Write_Address_0_p : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_1_p : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_2_p : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
RegWriteData : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Instruction_pp : OUT STD_LOGIC_VECTOR(INST_WIDTH-1 DOWNTO 0);
PC_plus_4_p : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
PC_plus_4_pp : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
--HAZARD DETECTION UNIT
IDEX_MemRead : IN STD_LOGIC;
IDEX_Register_Rt : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
IFID_Register_Rs : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
IFID_Register_Rt : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
-------HDU Output lines------
IDEXMemRead_out : OUT STD_LOGIC;
IDEXRegister_Rt_out : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
IFIDRegister_Rs_out : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
IFIDRegister_Rt_out : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
--BRANCH HAZARDS (CONTROL HAZARDS)
Branch_p : IN STD_LOGIC;
Branch_NE_p : IN STD_LOGIC;
Add_Result_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--IF_Flush : OUT STD_LOGIC;
Branch_pp : OUT STD_LOGIC;
Branch_NE_pp : OUT STD_LOGIC;
clock, reset : IN STD_LOGIC);
END COMPONENT;
COMPONENT controlunit IS
PORT( Opcode : IN STD_LOGIC_VECTOR (5 DOWNTO 0);
RegDst_p : OUT STD_LOGIC;
ALU_Op_p : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
ALUSrc_p : OUT STD_LOGIC;
MemWrite_p : OUT STD_LOGIC;
Branch_p : OUT STD_LOGIC;
Branch_NE_p : OUT STD_LOGIC;
MemRead_p : OUT STD_LOGIC;
MemtoReg_p : OUT STD_LOGIC;
RegWrite_p : OUT STD_LOGIC;
IF_Flush : OUT STD_LOGIC;
--HAZARD DETECTION UNIT
IDEX_MemRead : IN STD_LOGIC;
IDEX_Register_Rt : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
IFID_Register_Rs : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
IFID_Register_Rt : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Stall_out : OUT STD_LOGIC;
clock, reset : IN STD_LOGIC);
END COMPONENT;
COMPONENT execution IS
PORT( Read_Data_1 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Sign_Extend_p : IN STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
ALUSrc_p : IN STD_LOGIC;
Zero_p : OUT STD_LOGIC;
ALU_Result_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Funct_field : IN STD_LOGIC_VECTOR (5 DOWNTO 0);
ALU_Op_p : IN STD_LOGIC_VECTOR (1 DOWNTO 0);
PC_plus_4_pp : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
RegDst_p : IN STD_LOGIC;
Write_Address_0_p : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_1_p : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_p : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
MemtoReg_p : IN STD_LOGIC;
MemtoReg_pp : OUT STD_LOGIC;
Read_Data_2_pp : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
MemRead_p : IN STD_LOGIC;
RegWrite_p : IN STD_LOGIC;
MemRead_pp : OUT STD_LOGIC;
RegWrite_pp : OUT STD_LOGIC;
MemWrite_p : IN STD_LOGIC;
MemWrite_pp : OUT STD_LOGIC;
--FORWARDING UNIT SIGNALS
EXMEM_RegWrite : IN STD_LOGIC;
EXMEM_ALU_Result : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
EXMEM_Register_Rd : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
MEMWB_RegWrite : IN STD_LOGIC;
MEMWB_Register_Rd : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
MEMWB_Read_Data : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
IDEX_Register_Rs : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
IDEX_Register_Rt : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
ALU1 : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
ALU2 : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
forwardA : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
forwardB : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
EXMEMRegWrite : OUT STD_LOGIC;
EXMEMALU_Result : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
EXMEMRegister_Rd : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
MEMWBRegWrite : OUT STD_LOGIC;
MEMWBRegister_Rd : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
MEMWBRead_Data : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
IDEXRegister_Rs : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
IDEXRegister_Rt : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
clock, reset : IN STD_LOGIC);
END COMPONENT;
COMPONENT datamemory IS
PORT( Read_Data_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Address : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
Write_Data : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2_ppp : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
MemRead_pp : IN STD_LOGIC;
MemWrite_pp : IN STD_LOGIC;
MemRead_ppp : OUT STD_LOGIC;
MemWrite_ppp : OUT STD_LOGIC;
MemtoReg_pp : IN STD_LOGIC;
RegWrite_pp : IN STD_LOGIC;
MemtoReg_ppp : OUT STD_LOGIC;
RegWrite_ppp : OUT STD_LOGIC;
ALU_Result_p : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
ALU_Result_pp : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Write_Address_p : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_pp : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
Reg_WriteData : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
clock, reset : IN STD_LOGIC);
END COMPONENT;
--Signals used to connect VHDL Components


SIGNAL PC : STD_LOGIC_VECTOR (7 DOWNTO 0);
--PC_Plus_4 : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--Next_PC : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL Instruction_out :  STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
--ID
SIGNAL Read_Data1_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Read_Data2_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--EX
SIGNAL ALU_Input_1_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU_Input_2_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU_Result_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--Add_Result_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL Branch_out :  STD_LOGIC;
SIGNAL Branch_NE_out :  STD_LOGIC;
SIGNAL Zero_out :  STD_LOGIC;
--MEM
SIGNAL MemRead_out :  STD_LOGIC;
SIGNAL MemReadData_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL MemWrite_out :  STD_LOGIC;
SIGNAL Mem_Address_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL MemWrite_Data_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--WB
SIGNAL RegWrite_out :  STD_LOGIC;
SIGNAL WriteRegister_out :  STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL RegWriteData_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
--FORWARDING UNIT LINES
SIGNAL ForwardA_out :  STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL ForwardB_out :  STD_LOGIC_VECTOR (1 DOWNTO 0);
--HAZARD DETECTION LINES
SIGNAL STALL_out :  STD_LOGIC;
--BRACH HAZARD
SIGNAL IF_Flush_out :  STD_LOGIC;
SIGNAL IF_ReadData1_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL IF_ReadData2_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL IF_SignExtend_out :  STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
SIGNAL IF_Branch_out :  STD_LOGIC;
SIGNAL IF_BranchNE_out :  STD_LOGIC;
SIGNAL IF_PCPlus4_out :  STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL IF_AddResult_out :  STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL IF_Zero_out :  STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL clock, reset :  STD_LOGIC;





SIGNAL ALU_Op_p : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL Add_Result_p : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Add_Result_pp : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU_Result_p : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU_Result_pp : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU1 : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALU2 : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL ALUSrc_p : STD_LOGIC;
SIGNAL Branch_p : STD_LOGIC;
SIGNAL Branch_pp : STD_LOGIC;
SIGNAL Branch_NE_p : STD_LOGIC;
SIGNAL Branch_NE_pp : STD_LOGIC;
SIGNAL forwardA : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL forwardB : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL Instruction : STD_LOGIC_VECTOR(INST_WIDTH-1 DOWNTO 0);
SIGNAL Instruction_p : STD_LOGIC_VECTOR(INST_WIDTH-1 DOWNTO 0);
SIGNAL Instruction_pp : STD_LOGIC_VECTOR(INST_WIDTH-1 DOWNTO 0);
SIGNAL MemRead_p : STD_LOGIC;
SIGNAL MemRead_pp : STD_LOGIC;
SIGNAL MemRead_ppp : STD_LOGIC;
SIGNAL MemtoReg_p : STD_LOGIC;
SIGNAL MemtoReg_pp : STD_LOGIC;
SIGNAL MemtoReg_ppp : STD_LOGIC;
SIGNAL MemWrite_p : STD_LOGIC;
SIGNAL MemWrite_pp : STD_LOGIC;
SIGNAL MemWrite_ppp : STD_LOGIC;
SIGNAL PC_Out : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL PC_plus_4_p : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL PC_plus_4_pp : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL NXT_PC : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL Read_Data_1_p : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Read_Data_2_p : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Read_Data_p : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Read_Data_2_pp : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Read_Data_2_ppp : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL RegDst_p : STD_LOGIC;
SIGNAL RegWrite_p : STD_LOGIC;
SIGNAL RegWrite_pp : STD_LOGIC;
SIGNAL RegWrite_ppp : STD_LOGIC;
SIGNAL RegWriteOut : STD_LOGIC;
SIGNAL RegWriteData : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Reg_WriteData : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL Sign_Extend_p : STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
SIGNAL Write_Address_0_p : STD_LOGIC_VECTOR (4 DOWNTO 0); --Rd
SIGNAL Write_Address_1_p : STD_LOGIC_VECTOR (4 DOWNTO 0); --Rt
SIGNAL Write_Address_2_p : STD_LOGIC_VECTOR (4 DOWNTO 0); --Rs
SIGNAL Write_Address_p : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL Write_Address_pp : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL Write_Address_ppp : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL Zero_p : STD_LOGIC;
---FORWARDING UNIT SIGNALS
SIGNAL EXMEMRegWrite : STD_LOGIC;
SIGNAL EXMEMALU_Result : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL EXMEMRegister_Rd : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL MEMWBRegWrite : STD_LOGIC;
SIGNAL MEMWBRegister_Rd : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL MEMWBRead_Data : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL IDEXRegister_Rs : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL IDEXRegister_Rt : STD_LOGIC_VECTOR (4 DOWNTO 0);
--HAZARD DETECTION UNIT SIGNALS
SIGNAL STALLout : STD_LOGIC;
SIGNAL IDEXMemRead_out : STD_LOGIC;
SIGNAL IDEXRegister_Rt_out : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL IFIDRegister_Rs_out : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL IFIDRegister_Rt_out : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL StallInstruction : STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
--BRANCH HAZARD
SIGNAL IF_Flush : STD_LOGIC;
SIGNAL IF_Flush_p : STD_LOGIC;
SIGNAL IF_Flush_pp : STD_LOGIC;
SIGNAL IF_Flush_ppp : STD_LOGIC;
SIGNAL IF_ReadData1 : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL IF_ReadData2 : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL IF_SignExtend : STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
SIGNAL IF_Branch : STD_LOGIC;
SIGNAL IF_BranchNE : STD_LOGIC;
SIGNAL IF_PCPlus4 : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL IF_AddResult : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL IF_Zero : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);


-- Video Display Signals
SIGNAL Red_Data, Green_Data, Blue_Data  : std_logic; 
SIGNAL Power_On, Rev_video,vert_sync_in, clock_25Mhz : std_logic;

-- Signals for Video ROM Memory for Pixel Data
SIGNAL char_address				: std_logic_vector(5 DOWNTO 0);
SIGNAL sum_address				: std_logic_vector(6 DOWNTO 0);
SIGNAL col_address, row_address	: std_logic_vector(5 DOWNTO 0);
SIGNAL pixel_col, pixel_row		: std_logic_vector(9 DOWNTO 0);
SIGNAL rom_mux_output			: std_logic;
SIGNAL format_address			: std_logic_vector(5 DOWNTO 0);
SIGNAL format_data				: std_logic_vector(5 DOWNTO 0);


-- Signals for Push buttons
SIGNAL PBSWITCH_7_sync		: std_logic; 
SIGNAL PBSWITCH_7_debounced, PBSWITCH_7_debounced_Sync	: std_logic; 
SIGNAL PBSWITCH_7_single_pulse			: std_logic;
SIGNAL switch, switch_sync		: std_logic_vector(7 DOWNTO 0);



BEGIN




--Connect the MIPS Components
IFE : instrfetch PORT MAP (
PC_Out => PC_Out,
Instruction_p => Instruction,
PC_plus_4_p => PC_plus_4_p,
NXT_PC => NXT_PC,
Stall => STALLout,
--BRANCH HAZARD
Read_Data_1 => Read_Data_1_p,
Read_Data_2 => Read_Data_2_p,
Sign_Extend => Sign_Extend_p,
Branch => Branch_p,
Branch_NE => Branch_NE_p,
PC_plus_4 => PC_plus_4_p,
IFFlush => IF_Flush,
IFFlush_p => IF_Flush_p,
IFFlush_pp => IF_Flush_p,
IFFlush_ppp => IF_Flush_ppp,
IF_ReadData1 => IF_ReadData1,
IF_ReadData2 => IF_ReadData2,
IF_SignExtend => IF_SignExtend,
IF_Branch => IF_Branch,
IF_BranchNE => IF_BranchNE,
IF_PCPlus4 => IF_PCPlus4,
IF_AddResult => IF_AddResult,
IF_Zero => IF_Zero,
clock => clock,
reset => reset );



ID : operandfetch PORT MAP (
Read_Data_1_p => Read_Data_1_p,
Read_Data_2_p => Read_Data_2_p,
Write_Data => Reg_WriteData,
RegWrite_ppp => RegWrite_ppp,
RegWriteOut => RegWriteOut,
Write_Address_pp => Write_Address_pp,
Write_Address_ppp => Write_Address_ppp,
Read_Data_p => Read_Data_p,
MemtoReg_ppp => MemtoReg_ppp,
ALU_Result_pp => ALU_Result_pp,
Instruction_p => StallInstruction,
Sign_Extend_p => Sign_Extend_p,
Write_Address_0_p => Write_Address_0_p,
Write_Address_1_p => Write_Address_1_p,
Write_Address_2_p => Write_Address_2_p,
RegWriteData => RegWriteData,
Instruction_pp => Instruction_pp,
PC_plus_4_p => PC_plus_4_p,
PC_plus_4_pp => PC_plus_4_pp,
--HAZARD DETECTION UNIT
IDEX_MemRead => MemRead_p,
IDEX_Register_Rt => Write_Address_0_p,
IFID_Register_Rs => Instruction (25 DOWNTO 21), --Rs
IFID_Register_Rt => Instruction (20 DOWNTO 16), --Rt
-------HDU Output lines------
IDEXMemRead_out => IDEXMemRead_out,
IDEXRegister_Rt_out => IDEXRegister_Rt_out,
IFIDRegister_Rs_out => IFIDRegister_Rs_out,
IFIDRegister_Rt_out => IFIDRegister_Rt_out,
--BRANCH HAZARDS
Branch_p => Branch_p,
Branch_NE_p => Branch_NE_p,
Branch_pp => Branch_pp,
Branch_NE_pp => Branch_NE_pp,
clock => clock,
reset => reset );



CTRL : controlunit PORT MAP (
Opcode => Instruction (31 DOWNTO 26),
RegDst_p => RegDst_p,
ALU_Op_p => ALU_Op_p,
ALUSrc_p => ALUSrc_p,
MemWrite_p => MemWrite_p,
Branch_p => Branch_p,
Branch_NE_p => Branch_NE_p,
MemRead_p => MemRead_p,
MemtoReg_p => MemtoReg_p,
RegWrite_p => RegWrite_p,
IF_Flush => IF_Flush,
--HAZARD DETECTION UNIT
IDEX_MemRead => MemRead_p,
IDEX_Register_Rt => Write_Address_0_p,
IFID_Register_Rs => Instruction (25 DOWNTO 21), --Rs
IFID_Register_Rt => Instruction (20 DOWNTO 16), --Rt
Stall_out => STALLout,
clock => clock,
reset => reset);

EX : execution PORT MAP (
Read_Data_1 => Read_Data_1_p,
Read_Data_2 => Read_Data_2_p,
Sign_Extend_p => Sign_Extend_p,
ALUSrc_p => ALUSrc_p,
Zero_p => Zero_p,
ALU_Result_p => ALU_Result_p,
Funct_field => Instruction_pp (5 DOWNTO 0),
ALU_Op_p => ALU_Op_p,
PC_plus_4_pp => PC_plus_4_pp,
RegDst_p => RegDst_p,
Write_Address_0_p => Write_Address_0_p,
Write_Address_1_p => Write_Address_1_p,
Write_Address_p => Write_Address_p,
MemtoReg_p => MemtoReg_p,
MemtoReg_pp => MemtoReg_pp,
Read_Data_2_pp => Read_Data_2_pp,
MemRead_p => MemRead_p,
RegWrite_p => RegWrite_p,
MemRead_pp => MemRead_pp,
RegWrite_pp => RegWrite_pp,
MemWrite_p => MemWrite_p,
MemWrite_pp => MemWrite_pp,
EXMEM_RegWrite => RegWrite_pp,
EXMEM_ALU_Result => ALU_Result_p,
EXMEM_Register_Rd => Write_Address_p,
MEMWB_RegWrite => RegWrite_ppp,
MEMWB_Register_Rd => Write_Address_pp,
MEMWB_Read_Data => Reg_WriteData,
IDEX_Register_Rs => Write_Address_2_p,
IDEX_Register_Rt => Write_Address_0_p,
ALU1 => ALU1,
ALU2 => ALU2,
forwardA => forwardA,
forwardB => forwardB,
EXMEMRegWrite => EXMEMRegWrite,
EXMEMALU_Result => EXMEMALU_Result,
EXMEMRegister_Rd => EXMEMRegister_Rd,
MEMWBRegWrite => MEMWBRegWrite,
MEMWBRegister_Rd => MEMWBRegister_Rd,
MEMWBRead_Data => MEMWBRead_Data,
IDEXRegister_Rs => IDEXRegister_Rs,
IDEXRegister_Rt => IDEXRegister_Rt,
clock => clock,
reset => reset );

MEM : datamemory PORT MAP (
Read_Data_p => Read_Data_p,
Address => ALU_Result_p(7 DOWNTO 0),
Write_Data => Read_Data_2_pp,
Read_Data_2_ppp => Read_Data_2_ppp,
MemRead_pp => MemRead_pp,
MemWrite_pp => MemWrite_pp,
MemRead_ppp => MemRead_ppp,
MemWrite_ppp => MemWrite_ppp,
MemtoReg_pp => MemtoReg_pp,
RegWrite_pp => RegWrite_pp,
MemtoReg_ppp => MemtoReg_ppp,
RegWrite_ppp => RegWrite_ppp,
ALU_Result_p => ALU_Result_p,
ALU_Result_pp => ALU_Result_pp,
Write_Address_p => Write_Address_p,
Write_Address_pp => Write_Address_pp,
Reg_WriteData => Reg_WriteData,
clock => clock,
reset => reset);


--Signals to assign to output pins for SIMULATOR
Instruction_out <= Instruction;
PC <= PC_Out;
Read_Data1_out <= Read_Data_1_p;
Read_Data2_out <= Read_Data_2_p;
ALU_Input_1_out <= ALU1;
ALU_Input_2_out <= ALU2;
ALU_Result_out <= ALU_Result_p;
--Add_Result_out <= Add_Result_p;
Zero_out <= Zero_p;
MemRead_out <= MemRead_ppp;
MemReadData_out <= Read_Data_p WHEN MemRead_ppp = '1' ELSE ZEROS;
MemWrite_out <= MemWrite_ppp;
Mem_Address_out <= ALU_Result_pp WHEN MemRead_ppp = '1' OR MemWrite_ppp = '1' ELSE ZEROS;
MemWrite_Data_out <= Read_Data_2_ppp WHEN MemWrite_ppp = '1' ELSE ZEROS;
RegWrite_out <= '0' WHEN Write_Address_ppp = "00000" ELSE RegWriteOut;
WriteRegister_out <= Write_Address_ppp;
RegWriteData_out <= RegWriteData;
--FORWARDING UNIT LINES
ForwardA_out <= forwardA;
ForwardB_out <= forwardB;
--HAZARD DETECTION LINES
STALL_out <= STALLout;
StallInstruction <= Instruction WHEN STALLout = '0' ELSE Instruction_pp;
--BRANCH HAZARD
IF_Flush_out <= IF_Flush_p;
Branch_out <= Branch_p;
Branch_NE_out <= Branch_NE_p;
IF_ReadData1_out <= IF_ReadData1;
IF_ReadData2_out <= IF_ReadData2;
IF_SignExtend_out <= IF_SignExtend;
IF_Branch_out <= IF_Branch;
IF_BranchNE_out <= IF_BranchNE;
IF_PCPlus4_out <= IF_PCPlus4;
IF_AddResult_out <= IF_AddResult;
IF_Zero_out <= IF_Zero;



-- Character Format ROM for Video Display
-- Displays constant format character data
-- on left side of Display area
 format_rom: lpm_rom
      GENERIC MAP ( lpm_widthad => 6,
        lpm_numwords => 60,
        lpm_outdata => "UNREGISTERED",
        lpm_address_control => "REGISTERED",
			-- Reads in mif file for data display format
        lpm_file => "format.mif",
        lpm_width => 6)
      PORT MAP ( inclock => NOT clock_25Mhz, address => format_address, q => format_data);


---------------------------------------------------------------------------------------
-- MIPS structural model - contains processor module interconnections
-- Code for each module is in *.VHD files
--
	LCD_ON <= '1';
				-- Generate VGA sync signals for display
   SYNC: vga_sync
 		PORT MAP(clock_25Mhz => clock_25Mhz, 
				red => red_data, green => green_data, blue => blue_data,	
    	     	red_out => VGA_red, green_out => VGA_green, blue_out => VGA_blue,
			 	horiz_sync_out => VGA_Hsync, vert_sync_out => vert_sync_in,
				video_blank_out => video_blank_out, video_clock_out => video_clock_out,
			 	pixel_row => pixel_row, pixel_column => pixel_col);
				
-- PLL below is used to generate the 25.175Mhz pixel clock frequency
-- Uses UP 3's 48Mhz USB clock for PLL's input clock
video_PLL_inst : video_PLL PORT MAP (
		inclk0	 => Clock_48Mhz,
		c0	 => clock_25Mhz
	);				

-- Character Font ROM for Video Display
-- 64 by 64 by 1 video rom for pixel background data
--
 CGROM: char_rom
		PORT MAP( clock => clock_25Mhz,
				character_address => char_address,
				font_row => pixel_row(3 DOWNTO 1), font_col => pixel_col(3 DOWNTO 1),	
				rom_mux_output => rom_mux_output);
				-- Display PC in seven-segment displays

-- Debounce Button: Filters out mechanical bounce for around 64Ms.
-- Debounce clock uses Vert_Sync timing signal (16Ms) to save hardware
-- needed for a clock prescaler
	DB1: debounce
		PORT MAP(pb => PBSWITCH_7, clock_100Hz => vert_sync_in , pb_debounced => PBSWITCH_7_debounced);


				-- Output Pushbutton Pulse for 1 clock cycle
	SP1: onepulse
		PORT MAP(pb_debounced => PBSWITCH_7_debounced_sync, clock => clock_25Mhz,
		 	 	 pb_single_pulse => PBSWITCH_7_single_pulse);
	LCD: LCD_Display
		PORT MAP(reset => reset,  clk_48Mhz => clock_25Mhz,		
		 	Hex_Display_Data => "0000" & PC_out & Instruction_out,			
		 	LCD_RS => LCD_RS, LCD_E => LCD_E,
		 	LCD_RW => LCD_RW,
		 	DATA_BUS => DATA_BUS);


------------------------------------------------------------------------------


VGA_Vsync <= vert_sync_in;

			-- Colors for pixel data on video signal
			-- address video_rom for pixel color data
			-- Switch 2 xor Rev_Video will reverse video
Red_Data <= not ((rom_mux_output xor Switch_Sync(2)) xor Rev_video);
Green_Data <= not ((rom_mux_output xor Switch_Sync(2)) xor Rev_video);
Blue_Data <= '1';

			-- current character row and column being displayed
row_address(5 DOWNTO 0) <= pixel_row(9 DOWNTO 4);
col_address(5 DOWNTO 0) <= pixel_col(9 DOWNTO 4);


		-- Combine Flex Dip Switch Inputs into Switch vector
Switch <= "0000"&
          DIPSwitch_4 & DIPSwitch_3 & DIPSwitch_2 & DIPSwitch_1;

		-- Address for Constant Character Data Format ROM
format_address(1 DOWNTO 0) <= Col_address(1 DOWNTO 0);
format_address(5 DOWNTO 2) <= Row_address(4 DOWNTO 1);


-- This Process Provides Character Data for Video Display
-- by generating addresses for the Character Generator ROM
-- using the character row address and col address provided by the Video 
-- Sync process  - 40 characters by 30 lines of display area

VIDEO_DISPLAY_DATA: PROCESS
BEGIN
  WAIT UNTIL (clock_25Mhz'event) AND (clock_25Mhz='1');

			-- Reverse Video for Title at top of screen
IF (row_address <= "00011") THEN rev_video <= '1'; 
	ELSE rev_video <= '0';
END IF;

			-- Blank characters on edge of screen and on alternating lines
IF (row_address(0)='0')  OR
   (col_address < "001000") OR (col_address >"010101") 
THEN char_address <= "100000";
ELSE 

			-- Constant Character Area - use data from format ROM
 IF ((col_address >= "001000") AND (col_address <= "001011")) THEN
	 char_address <= format_data;
 ELSE
			-- Couple of Spaces
  IF (col_address = "001100") OR (col_address = "001101") 
			-- Blanks on Top and Bottom line of Display Area  
  OR (row_address < "00010") OR (row_address > "11011")
  THEN char_address <= "100000";
 
  ELSE

			-- Numeric Data From Simulation
			-- Display Values in Hex
  CASE  row_address(4 DOWNTO 1) IS
	WHEN  "0001"  =>
		CASE col_address IS
		-- Print "Computer" on first line of data display area
 	    	WHEN "001110" => 
				char_address <= "000011"; --C=3
	    	WHEN "001111" => 
				char_address <= "001111" ;--O=15
	    	WHEN "010000" => 
				char_address <= "001101" ;--M=13
	    	WHEN "010001" => 
				char_address <= "010000" ;--P=
	    	WHEN "010010" => 
				char_address <= "010101" ;--U=3
	    	WHEN "010011" => 
				char_address <= "010100" ;--T=3
	    	WHEN "010100" => 
				char_address <= "000101" ;--E=3
	    	WHEN "010101" => 
				char_address <= "010010" ;--R=3
	    	WHEN OTHERS =>
				char_address <= char_address;
        END CASE;
	WHEN  "0010" =>
	    CASE col_address IS
			WHEN "010011" => 
 				-- Selects Hex Character Address with 4-bit value from signal
               char_address <= "11" & "0000";
			WHEN "010100" => 
 				-- Selects Hex Character Address with 4-bit value from signal
               char_address <= "11" & PC_out(7 DOWNTO 4);
        	WHEN "010101" =>
  				-- Selects Hex Character Address with 4-bit value from signal
               char_address <= "11" & PC_out(3 DOWNTO 0);
            WHEN OTHERS =>
               char_address <= "110000";
        END CASE;
	WHEN  "0011"  =>
			-- Selects Hex Character Address with 4-bit value from signal
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "110000" ;
	    	WHEN "001111" => 
				char_address <= "110000" ;
	    	WHEN "010000" => 
				char_address <= "110000" ;
	    	WHEN "010001" => 
				char_address <= "110000" ;
	    	WHEN "010010" => 
				char_address <= "110000" ;
	    	WHEN "010011" => 
				char_address <= "110000" ;
	    	WHEN "010100" => 
				char_address <= "110000" ;
	    	WHEN "010101" => 
				char_address <= "110000";
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;
	WHEN   "0100"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "110000" ;
	    	WHEN "001111" => 
				char_address <= "110000" ;
	    	WHEN "010000" => 
				char_address <= "110000" ;
	    	WHEN "010001" => 
				char_address <= "110000" ;
	    	WHEN "010010" => 
				char_address <= "110000" ;
	    	WHEN "010011" => 
				char_address <= "110000" ;
	    	WHEN "010100" => 
				char_address <= "110000" ;
	    	WHEN "010101" => 
				char_address <= "110000";
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;
	WHEN   "0101"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "110000" ;
	    	WHEN "001111" => 
				char_address <= "110000" ;
	    	WHEN "010000" => 
				char_address <= "110000" ;
	    	WHEN "010001" => 
				char_address <= "110000" ;
	    	WHEN "010010" => 
				char_address <= "110000" ;
	    	WHEN "010011" => 
				char_address <= "110000" ;
	    	WHEN "010100" => 
				char_address <= "110000" ;
	    	WHEN "010101" => 
				char_address <= "110000";
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;
	WHEN   "0110"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "110000" ;
	    	WHEN "001111" => 
				char_address <= "110000" ;
	    	WHEN "010000" => 
				char_address <= "110000" ;
	    	WHEN "010001" => 
				char_address <= "110000" ;
	    	WHEN "010010" => 
				char_address <= "110000" ;
	    	WHEN "010011" => 
				char_address <= "110000" ;
	    	WHEN "010100" => 
				char_address <= "110000" ;
	    	WHEN "010101" => 
				char_address <= "110000";
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;


	WHEN   "0111"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "110000" ;
	    	WHEN "001111" => 
				char_address <= "110000" ;
	    	WHEN "010000" => 
				char_address <= "110000" ;
	    	WHEN "010001" => 
				char_address <= "110000" ;
	    	WHEN "010010" => 
				char_address <= "110000" ;
	    	WHEN "010011" => 
				char_address <= "110000" ;
	    	WHEN "010100" => 
				char_address <= "110000" ;
	    	WHEN "010101" => 
				char_address <= "110000";
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;


	WHEN   "1000"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "110000";
        ELSE
         char_address <= "100000";
        END IF;

	WHEN   "1001"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "110000";
        ELSE
         char_address <= "100000";
        END IF;

	WHEN   "1010"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "110000";
        ELSE
         char_address <= "100000";
        END IF;
	WHEN   "1011"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "110000";
        ELSE
         char_address <= "100000";
        END IF;

--	WHEN   "1001"  =>
--	    IF col_address = "001110" 
--			-- Select "0" OR "1" character address
--         THEN char_address <= "11000" & Zero;
--        ELSE
--         char_address <= "100000";
--        END IF;
--
--
--	WHEN   "1010"  =>
--	    IF col_address = "001110" 
-- 			-- Select "0" OR "1" character address
--        THEN char_address <= "11000" & Memread;
--        ELSE
--         char_address <= "100000";
--        END IF;
--
--
--	WHEN   "1011"  =>
--	    IF col_address = "001110" 
-- 			-- Select "0" OR "1" character address
--        THEN char_address <= "11000" & Memwrite;
--        ELSE
--         char_address <= "100000";
--        END IF;


	WHEN   "1100"  =>
	    IF col_address = "001110" 
 			-- Select Up arrow or Down arrow character address
        THEN char_address <= "0111" & clock & "0";
        ELSE
         char_address <= "100000";
        END IF;


	WHEN   "1101"  =>
	    IF col_address = "001110" 
 			-- Select Up arrow or Down arrow character address
         THEN char_address <= "0111" & reset & "0";
        ELSE
         char_address <= "100000";
        END IF;
	WHEN OTHERS =>
      char_address <= "100000";


  END CASE;
  END IF;
 END IF;
END IF;
END PROCESS VIDEO_DISPLAY_DATA;


	
-- Sync external pushbutton inputs to chip clock
PUSH_BUTTON_SYNC: PROCESS 
BEGIN
  	WAIT UNTIL (clock_25Mhz'event) AND (clock_25mhz='1');
	PBSWITCH_7_Sync <=  PBSWITCH_7;
	reset <=  NOT SW8;
	Switch_Sync <= Switch;
	PBSWITCH_7_DEBOUNCED_SYNC <= PBSWITCH_7_DEBOUNCED;
END PROCESS PUSH_BUTTON_SYNC;

			-- Push Button generates MIPS processor clock
PROCESS
BEGIN
  WAIT UNTIL (clock_25Mhz'event) AND (clock_25Mhz='1');
	IF PBSWITCH_7_Single_Pulse='1' THEN
		clock <= NOT clock;
	END IF;
END PROCESS;


END structure;
