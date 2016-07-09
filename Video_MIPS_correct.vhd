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
	PORT(reset, clk_48Mhz			: IN	STD_LOGIC;
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

ENTITY Video_MIPS IS

GENERIC(ADDR_WIDTH: integer := 12; DATA_WIDTH: integer := 1);

   PORT( PBSWITCH_7, SW8, Clock_48Mhz 				: IN std_logic;
         VGA_Red,VGA_Green,VGA_Blue 		: OUT std_logic;
         VGA_Hsync,VGA_Vsync,
		 Video_blank_out, Video_clock_out 	: OUT std_logic;
         DIPSwitch_1, DIPSwitch_2, 
		 DIPSwitch_3, DIPSwitch_4			: IN std_logic;
		 LCD_RS, LCD_E				: OUT	STD_LOGIC;
		 LCD_RW, LCD_ON						: OUT   STD_LOGIC;
		 DATA_BUS					: INOUT	STD_LOGIC_VECTOR(7 DOWNTO 0));
		
END Video_MIPS;

ARCHITECTURE behavior OF Video_MIPS IS
	COMPONENT Ifetch
--   	     PORT(	Instruction			: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		PC_plus_4_out 		: OUT  	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
--        		Add_result 			: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
--        		Branch 				: IN 	STD_LOGIC;
--        		Zero 				: IN 	STD_LOGIC;
--        		PC_out 				: OUT 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
--        		clock,reset 		: IN 	STD_LOGIC );

	PORT(	 Instruction 		: OUT	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
        	 PC_plus_4_out 	: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
        	 Add_result 		: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
        	 BranchEq 		: IN 	STD_LOGIC;
                 BranchNeq 		: IN 	STD_LOGIC;
        	 Zero 			: IN 	STD_LOGIC;
                 jumpSignalInIfetch	: IN    STD_LOGIC;	
      		 PC_out 			: OUT	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
        	 clock, reset 	: IN 	STD_LOGIC);
	END COMPONENT; 

	COMPONENT Idecode
-- 	     PORT(	read_data_1 		: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		read_data_2 		: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		Instruction 		: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		read_data 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		ALU_result 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		RegWrite, MemtoReg 	: IN 	STD_LOGIC;
--        		RegDst 				: IN 	STD_LOGIC;
--        		Sign_extend 		: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		clock, reset		: IN 	STD_LOGIC );
		PORT(
				read_data_1	: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				read_data_2	: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				Instruction : IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				read_data 	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				ALU_result	: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				JumpTargetAddOut: OUT 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
				RegWrite 	: IN 	STD_LOGIC;
				MemtoReg 	: IN 	STD_LOGIC;
				RegDst 		: IN 	STD_LOGIC;
				Sign_extend : OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				clock,reset	: IN 	STD_LOGIC );
	END COMPONENT;

	COMPONENT control
--	     PORT( 	Opcode 				: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
--             	RegDst 				: OUT 	STD_LOGIC;
--             	ALUSrc 				: OUT 	STD_LOGIC;
--             	MemtoReg 			: OUT 	STD_LOGIC;
--             	RegWrite 			: OUT 	STD_LOGIC;
--             	MemRead 			: OUT 	STD_LOGIC;
--             	MemWrite 			: OUT 	STD_LOGIC;
--             	Branch 				: OUT 	STD_LOGIC;
--             	ALUop 				: OUT 	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
--             	clock, reset		: IN 	STD_LOGIC );
		PORT( 	
				Opcode 		: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
				RegDst 		: OUT 	STD_LOGIC;
				ALUSrc 		: OUT 	STD_LOGIC;
				MemtoReg 	: OUT 	STD_LOGIC;
				RegWrite 	: OUT 	STD_LOGIC;
				MemRead 	: OUT 	STD_LOGIC;
				MemWrite 	: OUT 	STD_LOGIC;
				BranchEq	: OUT 	STD_LOGIC;
				BranchNeq	: OUT 	STD_LOGIC;
				JumpSignal	: OUT   STD_LOGIC;
				ADDUISignal : OUT   STD_LOGIC;
				ALUop 		: OUT 	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
				clock, reset	: IN 	STD_LOGIC );
	END COMPONENT;

	COMPONENT  Execute
--   	     PORT(	
--					Read_data_1 		: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--                	Read_data_2 		: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--               	Sign_Extend 		: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--               	Function_opcode		: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
--               	ALUOp 				: IN 	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
--               	ALUSrc 				: IN 	STD_LOGIC;
--               	Zero 				: OUT	STD_LOGIC;
--               	ALU_Result 			: OUT	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--               	Add_Result 			: OUT	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
--               	PC_plus_4 			: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
--               	clock, reset		: IN 	STD_LOGIC );

			PORT(	
					Read_data_1 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
					Read_data_2 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
					Sign_extend 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
					Function_opcode 		: IN 	STD_LOGIC_VECTOR( 5 DOWNTO 0 );
					ALUOp 					: IN 	STD_LOGIC_VECTOR( 1 DOWNTO 0 );
					ALUSrc 					: IN 	STD_LOGIC;
					Zero 					: OUT	STD_LOGIC;
					Non_Zero 				: OUT 	STD_LOGIC;
					JumpSignalExecute		: IN    STD_LOGIC;
					JumpTargetAddIn 		: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
					Overflowout		        : OUT   STD_LOGIC;
					ALU_Result 				: OUT	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
					Add_Result 				: OUT	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
					PC_plus_4 				: IN 	STD_LOGIC_VECTOR( 9 DOWNTO 0 );
					clock, reset			: IN 	STD_LOGIC );
	END COMPONENT;


	COMPONENT dmemory
	
--	     PORT(	read_data 			: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		address 			: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
--        		write_data 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
--        		MemRead, Memwrite 	: IN 	STD_LOGIC;
--        		Clock,reset			: IN 	STD_LOGIC );

		PORT(	read_data 			: OUT 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				address 			: IN 	STD_LOGIC_VECTOR( 7 DOWNTO 0 );
				write_data 			: IN 	STD_LOGIC_VECTOR( 31 DOWNTO 0 );
				MemRead, Memwrite 	: IN 	STD_LOGIC;
				clock,reset			: IN 	STD_LOGIC );
	END COMPONENT;

					-- declare signals used to connect VHDL components
				
					
	SIGNAL PC, PC_plus_4 		: STD_LOGIC_VECTOR( 9 DOWNTO 0 );
	SIGNAL read_data_1 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data_2 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Sign_Extend 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL Add_result 		: STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL ALU_result 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL read_data 		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL ALUSrc 			: STD_LOGIC;
	SIGNAL BranchEQ			: STD_LOGIC;
	SIGNAL BranchNEQ		: STD_LOGIC;
	SIGNAL RegDst 			: STD_LOGIC;
	SIGNAL Regwrite 		: STD_LOGIC;
	SIGNAL Zero 			: STD_LOGIC;
	SIGNAL MemWrite 		: STD_LOGIC;
	SIGNAL MemtoReg 		: STD_LOGIC;
	SIGNAL MemRead 			: STD_LOGIC;
	SIGNAL ALUop 			: STD_LOGIC_VECTOR(  1 DOWNTO 0 );
	SIGNAL Instruction		: STD_LOGIC_VECTOR( 31 DOWNTO 0 );
	SIGNAL ControlUnitSignal: STD_LOGIC;
	SIGNAL ComputedAddress  : STD_LOGIC_VECTOR( 7 DOWNTO 0 );
	SIGNAL ADDUISignalTmp	: STD_LOGIC;
	SIGNAL OverflowTmp		: STD_LOGIC;
	SIGNAL reset, clock 			: STD_LOGIC;

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
					-- connect the 5 MIPS components   
  IFE : Ifetch
--	PORT MAP (	Instruction 	=> Instruction,
--    	    	PC_plus_4_out 	=> PC_plus_4,
--				Add_result 		=> Add_result,
--				Branch 			=> Branch,
--				Zero 			=> Zero,
--				PC_out 			=> PC,        		
--				clock 			=> MIPS_clock,  
--				reset 			=> reset );
	PORT MAP (	Instruction 		=> Instruction,
    	    	PC_plus_4_out 			=> PC_plus_4,
				Add_result 				=> Add_result,
				BranchEq 				=> BranchEQ,
				BranchNeq 				=> BranchNEQ,
				Zero 						=> Zero,
				jumpSignalInIfetch	=> ControlUnitSignal,
				PC_out 					=> PC,        		
				clock 					=> clock,  
				reset 					=> reset );

   ID : Idecode
--   	PORT MAP (	read_data_1 	=> read_data_1,
--        		read_data_2 	=> read_data_2,
--        		Instruction 	=> Instruction,
--        		read_data 		=> read_data,
--				ALU_result 		=> ALU_result,
--				RegWrite 		=> RegWrite,
--				MemtoReg 		=> MemtoReg,
--				RegDst 			=> RegDst,
--				Sign_extend 	=> Sign_extend,
--        		clock 			=> MIPS_clock,  
--				reset 			=> reset );
PORT MAP (	read_data_1 	=> read_data_1,
        		read_data_2 	=> read_data_2,
        		Instruction 	=> Instruction,
        		read_data 		=> read_data,
				ALU_result 		=> ALU_result,
				JumpTargetAddOut=>ComputedAddress,
				RegWrite 		=> RegWrite,
				MemtoReg 		=> MemtoReg,
				RegDst 			=> RegDst,
				Sign_extend 	=> Sign_extend,
        		clock 			=> clock,  
				reset 			=> reset );


   CTL:   control
--	PORT MAP ( 	Opcode 			=> Instruction( 31 DOWNTO 26 ),
--				RegDst 			=> RegDst,
--				ALUSrc 			=> ALUSrc,
--				MemtoReg 		=> MemtoReg,
--				RegWrite 		=> RegWrite,
--				MemRead 		=> MemRead,
--				MemWrite 		=> MemWrite,
--				Branch 			=> Branch,
--				ALUop 			=> ALUop,
--                clock 			=> MIPS_clock,
--				reset 			=> reset );
	PORT MAP ( 	Opcode 			=> Instruction( 31 DOWNTO 26 ),
				RegDst 			=> RegDst,
				ALUSrc 			=> ALUSrc,
				MemtoReg 		=> MemtoReg,
				RegWrite 		=> RegWrite,
				MemRead 		=> MemRead,
				MemWrite 		=> MemWrite,
				BranchEq 		=> BranchEQ,
				BranchNeq 		=> BranchNEQ,
				JumpSignal      => ControlUnitSignal,
				ADDUISignal     => ADDUISignalTmp,
				ALUop 			=> ALUop,
                clock 			=> clock,
				reset 			=> reset );

   EXE:  Execute
--   	PORT MAP (	Read_data_1 	=> read_data_1,
--             	Read_data_2 	=> read_data_2,
--				Sign_extend 	=> Sign_extend,
--                Function_opcode	=> Instruction( 5 DOWNTO 0 ),
--				ALUOp 			=> ALUop,
--				ALUSrc 			=> ALUSrc,
--				Zero 			=> Zero,
--                ALU_Result		=> ALU_Result,
--				Add_Result 		=> Add_Result,
--				PC_plus_4		=> PC_plus_4,
--                Clock			=> MIPS_clock,
--				Reset			=> reset );
	PORT MAP (	Read_data_1 	=> read_data_1,
             	Read_data_2 	=> read_data_2,
				Sign_extend 	=> Sign_extend,
                Function_opcode	=> Instruction( 5 DOWNTO 0 ),
				ALUOp 			=> ALUop,
				ALUSrc 			=> ALUSrc,
				Zero 			=> Zero,
				JumpSignalExecute=>ControlUnitSignal,
				JumpTargetAddIn=>ComputedAddress,
				Overflowout		=>OverflowTmp,				
                ALU_Result		=> ALU_Result,
				Add_Result 		=> Add_Result,
				PC_plus_4		=> PC_plus_4,
                Clock			=> clock,
				Reset			=> reset );

   MEM:  dmemory
--	PORT MAP (	read_data 		=> read_data,
--				address 		=> ALU_Result (7 DOWNTO 0),
--				write_data 		=> read_data_2,
--				MemRead 		=> MemRead, 
--				Memwrite 		=> MemWrite, 
--                clock 			=> MIPS_clock,  
--				reset 			=> reset );
	PORT MAP (	read_data 		=> read_data,
				address 		=> ALU_Result (7 DOWNTO 0),
				write_data 		=> read_data_2,
				MemRead 		=> MemRead, 
				Memwrite 		=> MemWrite, 
                clock 			=> clock,  
				reset 			=> reset );





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
		 	Hex_Display_Data => "00" & PC & Instruction,			
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
               char_address <= "11" & "00" & PC(9 DOWNTO 8);
			WHEN "010100" => 
 				-- Selects Hex Character Address with 4-bit value from signal
               char_address <= "11" & PC(7 DOWNTO 4);
        	WHEN "010101" =>
  				-- Selects Hex Character Address with 4-bit value from signal
               char_address <= "11" & PC(3 DOWNTO 0);
            WHEN OTHERS =>
               char_address <= "110000";
        END CASE;
	WHEN  "0011"  =>
			-- Selects Hex Character Address with 4-bit value from signal
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "11" & Instruction(31 DOWNTO 28);
	    	WHEN "001111" => 
				char_address <= "11" & Instruction(27 DOWNTO 24);
	    	WHEN "010000" => 
				char_address <= "11" & Instruction(23 DOWNTO 20);
	    	WHEN "010001" => 
				char_address <= "11" & Instruction(19 DOWNTO 16);
	    	WHEN "010010" => 
				char_address <= "11" & Instruction(15 DOWNTO 12);
	    	WHEN "010011" => 
				char_address <= "11" & Instruction(11 DOWNTO 8);
	    	WHEN "010100" => 
				char_address <= "11" & Instruction(7 DOWNTO 4);
	    	WHEN "010101" => 
				char_address <= "11" & Instruction(3 DOWNTO 0);
	    	WHEN OTHERS =>
				char_address <= char_address;
        END CASE;
	WHEN   "0100"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "11" & Read_data_1(31 DOWNTO 28);
	    	WHEN "001111" => 
				char_address <= "11" & Read_data_1(27 DOWNTO 24);
	    	WHEN "010000" => 
				char_address <= "11" & Read_data_1(23 DOWNTO 20);
	    	WHEN "010001" => 
				char_address <= "11" & Read_data_1(19 DOWNTO 16);
	    	WHEN "010010" => 
				char_address <= "11" & Read_data_1(15 DOWNTO 12);
	    	WHEN "010011" => 
				char_address <= "11" & Read_data_1(11 DOWNTO 8);
	    	WHEN "010100" => 
				char_address <= "11" & Read_data_1(7 DOWNTO 4);
	    	WHEN "010101" => 
				char_address <= "11" & Read_data_1(3 DOWNTO 0);
        	WHEN OTHERS =>
         		char_address <= "110000";
        END CASE;
	WHEN   "0101"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "11" & read_data_2(31 DOWNTO 28);
	    	WHEN "001111" => 
				char_address <= "11" & read_data_2(27 DOWNTO 24);
	    	WHEN "010000" => 
				char_address <= "11" & read_data_2(23 DOWNTO 20);
	    	WHEN "010001" => 
				char_address <= "11" & read_data_2(19 DOWNTO 16);
	    	WHEN "010010" => 
				char_address <= "11" & read_data_2(15 DOWNTO 12);
	    	WHEN "010011" => 
				char_address <= "11" & read_data_2(11 DOWNTO 8);
	    	WHEN "010100" => 
				char_address <= "11" & read_data_2(7 DOWNTO 4);
	    	WHEN "010101" => 
				char_address <= "11" & read_data_2(3 DOWNTO 0);
        	WHEN OTHERS =>
         		char_address <= "110000";
        END CASE;
	WHEN   "0110"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "11" & ALU_result(31 DOWNTO 28);
	    	WHEN "001111" => 
				char_address <= "11" & ALU_result(27 DOWNTO 24);
	    	WHEN "010000" => 
				char_address <= "11" & ALU_result(23 DOWNTO 20);
	    	WHEN "010001" => 
				char_address <= "11" & ALU_result(19 DOWNTO 16);
	    	WHEN "010010" => 
				char_address <= "11" & ALU_result(15 DOWNTO 12);
	    	WHEN "010011" => 
				char_address <= "11" & ALU_result(11 DOWNTO 8);
	    	WHEN "010100" => 
				char_address <= "11" & ALU_result(7 DOWNTO 4);
	    	WHEN "010101" => 
				char_address <= "11" & ALU_result(3 DOWNTO 0);
		     WHEN OTHERS => 
                 char_address <= "110000";
        END CASE;


	WHEN   "0111"  =>
	    CASE col_address IS
			WHEN "001110" => 
         		char_address <= "11" & Read_Data(31 DOWNTO 28);
	    	WHEN "001111" => 
				char_address <= "11" & Read_Data(27 DOWNTO 24);
	    	WHEN "010000" => 
				char_address <= "11" & Read_Data(23 DOWNTO 20);
	    	WHEN "010001" => 
				char_address <= "11" & Read_Data(19 DOWNTO 16);
	    	WHEN "010010" => 
				char_address <= "11" & Read_Data(15 DOWNTO 12);
	    	WHEN "010011" => 
				char_address <= "11" & Read_Data(11 DOWNTO 8);
	    	WHEN "010100" => 
				char_address <= "11" & Read_Data(7 DOWNTO 4);
	    	WHEN "010101" => 
				char_address <= "11" & Read_Data(3 DOWNTO 0);
        	WHEN OTHERS =>
         	  char_address <= "110000";
        END CASE;


	WHEN   "1000"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "11000" & BranchEq;
        ELSE
         char_address <= "100000";
        END IF;

	WHEN   "1001"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "11000" & BranchNeq;
        ELSE
         char_address <= "100000";
        END IF;

	WHEN   "1010"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "11000" & ControlUnitSignal;
        ELSE
         char_address <= "100000";
        END IF;
	WHEN   "1011"  =>
	    IF col_address = "001110" 
			-- Select "0" OR "1" character address
        THEN char_address <= "11000" & ADDUISignalTmp;
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

END behavior;

