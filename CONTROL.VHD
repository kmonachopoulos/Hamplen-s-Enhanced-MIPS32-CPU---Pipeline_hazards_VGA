		-- control module (implements MIPS control unit)
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;

ENTITY control IS
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

END control;

ARCHITECTURE behavior OF control IS

	SIGNAL  ADDUISignalTmp,R_format, Lw, Sw, Beq ,Bne	: STD_LOGIC;

BEGIN           
				-- Code to generate control signals using opcode bits
	R_format 	    <=  '1'  WHEN  Opcode = "000000"  ELSE '0';
	Lw              <=  '1'  WHEN  Opcode = "100011"  ELSE '0';
 	Sw              <=  '1'  WHEN  Opcode = "101011"  ELSE '0';
   	Beq             <=  '1'  WHEN  Opcode = "000100"  ELSE '0';
   	Bne             <=  '1'  WHEN  Opcode = "000101"  ELSE '0';
	JumpSignal	    <=  '1'  WHEN  Opcode = "000010"  ELSE '0';
	ADDUISignalTmp	<=  '1'  WHEN  Opcode = "001001"  ELSE '0';
	ADDUISignal     <=  ADDUISignalTmp;
  	RegDst    	    <=  R_format;
 	ALUSrc  		<=  Lw OR Sw OR ADDUISignalTmp;
	MemtoReg 		<=  Lw;
  	RegWrite 		<=  R_format OR Lw OR ADDUISignalTmp;
  	MemRead 		<=  Lw;
   	MemWrite 		<=  Sw; 
 	BranchEq      	<=  Beq;
	BranchNeq       <=  Bne;
	ALUOp( 1 ) 		<=  R_format OR ADDUISignalTmp;
	ALUOp( 0 ) 		<=  Beq OR Bne; 

   END behavior;


