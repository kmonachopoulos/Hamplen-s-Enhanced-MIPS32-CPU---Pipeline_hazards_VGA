LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
library work;
use work.definitions.all;

LIBRARY LPM;
USE LPM.LPM_COMPONENTS.ALL;
ENTITY instrfetch IS
PORT( --Program Counter
PC_Out : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--Instruction Memory
Instruction_p : OUT STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
--PC + 4
PC_plus_4_p : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
NXT_PC : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
--Hazard Detection Unit
Stall : IN STD_LOGIC;
--BRANCH HAZARD
Read_Data_1 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2 : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Sign_Extend : IN STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
Branch : IN STD_LOGIC;
Branch_NE : IN STD_LOGIC;
PC_plus_4 : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
IFFlush : IN STD_LOGIC;
IFFlush_p : OUT STD_LOGIC;
IFFlush_pp : IN STD_LOGIC;
IFFlush_ppp : OUT STD_LOGIC; --Assert if Flush used!
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
END instrfetch;
ARCHITECTURE behavior OF instrfetch IS
SIGNAL PC : STD_LOGIC_VECTOR (9 DOWNTO 0);
SIGNAL PCplus4 : STD_LOGIC_VECTOR (9 DOWNTO 0);
SIGNAL Next_PC : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL Instruction : STD_LOGIC_VECTOR (INST_WIDTH-1 DOWNTO 0);
SIGNAL zero : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
SIGNAL add_result : STD_LOGIC_VECTOR (7 DOWNTO 0);
BEGIN
Instr_Memory: altsyncram
GENERIC MAP(
operation_mode => "ROM",
width_a => 32,
widthad_a => 8,
lpm_type => "altsyncram",
outdata_reg_a => "UNREGISTERED",
init_file => "pipelining_example.mif",
intended_device_family => "Cyclone"	
)
PORT MAP (
clock0     => clock,
--Bits (9 DOWNTO 2) used to word-address instructions
-- e.g. +1 not +4(byte-addressed)
address_a => PC (9 DOWNTO 2),
--Output of Instruction Memory is 32-bit instruction
q_a => Instruction );
-- Copy Output Signals
PC_Out <= PC (7 DOWNTO 0);
NXT_PC <= Next_PC;
-- Adder to increment PC by 4 by shifting bits
PCplus4 (9 DOWNTO 2) <= PC (9 DOWNTO 2) + 1;
PCplus4 (1 DOWNTO 0) <= "00";
--New Branch Compare - compare Read_Data_1 XOR Read_Data_2:
zero <= Read_Data_1 (BUS_WIDTH-1 DOWNTO 0) XOR Read_Data_2 (BUS_WIDTH-1 DOWNTO 0);
add_result <= PC_plus_4 (7 DOWNTO 2) + Sign_Extend (7 DOWNTO 0);
-- Mux to select Branch Address or PC + 4 or Jump (PCSrc Mux)
Next_PC <= add_result WHEN ( IFFlush_pp = '1' AND ( zero = "00000000" )
AND ( Branch = '1') ) OR ( IFFlush_pp = '1'
AND ( zero /= "00000000" ) AND ( Branch_NE = '1') )
ELSE PCplus4 (9 DOWNTO 2);
IF_ReadData1 <= Read_Data_1;
IF_ReadData2 <= Read_Data_2;
IF_SignExtend <= Sign_Extend;
IF_Branch <= Branch;
IF_BranchNE <= Branch_NE;
IF_PCPlus4 <= PC_plus_4;
IF_AddResult <= add_result;
IF_Zero <= zero;
IFFlush_p <= IFFlush;
--Branch Flushing Unit to flush IF/ID Pipeline Register
Instruction_p <= ZEROS32
WHEN (reset = '1')
OR ( IFFlush_pp = '1' AND ( zero = "00000000" )
AND ( Branch = '1') ) OR ( IFFlush_pp = '1'
AND ( zero /= "00000000" ) AND ( Branch_NE = '1') )
ELSE Instruction;
PROCESS
BEGIN
WAIT UNTIL ( clock'EVENT ) AND ( clock = '1' );
IF reset = '1' THEN
PC <= "0000000000";
PC_plus_4_p <= "00000000";
IFFlush_ppp <= '0';
ELSE
IF (Stall = '1') THEN
--AVOID Writing any signals for Load-Use Data Hazard
ELSE
PC (9 DOWNTO 2) <= Next_PC (7 DOWNTO 0);
PC_plus_4_p <= PCplus4 (7 DOWNTO 0);
--Assert if Flush Signal Used!!!
IF ( IFFlush_pp = '1' AND ( zero = "00000000" )
AND ( Branch = '1') ) OR
( IFFlush_pp = '1' AND ( zero /= "00000000" )
AND ( Branch_NE = '1') ) THEN
IFFlush_ppp <= '1';
ELSE
IFFlush_ppp <= '0';
END IF;
END IF;
END IF;
END PROCESS;
END behavior;
