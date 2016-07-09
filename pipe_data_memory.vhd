LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_SIGNED.ALL;
library work;
use work.definitions.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;
ENTITY datamemory IS
PORT(
Read_Data_p : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Address : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
Write_Data : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Read_Data_2_ppp : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
---Pipelined Signals
MemRead_pp : IN STD_LOGIC;
MemRead_ppp : OUT STD_LOGIC;
MemWrite_pp : IN STD_LOGIC;
MemWrite_ppp : OUT STD_LOGIC;
MemtoReg_pp : IN STD_LOGIC;
MemtoReg_ppp : OUT STD_LOGIC;
RegWrite_pp : IN STD_LOGIC;
RegWrite_ppp : OUT STD_LOGIC;
ALU_Result_p : IN STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
ALU_Result_pp : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
Write_Address_p : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
Write_Address_pp : OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
Reg_WriteData : OUT STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
clock, reset : IN STD_LOGIC);
END datamemory;
ARCHITECTURE behavior OF datamemory IS
--Internal Signals
SIGNAL LPM_WRITE : STD_LOGIC;
SIGNAL ReadData : STD_LOGIC_VECTOR (BUS_WIDTH-1 DOWNTO 0);
BEGIN
datamemory : altsyncram
GENERIC MAP(
        operation_mode => "SINGLE_PORT",
		width_a => BUS_WIDTH,
		widthad_a => 8,
		lpm_type => "altsyncram",
		outdata_reg_a => "UNREGISTERED",
		init_file => "data_memory.mif",
		intended_device_family => "Cyclone" )
PORT MAP(
wren_a => MemWrite_pp,
	clock0 => clock,
	address_a => Address,
	data_a => Write_Data,
	q_a => ReadData 
);
LPM_WRITE <= MemWrite_pp AND (NOT clock);
PROCESS
BEGIN
WAIT UNTIL clock'EVENT AND clock = '1';
IF reset = '1' THEN
Read_Data_p <= ZEROS;
MemtoReg_ppp <= '0';
RegWrite_ppp <= '0';
ALU_Result_pp <= ZEROS;
Write_Address_pp <= "00000";
MemRead_ppp <= '0';
MemWrite_ppp <= '0';
Read_Data_2_ppp <= ZEROS;
ELSE
Read_Data_p <= readdata;
MemtoReg_ppp <= MemtoReg_pp;
RegWrite_ppp <= RegWrite_pp;
ALU_Result_pp <= ALU_Result_p;
Write_Address_pp <= Write_Address_p;
MemRead_ppp <= MemRead_pp;
MemWrite_ppp <= MemWrite_pp;
Read_Data_2_ppp <= Write_Data;
IF (MemtoReg_pp = '1') THEN
Reg_WriteData <= ReadData;
ELSE
Reg_WriteData <= ALU_Result_p;
END IF;
END IF;
END PROCESS;
END behavior;