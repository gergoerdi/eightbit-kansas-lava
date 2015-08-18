library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.lava.all;
use work.all;

entity PET is
  port(CLK_32MHZ : in std_logic;
       RESET : in std_logic;
       PS2A_CLK : in std_logic;
       PS2A_DAT : in std_logic;
       PS2B_CLK : in std_logic;
       PS2B_DAT : in std_logic;
       VGA_VSYNC : out std_logic;
       VGA_HSYNC : out std_logic;
       VGA_R : out std_logic_vector(3 downto 0);
       VGA_G : out std_logic_vector(3 downto 0);
       VGA_B : out std_logic_vector(3 downto 0));
end entity PET;
architecture rtl of PET is
  signal CLK_40MHZ : std_logic;
  signal CLK_1MHZ : std_logic;
  
  signal MAIN_VRAM_WE : std_logic;
  signal MAIN_VRAM_ADDR : std_logic_vector(9 downto 0);
  signal MAIN_VRAM_DOUT : std_logic_vector(7 downto 0);
  signal MAIN_VRAM_DIN : std_logic_vector(7 downto 0);

--  signal CPU_PC : std_logic_vector(15 downto 0);
--  signal CPU_ADDR : std_logic_vector(15 downto 0);
--  signal CPU_READ : std_logic_vector(7 downto 0);
--  signal CPU_WRITE : std_logic_vector (7 downto 0);
--  signal CPU_STATE : std_logic_vector(3 downto 0);
--  signal KEYBOARD : std_logic_vector(9 downto 0);
  
  signal VIDEO_VBLANK : std_logic;
  signal MAIN_VBLANK : std_logic;
  signal VIDEO_VRAM_ADDR : std_logic_vector(9 downto 0);
  signal VIDEO_VRAM_DOUT : std_logic_vector(7 downto 0);
  
begin
  inst_clockman : entity work.clockman
  port map (CLK_IN1 => CLK_32MHZ,
            RESET => RESET,
            CLK_OUT1 => CLK_40MHZ,
            CLK_OUT2 => CLK_1MHZ);
            
  inst_MainBoard : entity work.MainBoard
  port map (CLK_1MHZ => CLK_1MHZ,
            RESET => RESET,
            PS2A_CLK => PS2A_CLK,
            PS2A_DAT => PS2A_DAT,
            PS2B_CLK => PS2B_CLK,
            PS2B_DAT => PS2B_DAT,
            VBLANK => MAIN_VBLANK,
            VRAM_WE => MAIN_VRAM_WE,
            VRAM_ADDR => MAIN_VRAM_ADDR,
            VRAM_DIN => MAIN_VRAM_DIN,
            VRAM_DOUT => MAIN_VRAM_DOUT);
            
  inst_vram : entity work.bram_tdp
  generic map (DATA => 8, ADDR => 10)
  port map (a_clk => CLK_1MHZ,
            a_wr => MAIN_VRAM_WE,
            a_addr => MAIN_VRAM_ADDR,
            a_din => MAIN_VRAM_DIN,
            a_dout => MAIN_VRAM_DOUT,
            
            b_clk => CLK_40MHZ,
            b_wr => '0',
            b_addr => VIDEO_VRAM_ADDR,
            b_din => (others => '0'),
            b_dout => VIDEO_VRAM_DOUT);
            
  inst_video : entity work.Video
  port map (CLK_40MHZ => CLK_40MHZ,
            RESET => RESET,
            VRAM_DOUT => VIDEO_VRAM_DOUT,
            VRAM_ADDR => VIDEO_VRAM_ADDR,
            VBLANK => VIDEO_VBLANK,
            VGA_VSYNC => VGA_VSYNC,
            VGA_HSYNC => VGA_HSYNC,
            VGA_R => VGA_R,
            VGA_G => VGA_G,
            VGA_B => VGA_B);

  VBlankConv : entity work.ToggleSync
  port map (CLK_FAST_SOURCE => CLK_40MHZ,
            CLK_SLOW_TARGET => CLK_1MHZ,
            PULSE_IN => VIDEO_VBLANK,
            PULSE_OUT => MAIN_VBLANK);
end architecture rtl;
