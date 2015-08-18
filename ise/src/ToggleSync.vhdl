-- Based on http://www.edn.com/electronics-blogs/day-in-the-life-of-a-chip-designer/4435339/Synchronizer-techniques-for-multi-clock-domain-SoCs
-- via http://electronics.stackexchange.com/questions/182331/

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ToggleSync is
    Port ( CLK_FAST_SOURCE : in  STD_LOGIC;
           CLK_SLOW_TARGET : in  STD_LOGIC;
           PULSE_IN : in  STD_LOGIC;
           PULSE_OUT : out  STD_LOGIC);
end ToggleSync;

architecture Behavioral of ToggleSync is
  signal fast_buffer : std_logic := '0';
  signal slow_buffer1 : std_logic := '0';
  signal slow_buffer2 : std_logic := '0';
  signal slow_buffer3 : std_logic := '0';
begin
  process (CLK_FAST_SOURCE)
  begin
    if rising_edge(CLK_FAST_SOURCE) then
      if PULSE_IN = '0' then
        fast_buffer <= fast_buffer;
      else
        fast_buffer <= not fast_buffer;
      end if;
    end if;
  end process;

  process (CLK_SLOW_TARGET)
  begin
    if rising_edge(CLK_SLOW_TARGET) then
      slow_buffer1 <= fast_buffer;
    end if;
  end process;

  process (CLK_SLOW_TARGET)
  begin
    if rising_edge(CLK_SLOW_TARGET) then
      slow_buffer2 <= slow_buffer1;
    end if;
  end process;

  process (CLK_SLOW_TARGET)
  begin
    if rising_edge(CLK_SLOW_TARGET) then
      slow_buffer3 <= slow_buffer2;
    end if;
  end process;

  PULSE_OUT <= slow_buffer2 xor slow_buffer3;

end Behavioral;
