library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-------------------------------------------------------------------------------

entity uart_baudgen is

  generic (
    g_PRESCALE     : natural := 16;     -- clock divider factor
    g_PHASE_OFFSET : natural := 7    -- indicate counter value to strobe output
    );

  port (
    i_clk    : in  std_logic;
    i_rst    : in  std_logic;
    i_clear  : in  std_logic;
    i_enable : in  std_logic;
    o_clk_en : out std_logic
    );

end entity uart_baudgen;

-------------------------------------------------------------------------------

architecture rtl of uart_baudgen is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal clk_div_cnt    : integer range 0 to g_PRESCALE - 1;
  signal clk_div_strobe : std_logic;

begin  -- architecture rtl

  p_clk_div_cnt : process (i_clk) is
  begin
    if rising_edge(i_clk) then
      if i_clear = '1' or i_rst = '1' then
        clk_div_cnt <= 0;
      elsif i_enable = '1' then
        if clk_div_cnt = (g_PRESCALE - 1) then
          clk_div_cnt <= 0;
        else
          clk_div_cnt <= clk_div_cnt + 1;
        end if;
      end if;
    end if;
  end process p_clk_div_cnt;

  clk_div_strobe <= '1' when clk_div_cnt = g_PHASE_OFFSET else '0';

  p_clk_enable : process (i_clk) is
  begin
    if rising_edge(i_clk) then
      o_clk_en <= i_enable and clk_div_strobe;
    end if;
  end process p_clk_enable;

end architecture rtl;

-------------------------------------------------------------------------------
