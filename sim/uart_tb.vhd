library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
-------------------------------------------------------------------------------

entity uart_tb is

end entity uart_tb;

-------------------------------------------------------------------------------
architecture behavioral of uart_tb is

  constant CLK_PULSE_WIDTH : time    := 50 ns;
  constant CLK_FREQ        : natural := 10000000;  -- 10 MHz
  constant BAUD_RATE       : natural := 9600;
  -- constant BIT_PERIOD      : time    := integer(ceil(1.0 / real(BAUD_RATE))) * 1 us;
  constant BIT_PERIOD      : time    := 104 us;

  -- component ports
  signal r_clk             : std_logic := '0';
  signal r_rst             : std_logic;
  -- signal o_txd             : std_logic;
  signal r_rxd             : std_logic;
  -- signal i_tx_din          : std_logic_vector (7 downto 0);
  -- signal i_tx_din_valid    : std_logic;
  -- signal o_tx_din_ready    : std_logic;
  -- signal o_tx_idle         : std_logic;
  signal r_rx_dout         : std_logic_vector (7 downto 0);
  signal r_rx_dout_valid   : std_logic;
  signal r_rx_frame_error  : std_logic;
  signal r_rx_parity_error : std_logic;
  signal r_rx_idle         : std_logic;

  procedure uart_write (
    constant UART_BIT_PERIOD :     time;  -- bit period
    uart_din                 : in  std_logic_vector (7 downto 0);
    signal uart_txd          : out std_logic
    ) is
  begin
    -- send start bit
    uart_txd <= '0';
    wait for UART_BIT_PERIOD;
    -- send data bits
    for i in 0 to (uart_din'length - 1) loop
      uart_txd <= uart_din(i);
      wait for UART_BIT_PERIOD;
    end loop;
    -- send stop bit
    uart_txd <= '1';
    wait for UART_BIT_PERIOD;
  end procedure uart_write;

begin  -- architecture behavioral

  dut : entity work.uart
    generic map (
      g_CLK_FREQ  => CLK_FREQ,
      g_BAUD_RATE => BAUD_RATE
      )

    port map (
      i_clk             => r_clk,
      i_rst             => r_rst,
      o_txd             => open,
      i_rxd             => r_rxd,
      i_tx_din          => (others => '0'),
      i_tx_din_valid    => '0',
      o_tx_din_ready    => open,
      o_tx_idle         => open,
      o_rx_dout         => r_rx_dout,
      o_rx_dout_valid   => r_rx_dout_valid,
      o_rx_frame_error  => r_rx_frame_error,
      o_rx_parity_error => r_rx_parity_error,
      o_rx_idle         => r_rx_idle
      );

  -- clock generation
  p_clock_gen : process is
  begin
    r_clk <= '1' after CLK_PULSE_WIDTH, '0' after 2 * CLK_PULSE_WIDTH;
    wait for 2 * CLK_PULSE_WIDTH;
  end process p_clock_gen;

  -- reset generation
  p_reset_gen : process is
  begin
    r_rst <= '1';
    wait until rising_edge(r_clk);
    wait until rising_edge(r_clk);
    r_rst <= '0';
    wait;
  end process p_reset_gen;

  -----------------------------------------------------------------------------
  -- UART Rx Test
  -----------------------------------------------------------------------------
  p_uart_rx : process is
  begin
    r_rxd <= '1';
    wait until r_rst = '0';
    wait until rising_edge(r_clk);

    -- send a command to the UART
    wait until rising_edge(r_clk);
    uart_write(BIT_PERIOD, X"9A", r_rxd);
    wait until rising_edge(r_clk);

    -- check that the correct command was received
    -- if r_rx_dout = X"9A" then
    --   report "Test Passed - Correct Byte Recevied" severity note;
    -- else
    --   report "Test Failed - Incorrect Byte Received" severity note;
    -- end if;
    assert r_rx_dout = X"9A" report "Incorrect Rx byte received" severity note;

    -- assert false report "UART Test Finished!" severity failure;
    report "UART Test Finished" severity note;
    std.env.finish;
  end process p_uart_rx;

end architecture behavioral;
-------------------------------------------------------------------------------
