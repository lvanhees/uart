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
  constant BIT_PERIOD      : time    := 104 us;
  constant PARITY_BIT      : string  := "even";

  -- component ports
  signal r_clk      : std_logic := '0';
  signal r_rst      : std_logic;
  signal r_txd      : std_logic;
  signal r_rxd      : std_logic;
  -- signal r_tx_ready    : std_logic;
  signal r_tx_idle  : std_logic;
  signal r_rx_dout  : std_logic_vector (7 downto 0);
  signal r_rx_valid : std_logic;
  signal r_rx_error : std_logic;
  signal r_rx_idle  : std_logic;

  procedure uart_write (
    constant UART_BIT_PERIOD :     time;  -- bit period
    uart_din                 : in  std_logic_vector (7 downto 0);
    signal uart_txd          : out std_logic
    ) is
    variable parity : std_logic;
  begin
    -- send start bit
    uart_txd <= '0';
    wait for UART_BIT_PERIOD;
    -- send data bits
    report "Sending byte: " & to_string(uart_din) severity note;
    for i in 0 to (uart_din'length - 1) loop
      uart_txd <= uart_din(i);
      wait for UART_BIT_PERIOD;
    end loop;
    -- send parity bit
    report "Using parity mode: " & PARITY_BIT severity note;
    if PARITY_BIT = "even" then
      parity   := xor uart_din;
      report "Sending parity bit: " & to_string(parity) severity note;
      uart_txd <= parity;
      wait for UART_BIT_PERIOD;
    end if;
    -- send stop bit
    uart_txd <= '1';
    wait for UART_BIT_PERIOD;
  end procedure uart_write;

  procedure uart_read (
    constant UART_BIT_PERIOD :     time;
    uart_rxd                 :     std_logic;
    signal uart_dout         : out std_logic_vector (7 downto 0)
    ) is
  begin
    null;
  end procedure uart_read;

begin  -- architecture behavioral

  dut : entity work.uart
    generic map (
      g_CLK_FREQ   => CLK_FREQ,
      g_BAUD_RATE  => BAUD_RATE,
      g_PARITY_BIT => PARITY_BIT)

    port map (
      i_clk      => r_clk,
      i_rst      => r_rst,
      o_txd      => r_txd,
      i_rxd      => r_rxd,
      i_tx_din   => r_rx_dout,
      i_tx_valid => r_rx_valid,
      -- o_tx_ready => open,
      o_tx_idle  => r_tx_idle,
      o_rx_dout  => r_rx_dout,
      o_rx_valid => r_rx_valid,
      o_rx_error => r_rx_error,
      o_rx_idle  => r_rx_idle);

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
  p_rx_stimulus : process is
  begin
    r_rxd <= '1';
    wait until r_rst = '0';
    wait until rising_edge(r_clk);

    -- send a command to the UART
    wait until rising_edge(r_clk);
    uart_write(BIT_PERIOD, X"9A", r_rxd);
    wait;
  end process p_rx_stimulus;

  p_rx_dout_mon : process is
  begin
    wait until r_rx_idle = '0';
    report "UART Rx Test Started" severity note;

    wait until r_rx_valid = '1' or r_rx_idle = '1';
    assert r_rx_valid = '1' report "Output data valid has not been asserted" severity error;
    assert r_rx_dout = X"9A" report "Incorrect Rx byte received" severity note;

    wait until r_rx_idle = '1';
    report "UART Rx Test Finished" severity note;

    wait;
  end process p_rx_dout_mon;

  p_rx_error_mon : process is
  begin
    wait until r_rx_error = '1';
    assert r_rx_error = '1' report "Receive frame error reported" severity error;
    wait;
  end process p_rx_error_mon;

  -----------------------------------------------------------------------------
  -- UART Tx Test
  -----------------------------------------------------------------------------
  p_txd_mon : process is
  begin
    -- wait until r_rx_valid = '1';
    wait until r_tx_idle = '0';
    report "UART Tx Test Started" severity note;

    wait until r_tx_idle = '1';
    wait until rising_edge(r_clk);

    -- assert false report "UART Test Finished!" severity failure;
    report "UART Tx Test Finished" severity note;
    report "UART Test Finished" severity note;
    std.env.finish;
  end process p_txd_mon;

end architecture behavioral;
-------------------------------------------------------------------------------
