library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity uart is

  generic (
    g_CLK_FREQ   : integer := 100E6;    -- system clock frequency [Hz]
    g_BAUD_RATE  : integer := 115200;   -- buad rate setting
    g_PARITY_BIT : string  := "none"    -- none, even, odd, mark, space
    );

  port (
    -- clock and reset
    i_clk      : in  std_logic;         -- system clock
    i_rst      : in  std_logic;         -- active high synchronous reset
    -- uart interface
    o_txd      : out std_logic;         -- serial transmit data
    i_rxd      : in  std_logic;         -- serial receive data
    -- user data input interface
    i_tx_din   : in  std_logic_vector (7 downto 0);  -- byte to transmit
    i_tx_valid : in  std_logic;         -- assert to begin transmission
    -- o_tx_ready : out std_logic;  -- indicates transmitter is ready to send data
    o_tx_idle  : out std_logic;         -- low when transmit line is idle
    -- user data output interface
    o_rx_dout  : out std_logic_vector (7 downto 0);  -- byte received
    o_rx_valid : out std_logic;  -- indicates that a byte has been received
    o_rx_error : out std_logic;         -- indicates error in received packet
    o_rx_idle  : out std_logic          -- low when receive line is idle
    );

end entity uart;

architecture str of uart is

  constant OVERSAMPLE      : integer := 16;
  constant PRESCALE        : integer := integer(round(real(g_CLK_FREQ) / real(OVERSAMPLE * g_BAUD_RATE)));
  constant RX_PHASE_OFFSET : integer := integer(floor(real(OVERSAMPLE) / 2.0)) - 1;

  signal w_clk_en       : std_logic;
  signal r_uart_rx_meta : std_logic;
  signal r_uart_rx_sync : std_logic;

  -- counter is held is reset until start bit is detected
  component uart_baudgen is
    generic (
      g_PRESCALE     : integer;         -- Desired baud rate
      g_PHASE_OFFSET : integer          -- Master system clock
      );

    port (
      i_clk    : in  std_logic;         -- system clock
      i_rst    : in  std_logic;         -- active high synchronous reset
      i_clear  : in  std_logic;         -- clock divider counter clear
      i_enable : in  std_logic;         -- clock divider counter enable
      o_clk_en : out std_logic          -- bit/symbol clock (next_bit/bit_clk)
      );
  end component uart_baudgen;

  component uart_rx is
    generic (
      g_PRESCALE     : integer;         -- number of clocks per bit
      g_PHASE_OFFSET : integer;
      g_PARITY_BIT   : string
      );

    port (
      -- clock and reset
      i_clk    : in  std_logic;         -- system clock
      i_rst    : in  std_logic;         -- active high synchronous reset
      -- uart interface
      i_clk_en : in  std_logic;         -- uart clock enable
      i_rxd    : in  std_logic;         -- serial receive data
      -- user data output interface
      o_dout   : out std_logic_vector (7 downto 0);  -- byte received
      o_valid  : out std_logic;  -- indicates that a byte has been received
      o_error  : out std_logic;         -- indicates error in received packet
      o_idle   : out std_logic          -- low when receive line is idle
      );
  end component uart_rx;

  component uart_tx is
    generic (
      g_PRESCALE   : integer;           -- number of clocks per bit
      g_PARITY_BIT : string
      );

    port (
      -- clock and reset
      i_clk    : in  std_logic;         -- system clock
      i_rst    : in  std_logic;         -- active high synchronous reset
      -- uart interface
      i_clk_en : in  std_logic;         -- uart clock enable
      o_txd    : out std_logic;         -- serial transmit data
      -- user data input interface
      i_din    : in  std_logic_vector (7 downto 0);  -- byte to transmit
      i_valid  : in  std_logic;         -- assert to begin transmission
      -- o_ready  : out std_logic;  -- indicates transmitter is ready to send data
      o_idle   : out std_logic          -- low when transmit line is idle
      );
  end component uart_tx;

begin  -- architecture str

  -- UART oversampling (~16x) clock divider and clock enable flag
  u_uart_baudgen : component uart_baudgen
    generic map (
      g_PRESCALE     => PRESCALE,
      g_PHASE_OFFSET => 0)
    port map (
      i_clk    => i_clk,
      i_rst    => i_rst,
      i_clear  => i_rst,
      i_enable => '1',
      o_clk_en => w_clk_en);

  -- UART RX cross from slow clock to fast clock domain to avoid metastability
  p_uart_rx_cdc : process (i_clk) is
  begin  -- process p_uart_rx_cdc
    if rising_edge(i_clk) then
      r_uart_rx_meta <= i_rxd;
      r_uart_rx_sync <= r_uart_rx_meta;
    end if;
  end process p_uart_rx_cdc;

  -- UART Receiver
  u_uart_rx : component uart_rx
    generic map (
      g_PRESCALE     => OVERSAMPLE,
      g_PHASE_OFFSET => RX_PHASE_OFFSET,
      g_PARITY_BIT   => g_PARITY_BIT)
    port map (
      i_clk    => i_clk,
      i_rst    => i_rst,
      i_clk_en => w_clk_en,
      i_rxd    => r_uart_rx_sync,
      o_dout   => o_rx_dout,
      o_valid  => o_rx_valid,
      o_error  => o_rx_error,
      o_idle   => o_rx_idle);

  -- UART Transmitter
  u_uart_tx : component uart_tx
    generic map (
      g_PRESCALE   => OVERSAMPLE,
      g_PARITY_BIT => g_PARITY_BIT)
    port map (
      i_clk    => i_clk,
      i_rst    => i_rst,
      i_clk_en => w_clk_en,
      o_txd    => o_txd,
      i_din    => i_tx_din,
      i_valid  => i_tx_valid,
      -- o_ready  => o_tx_ready,
      o_idle   => o_tx_idle);

end architecture str;
