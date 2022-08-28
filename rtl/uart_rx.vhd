library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity uart_rx is

  generic (
    g_PRESCALE     : integer := 16;
    g_PHASE_OFFSET : integer := 8
    );

  port (
    i_clk           : in  std_logic;
    i_rst           : in  std_logic;
    i_clk_en        : in  std_logic;
    i_rxd           : in  std_logic;
    o_dout          : out std_logic_vector (7 downto 0);
    o_dout_valid    : out std_logic;
    o_frame_error   : out std_logic;
    o_overrun_error : out std_logic;
    o_parity_error  : out std_logic;
    o_idle          : out std_logic
    );

end entity uart_rx;

architecture rtl of uart_rx is

  constant NUM_BITS        : integer := 8;
  constant BIT_COUNT_WIDTH : integer := integer(ceil(log2(real(NUM_BITS))));
  -- constant BIT_COUNT_MAX   : unsigned (BIT_COUNT_WIDTH - 1 downto 0) := unsigned(NUM_BITS - 1);

  signal w_bit_clk        : std_logic;  -- rx clk_en (bit clock)
  signal r_rx_data        : std_logic_vector (NUM_BITS - 1 downto 0);
  -- signal r_rx_bit_count   : unsigned (BIT_COUNT_WIDTH - 1 downto 0);
  signal r_rx_parity_bit  : std_logic;
  signal w_bits_remaining : std_logic;

  type t_bit_count is range 0 to NUM_BITS - 1;
  signal r_rx_bit_count : t_bit_count;

  type t_state_rx is (
    RX_IDLE,
    RX_CHECK_START,
    RX_READ_BITS,
    RX_CHECK_STOP,
    RX_ERROR,
    RX_DELAY_RESTART,
    RX_RECEIVED
    );

  signal r_state : t_state_rx;

  signal r_fsm_idle     : std_logic;
  signal r_fsm_stopbit  : std_logic;
  signal r_fsm_databits : std_logic;

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

begin  -- architecture rx

  o_idle <= r_fsm_idle;
  -- o_busy <= not(r_fsm_idle);

  -- UART oversampling (~16x) clock divider and clock enable flag
  u_rx_baudgen : component uart_baudgen
    generic map (
      g_PRESCALE     => g_PRESCALE,
      g_PHASE_OFFSET => g_PHASE_OFFSET)
    port map (
      i_clk    => i_clk,
      i_rst    => i_rst,
      i_clear  => r_fsm_idle,
      i_enable => i_clk_en,
      o_clk_en => w_bit_clk);

  -- receive data shift register
  -- signal r_rx_data : std_logic_vector (NUM_BITS-1 downto 0);
  -- signal r_rx_bit_count : unsigned (BIT_COUNT_WIDTH-1 downto 0);
  p_rx_shift : process (i_clk) is
  begin
    if rising_edge(i_clk) then
      if w_bit_clk = '1' and r_fsm_databits = '1' then
        r_rx_data <= r_rx_data(NUM_BITS - 2 downto 0) & i_rxd;
      end if;
    end if;
  end process p_rx_shift;

  o_dout <= r_rx_data;

  -- with r_rx_bit_count select w_bits_remaining <= '0' when to_unsigned(NUM_BITS - 1, r_rx_bit_count'length), '1' when others;

  p_rx_count : process (i_clk) is
  begin
    if rising_edge(i_clk) then
      if i_rst = '1' then
        r_rx_bit_count <= 0;
      else
        if w_bit_clk = '1' and r_fsm_databits = '1' then
          if r_rx_bit_count = t_bit_count'right then
            r_rx_bit_count <= 0;
          else
            r_rx_bit_count <= r_rx_bit_count + 1;
          end if;
        end if;
      end if;
    end if;
  end process p_rx_count;

-- control rx state machine
  p_rx_fsm : process (i_clk) is
  begin  -- process p_uart_rx
    if rising_edge(i_clk) then          -- rising clock edge
      if i_rst = '1' then               -- synchronous reset (active high)
        r_state <= RX_IDLE;
      else
        r_fsm_idle     <= '0';
        r_fsm_databits <= '0';
        case r_state is
          when RX_IDLE =>
            r_fsm_idle <= '1';
            -- low pulse on the receive line indicates start bit
            if i_rxd = '0' then
              r_state <= RX_CHECK_START;
            end if;
          when RX_CHECK_START =>
            if w_bit_clk = '1' then
              if i_rxd = '0' then
                r_state <= RX_READ_BITS;
              else
                r_state <= RX_ERROR;
              end if;
            end if;
          when RX_READ_BITS =>
            r_fsm_databits <= '1';
            if w_bit_clk = '1' then
              if r_rx_bit_count = t_bit_count'right then
                r_state <= RX_CHECK_STOP;
              end if;
            end if;
          when RX_CHECK_STOP =>
            if w_bit_clk = '1' then
              if i_rxd = '1' then
                r_state <= RX_RECEIVED;
              else
                r_state <= RX_ERROR;
              end if;
            end if;
          when RX_ERROR =>
            -- raise frame error for one clock cycle
            r_state <= RX_DELAY_RESTART;
          when RX_DELAY_RESTART =>
            -- wait one bit periods before accepting another transmission
            -- TODO wait two by adding another state
            if w_bit_clk = '1' then
              r_state <= RX_IDLE;
            end if;
          when RX_RECEIVED =>
            -- raise data valid for one clock cycle
            r_state <= RX_IDLE;
        end case;
      end if;
    end if;
  end process p_rx_fsm;

end architecture rtl;
