library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity uart_tx is

  generic (
    g_PRESCALE : integer := 16
    );

  port (
    i_clk    : in  std_logic;
    i_rst    : in  std_logic;
    i_clk_en : in  std_logic;
    o_txd    : out std_logic;
    i_din    : in  std_logic_vector (7 downto 0);
    i_valid  : in  std_logic;
    -- o_ready  : out std_logic;
    o_idle   : out std_logic
    );

end entity uart_tx;

architecture rtl of uart_tx is

  constant NUM_BITS        : integer := 8;
  constant BIT_COUNT_WIDTH : integer := integer(ceil(log2(real(NUM_BITS))));

  signal w_bit_clk      : std_logic;    -- tx clk_en (bit clock)
  signal r_tx_data      : std_logic_vector (NUM_BITS - 1 downto 0);
  signal r_tx_bit_count : unsigned (BIT_COUNT_WIDTH - 1 downto 0);

  type t_state_tx is (
    TX_IDLE,
    TX_SEND_START,
    TX_SEND_BITS,
    TX_SEND_STOP,
    TX_DELAY_RESTART,
    TX_RECOVER
    );

  signal r_state    : t_state_tx;
  signal r_fsm_idle : std_logic;

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

begin  -- architecture tx

  o_idle     <= r_fsm_idle;
  r_fsm_idle <= '1' when r_state = TX_IDLE else '0';
  -- o_ready    <= '1' when r_state = RX_RECEIVED else '0';

  u_rx_baudgen : component uart_baudgen
    generic map (
      g_PRESCALE     => g_PRESCALE,
      g_PHASE_OFFSET => 0)
    port map (
      i_clk    => i_clk,
      i_rst    => i_rst,
      i_clear  => r_fsm_idle,
      i_enable => i_clk_en,
      o_clk_en => w_bit_clk);

  p_tx_count : process (i_clk) is
  begin
    if rising_edge(i_clk) then
      if i_rst = '1' then
        r_tx_bit_count <= (others => '0');
      else
        if w_bit_clk = '1' and r_state = TX_SEND_BITS then
          if r_tx_bit_count = NUM_BITS - 1 then
            r_tx_bit_count <= (others => '0');
          else
            r_tx_bit_count <= r_tx_bit_count + 1;
          end if;
        end if;
      end if;
    end if;
  end process p_tx_count;

  -- control tx state machine
  p_tx_fsm : process (i_clk) is
  begin
    if rising_edge(i_clk) then          -- rising clock edge
      if i_rst = '1' then               -- synchronous reset (active high)
        r_state <= TX_IDLE;
      else
        case r_state is
          when TX_IDLE =>
            -- initiate transmission if high pulse on the valid input
            if i_valid = '1' then
              r_tx_data <= i_din;       -- load shift register
              r_state   <= TX_SEND_START;
            end if;
          when TX_SEND_START =>
            if w_bit_clk = '1' then
              o_txd   <= '0';
              r_state <= TX_SEND_BITS;
            end if;
          when TX_SEND_BITS =>
            if w_bit_clk = '1' then
              o_txd <= r_tx_data(to_integer(r_tx_bit_count));
              if r_tx_bit_count = NUM_BITS - 1 then
                r_state <= TX_SEND_STOP;
              end if;
            end if;
          when TX_SEND_STOP =>
            if w_bit_clk = '1' then
              o_txd   <= '1';
              r_state <= TX_DELAY_RESTART;
            end if;
          when TX_DELAY_RESTART =>
            -- wait one bit period before accepting another transmission
            if w_bit_clk = '1' then
              r_state <= TX_RECOVER;
            end if;
          when TX_RECOVER =>
            -- wait until valid has been deactivated to prevent repeated characters
            if i_valid = '0' then
              r_state <= TX_IDLE;
            end if;
        end case;
      end if;
    end if;
  end process p_tx_fsm;

end architecture rtl;
