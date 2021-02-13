	component nios_setup_v2 is
		port (
			clk_clk                           : in  std_logic                    := 'X';             -- clk
			reset_reset_n                     : in  std_logic                    := 'X';             -- reset_n
			button_external_connection_export : in  std_logic_vector(3 downto 0) := (others => 'X'); -- export
			switch_external_connection_export : in  std_logic_vector(9 downto 0) := (others => 'X'); -- export
			led_external_connection_export    : out std_logic_vector(9 downto 0);                    -- export
			hex0_external_connection_export   : out std_logic_vector(6 downto 0);                    -- export
			hex1_external_connection_export   : out std_logic_vector(6 downto 0);                    -- export
			hex2_external_connection_export   : out std_logic_vector(6 downto 0);                    -- export
			hex3_external_connection_export   : out std_logic_vector(6 downto 0);                    -- export
			hex4_external_connection_export   : out std_logic_vector(6 downto 0);                    -- export
			hex5_external_connection_export   : out std_logic_vector(6 downto 0)                     -- export
		);
	end component nios_setup_v2;

	u0 : component nios_setup_v2
		port map (
			clk_clk                           => CONNECTED_TO_clk_clk,                           --                        clk.clk
			reset_reset_n                     => CONNECTED_TO_reset_reset_n,                     --                      reset.reset_n
			button_external_connection_export => CONNECTED_TO_button_external_connection_export, -- button_external_connection.export
			switch_external_connection_export => CONNECTED_TO_switch_external_connection_export, -- switch_external_connection.export
			led_external_connection_export    => CONNECTED_TO_led_external_connection_export,    --    led_external_connection.export
			hex0_external_connection_export   => CONNECTED_TO_hex0_external_connection_export,   --   hex0_external_connection.export
			hex1_external_connection_export   => CONNECTED_TO_hex1_external_connection_export,   --   hex1_external_connection.export
			hex2_external_connection_export   => CONNECTED_TO_hex2_external_connection_export,   --   hex2_external_connection.export
			hex3_external_connection_export   => CONNECTED_TO_hex3_external_connection_export,   --   hex3_external_connection.export
			hex4_external_connection_export   => CONNECTED_TO_hex4_external_connection_export,   --   hex4_external_connection.export
			hex5_external_connection_export   => CONNECTED_TO_hex5_external_connection_export    --   hex5_external_connection.export
		);

