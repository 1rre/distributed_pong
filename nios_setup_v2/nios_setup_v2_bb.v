
module nios_setup_v2 (
	clk_clk,
	reset_reset_n,
	button_external_connection_export,
	switch_external_connection_export,
	led_external_connection_export,
	hex0_external_connection_export,
	hex1_external_connection_export,
	hex2_external_connection_export,
	hex3_external_connection_export,
	hex4_external_connection_export,
	hex5_external_connection_export);	

	input		clk_clk;
	input		reset_reset_n;
	input	[3:0]	button_external_connection_export;
	input	[9:0]	switch_external_connection_export;
	output	[9:0]	led_external_connection_export;
	output	[6:0]	hex0_external_connection_export;
	output	[6:0]	hex1_external_connection_export;
	output	[6:0]	hex2_external_connection_export;
	output	[6:0]	hex3_external_connection_export;
	output	[6:0]	hex4_external_connection_export;
	output	[6:0]	hex5_external_connection_export;
endmodule
