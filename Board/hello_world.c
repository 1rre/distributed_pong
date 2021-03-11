#include "system.h"
#include "altera_avalon_pio_regs.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>


int main() {
  printf("Running..\n");
  int fd = open("/dev/jtag_uart",O_RDWR|O_NONBLOCK);
  printf("Opened File\n");
  FILE* fp;
  fp = fdopen(fd, "r+");
  printf("Opened Stream\n");
  alt_u8 prompt;

  alt_u8 button_datain;
  alt_u8 led_on = 0;

  while(1) {
	  prompt = getc(fp);
	  if (prompt == 'a') {
		  IOWR_ALTERA_AVALON_PIO_DATA(LED_BASE, 0xffffffff);
		  prompt = 0;
	  } else if (prompt == 'b') {
		  IOWR_ALTERA_AVALON_PIO_DATA(LED_BASE, 0);
		  prompt = 0;
	  }
	  button_datain = 3 & ~IORD_ALTERA_AVALON_PIO_DATA(BUTTON_BASE);
	  if (button_datain && !led_on) {
		  fprintf(fp,"%c\n",'a');
		  led_on--;
	  } else if (led_on && !button_datain) {
		  fprintf(fp,"%c\n",'b');
		  led_on++;
	  }
  }
  printf("Complete\n");

  return 0;
}

