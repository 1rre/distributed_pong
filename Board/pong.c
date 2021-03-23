#include "system.h"
#include "altera_avalon_pio_regs.h"
#include "altera_up_avalon_accelerometer_spi.h"
#include "altera_avalon_timer_regs.h"
#include "altera_avalon_timer.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
// This changes the speed of the thing
#define SLEEP_TIME 40000
FILE* fp;
alt_32 x_read;
alt_u16 x_val = 127<<8;
alt_up_accelerometer_spi_dev *  acc_dev;
alt_u8 send_counter = 0;
alt_u8 uintto7seg[10] = {0b00000010,0b11110011,0b00100101,0b00001101,0b10011001,0b01001001,0b01000010,0b00011111,0b000000001,0b00001001};




void update_hex0(alt_8 number){
IOWR_ALTERA_AVALON_PIO_DATA(HEX0_BASE, uintto7seg[number]);
IOWR(HEX1_BASE,0, uintto7seg[number]);
}



void led_write(alt_u8 led_pattern) {
    IOWR(LED_BASE, 0, led_pattern);
}

alt_16 no_overflow(alt_u16 last, alt_32 change) {
	if (change>0) return 0xFFFF-change<last?0xFFFF:last+change;
	if (change<0) return 0-change>last?0:last+change;
	return last;
}

void update_xval(){
    alt_up_accelerometer_spi_read_x_axis(acc_dev, & x_read);
    x_val = no_overflow(x_val,4*x_read);
    led_write(x_val>>8);
    //printf("xvalue = %d, x_read %d\n",x_val,x_read);
    fprintf(fp,"%c\n",x_val>>8);
    usleep(SLEEP_TIME);
}



int main(){

  printf("Running..\n");

  int fd = open("/dev/jtag_uart",O_RDWR|O_NONBLOCK);
  printf("Opened File\n");

  fp = fdopen(fd, "r+");
  printf("Opened Stream\n");
  alt_u8 prompt;

  int score = 0;
  int speed = 4;



  acc_dev = alt_up_accelerometer_spi_open_dev("/dev/accelerometer_spi");

  x_val = 0;
  while(1) {
	  update_xval();
      int new_score = score;
      int new_speed = speed;
  	  fscanf(fp,"\u001bs%c\u001b",&new_speed);
  	  fscanf(fp,"\u001bg%c\u001b",&new_score);
      if (score != new_score) {
          score = new_score;
  		  IOWR_ALTERA_AVALON_PIO_DATA(LED_BASE, score);
      }

    }
    printf("Complete\n");

}