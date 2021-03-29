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
#define SLEEP_TIME 50
#define FILTER_SIZE 1200
FILE* fp;
alt_32 x_read[FILTER_SIZE];
alt_u16 x_val = 127<<8;
alt_up_accelerometer_spi_dev *  acc_dev;
alt_u8 send_counter = 0;
alt_u8 uintto7seg[10] = {0b00000010,0b11110011,0b00100101,0b00001101,0b10011001,0b01001001,0b01000010,0b00011111,0b000000001,0b00001001};
int speed = 4;
alt_64 acc;

void write_to_hex(alt_u8 number, alt_u8 hex_num) {
    alt_u32 dest,writedata;
    switch (hex_num) {
    case 0: dest = HEX0_BASE; break;
    case 1: dest = HEX1_BASE; break;
    case 2: dest = HEX2_BASE; break;
    case 3: dest = HEX3_BASE; break;
    case 4: dest = HEX4_BASE; break;
    case 5: dest = HEX5_BASE; break;
    default: return;
    }
    switch (number % 10) {
    case 0: writedata = 0b11000000; break;
    case 1: writedata = 0b11111001; break;
    case 2: writedata = 0b10100100; break;
    case 3: writedata = 0b10110000; break;
    case 4: writedata = 0b10011001; break;
    case 5: writedata = 0b10010010; break;
    case 6: writedata = 0b10000010; break;
    case 7: writedata = 0b11111000; break;
    case 8: writedata = 0b10000000; break;
    case 9: writedata = 0b10010000; break;
    }
    IOWR_ALTERA_AVALON_PIO_DATA(dest, writedata);
    write_to_hex(number/10,hex_num+1);
}
alt_16 no_overflow(alt_u16 last, alt_32 change) {
    if (change>0) return 0xFFFF-change<last?0xFFFF:last+change;
    if (change<0) return 0-change>last?0:last+change;
    return last;
}
void update_xval(){
    acc-=x_read[FILTER_SIZE]/FILTER_SIZE;
    for (int i = 0; i < FILTER_SIZE-1;i++) {
        x_read[i + 1] = x_read[i];
    }
    acc+=x_read[0]/FILTER_SIZE;
    alt_up_accelerometer_spi_read_x_axis(acc_dev, & x_read[0]);
    x_val = no_overflow(x_val,speed*filtered(x_read));
    //led_write(x_val>>8);
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

  acc_dev = alt_up_accelerometer_spi_open_dev("/dev/accelerometer_spi");
  x_val = 0;
  while(1) {
      update_xval();
      char read = -1;
      read = getc(fp);
      if (read == '\x1b') {
          switch (getc(fp)) {
            case 's':
              speed = getc(fp);
            break;
            case 'g':
              score = getc(fp);
          }
      }
      write_to_hex(speed,0);
    }
    printf("Complete\n");
}
