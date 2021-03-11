
#define FILTER_TAP 5
#define WEIGHT_COEFF 1
#define INIT_WEIGHT 0.2
#define OFFSET -32
#define PWM_PERIOD 16

alt_8 pwm = 0;
alt_u8 led;
int level;

FILE* fp;
alt_u8 send_counter = 0;

void led_write(alt_u8 led_pattern) {
    IOWR(LED_BASE, 0, led_pattern);
}

void convert_read(alt_32 acc_read, int * level, alt_u8 * led) {
    acc_read += OFFSET;
    alt_u8 val = (acc_read >> 6) & 0x07;
    * led = (8 >> val) | (8 << (8 - val));
    * level = (acc_read >> 1) & 0x1f;
}

alt_32 set_filter_coeffs(alt_32* init, alt_32 read) {
  alt_32 result = 0;
  for (int i = FILTER_TAP-1; i > 0; i--) {
    *(init+i) = *(init+i-1) * WEIGHT_COEFF;
    result += *(init+i);
  }
  *init = read * INIT_WEIGHT;
  return result + *init;
}

void sys_timer_isr() {
    IOWR_ALTERA_AVALON_TIMER_STATUS(TIMER_BASE, 0);

    if (pwm < abs(level)) {

        if (level < 0) {
            led_write(led << 1);
        } else {
            led_write(led >> 1);
        }

    } else {
        led_write(led);
    }

    if (pwm > PWM_PERIOD) {
        pwm = 0;
    } else {
        pwm++;
    }


    alt_32 filter_coeffs[FILTER_TAP];
    alt_32 x_read;
    alt_up_accelerometer_spi_dev * acc_dev;
    acc_dev = alt_up_accelerometer_spi_open_dev("/dev/accelerometer_spi");
    alt_up_accelerometer_spi_read_x_axis(acc_dev, & x_read);
    convert_read(set_filter_coeffs(filter_coeffs,x_read), & level, & led);
    alt_u8* write = (alt_u8*)&x_read;
    if (!(send_counter++)) fprintf(fp,"%c%c%c%c",*write,*(write+1),*(write+2),*(write+3));

	if (ferror(fp)) {
		clearerr(fp);
	}

}

void timer_init(void * isr) {
    IOWR_ALTERA_AVALON_TIMER_CONTROL(TIMER_BASE, 0x0003);
    IOWR_ALTERA_AVALON_TIMER_STATUS(TIMER_BASE, 0);
    IOWR_ALTERA_AVALON_TIMER_PERIODL(TIMER_BASE, 0x900);
    IOWR_ALTERA_AVALON_TIMER_PERIODH(TIMER_BASE, 0x0000);
    alt_irq_register(TIMER_IRQ, 0, isr);
    IOWR_ALTERA_AVALON_TIMER_CONTROL(TIMER_BASE, 0x0007);

} 
