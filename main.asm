;*******************************************************************
;*
;* Title: "temp_meas"- temperature measurements
;*
;* Author:					Pengxu Chen
;* Version:					6.0						
;* Last updated:			11/19/2020
;* Target:			;ATmega4809 @3.3MHz
;*
;* DESCRIPTION:
;* This program polls the flag associated with ADC0. ADC0 convert 
;* analog data from a temperature sneor to a 10 bit binary. The
;* 10 bit binary need to go through serval calculation and 
;* conversion The flag is set by the ADC0 result ready interrupt.
;* If the interrupt flag is set, the result of the ADC0 is read
;* and converted to decimal number. 
;* 
;* This program also continually multiplexes the display so that 
;* the temperature are constantly seen on the display. The 
;* display time for each digit is control by the internal timer. 
;* Every time the timer is timed to the top value, it will set a 
;* interrupt flag, and the next digit will be displayed. 
;*
;*******************************************************************
.nolist
.include "m4809def.inc"
.list 
.dseg
digit_num: .byte 1   ;locate 1 bytes for digit_num in sram
led_display: .byte 4 ;locate 4 bytes for led_display in sram

.cseg
.equ PERIOD_EXAMPLE_VALUE = 65
reset:	
 	jmp start			;reset vector executed a power on
.org TCA0_OVF_vect      ;TCA0 over flow interrupt vector
	jmp TCA0_OVF_ISR        ;jump to "TCA0_ISR"
.org ADC0_RESRDY_vect
	jmp ADC0_RESRDY_ISR ;jump to "A_D_convert_ISR"

;Initialization of ports and registers
start:
	ldi r16, 0xff
	OUT VPORTD_DIR, r16 ;set portD as outpout
	OUT VPORTC_DIR, r16 ;set portC as outpout
	;configure TCA0
	ldi r16, TCA_SINGLE_WGMODE_NORMAL_gc 
	sts TCA0_SINGLE_CTRLB, r16  ;set TCA0 to normal mode
	ldi r16, TCA_SINGLE_OVF_bm	
	sts TCA0_SINGLE_INTCTRL, r16 ;enable TCA0's overflow interrupt 
	;load period low byte then high byte 
	ldi r16, LOW(PERIOD_EXAMPLE_VALUE)	;set the period 
	sts TCA0_SINGLE_PER, r16 
	ldi r16, HIGH(PERIOD_EXAMPLE_VALUE) ;set the period 
	sts TCA0_SINGLE_PER + 1, r16 
	;set clock and start timer
	ldi r16, TCA_SINGLE_CLKSEL_DIV256_gc | TCA_SINGLE_ENABLE_bm
	sts TCA0_SINGLE_CTRLA, r16
	ldi r16, 0x00
	sts digit_num, r16 ;initialize digit number to 0
	ldi YL, LOW(led_display)
	ldi YH, HIGH(led_display);set Y to point to start of led_display
	ldi r16, 0x00
	std Y+0, r16   ;turn one all segments of digit 0 when display
	std Y+1, r16   ;turn one all segments of digit 1 when display
	std Y+2, r16   ;turn one all segments of digit 2 when display
	std Y+3, r16   ;turn one all segments of digit 3 when display
	sei			;enable global interrupts
	rcall POST_display
	;confirgue ADC0
	ldi r16, VREF_ADC0REFSEL_2V5_gc
	sts VREF_CTRLA, r16   ;set reference voltage to 2.5V
	ldi r16, 0x04
	sts PORTE_PIN3CTRL, r16 ;disable portE's input buffer, intterupt 
	ldi r16, 0x45
	sts ADC0_CTRLC, r16 ;set samplecap, internal ref and frequency
	ldi r16, 0x0b
	sts ADC0_MUXPOS, r16 ;set PE1 to analog input
	ldi r16, 0x01
	sts ADC0_CTRLA, r16   ;Enable ADC0
	sts ADC0_INTCTRL, r16 ;enable ADC0's interrupt
	ldi r16, 0x01
	sts ADC0_COMMAND, r16  ;start ADC0's converstion

main:
	nop
	rjmp main              ;jump back to main

;*******************************************************************
;* 
;* "TCA0_OVF_ISR" - TCA0 overflow interrupt service routine
;*
;* Description: 
;* This routine is used to call the "multiplex_display" subroutine
;* to multiplexes the display whenever TCA0 interrupt flag is set. 
;* Evertime this routine is called, it first saves all the rgisters
;* that will be used in this routine. After program leaves 
;* "multiplex_display", it clears the interrupt flag. Also, before
;* program leaves this routine, it pops out all the registers that 
;* are pushed at the begging of this routine in a last in first out
;* order. 
;*
;* Author:					Pengxu Chen
;* Version:					2.0						
;* Last updated:			11/19/2020
;* Target:					;ATmega4809 @3.3MHz 
;* Number of words:         14
;* Number of cycles:        47
;* Low registers modified:
;* High registers modified: r16, r17, r18
;*
;* Parameters: None
;* Returns:    None
;*
;* Notes: None
;*
;*******************************************************************
TCA0_OVF_ISR:
	;push registers that will be used in routine to stack
	push r16  ;pushes r16 to stack
	push r17  ;pushes r17 to stack
	push r18  ;pushes r18 to stack
	in r16, cpu_SREG  ;pushes status register to stack
	push r16
	cli

	rcall multiplex_display    ;calls "multiplex_display"
	ldi r16, TCA_SINGLE_OVF_bm
	sts TCA0_SINGLE_INTFLAGS, r16

	pop r16
	out cpu_SREG, r16 ;pops status register back 
	pop r18   ;pops r18 back
	pop r17   ;pops r17 back
	pop r16   ;pops r16 back6
	reti    

;*******************************************************************
;* 
;* "ADC0_RESRDY_ISR" - ADC0 result ready interrupt service routine
;*
;* Description: 
;* This routine is used to read the result from ADC0 and compute and
;* convert to decimal number with one decimal place. More 
;* specifically, it first multiplies the ADC0's result with 2500.
;* Then, the middle two bytes of the multiplication result is
;* shifted to right two times, and subtract 500. Then, the 
;* computation result is converted to BCD format. Finally, the BCD
;* number is converted to 7 segment value. Also, the decimal point
;* of the second digit is turned on.
;*
;* Author:					Pengxu Chen
;* Version:					2.0						
;* Last updated:			11/19/2020
;* Target:			;ATmega4809 @3.3MHz 
;* Number of words:         14
;* Number of cycles:        1570
;* Low registers modified: r14, r15
;* High registers modified: r16, r17, r18, r19, r20, r21, r22, r23
;*                          r24
;*
;* Parameters: None
;* Returns:    None
;*
;* Notes: None
;*
;*******************************************************************
ADC0_RESRDY_ISR:
	;push registers that will be used in routine to stack
	push r14  ;pushes r14 to stack
	push r15  ;pushes r15 to stack
	push r16  ;pushes r16 to stack
	push r17  ;pushes r17 to stack
	push r18  ;pushes r18 to stack
	push r19  ;pushes r19 to stack
	push r20  ;pushes r20 to stack
	push r21  ;pushes r21 to stack
	push r22  ;pushes r22 to stack
	push r23  ;pushes r23 to stack
	push r24  ;pushes r24 to stack
	in r16, cpu_SREG  ;pushes status register to stack
	push r16
	cli

	lds r16, ADC0_RESL ;load low byte of result to r13
	lds r17, ADC0_RESH ;load high byte of result to r14
	ldi r18, 0xc4
	ldi r19, 0x09   ;load 2500 into r19:r18
	rcall mpy16u    ;multiply ADC0's output with 2500
	clc             ;clear carry
;divide multiplication result by 1024
;shift middle two bytes of multiply result to right 2 times
	lsr r20         ;right shift r20
	ror r19         ;right rotate r19
	lsr r20         ;right shift r20
	ror r19         ;right rotate r19
	ldi r16, 0xf4
	ldi r17, 0x01   ;load 500 to r17:r16
	sub r19, r16
	sbc r20, r17    ;subtract 500 from the division result
	mov r16, r19    ;copy r19 to t16
	mov r17, r20    ;copy r20 to t17
	rcall bin16_to_BCD  ;convert binary to BCD
	ldi ZL, LOW(led_display)
	ldi ZH, HIGH(led_display);set Z to point to start of led_display
	mov r18, r22       ;copy r22 to r18
	andi r18, 0x0f     ;mask r18 to only keep low half 4 bits
	rcall hex_to_7seg  ;convert hex to 7 seg 
	std Z+0, r18       ;stores the 7 segment value to led_display[0]
	mov r18, r22       ;copy r22 to r18
	andi r18, 0xf0     ;mask r18 to only keep high half 4 bits
	swap r18           ;swap r18
	rcall hex_to_7seg  ;convert hex to 7 seg 
	andi r18, 0x7f     ;turn on decimal points of digit 1
	std Z+1, r18       ;stores the 7 segment value to led_display[1]
	mov r18, r23       ;copy r23 to r18
	andi r18, 0x0f     ;mask r18 to only keep low half 4 bits
	rcall hex_to_7seg  ;convert hex to 7 seg 
	std Z+2, r18       ;stores the 7 segment value to led_display[2]
	mov r18, r23       ;copy r23 to r18
	andi r18, 0xf0     ;mask r18 to only keep high half 4 bits
	swap r18           ;swap r18
	rcall hex_to_7seg  ;convert hex to 7 seg 
	std Z+3, r18       ;stores the 7 segment value to led_display[3]
	ldi r16, 0x01
	sts ADC0_COMMAND, r16 ;start ADC0's converstion

	pop r16
	out cpu_SREG, r16 ;pops status register back 
	pop r24  ;pops r24 back
	pop r23  ;pops r23 back
	pop r22  ;pops r22 back
	pop r21  ;pops r21 back
	pop r20  ;pops r20 back
	pop r19  ;pops r19 back
	pop r18  ;pops r18 back
	pop r17  ;pops r17 back
	pop r16  ;pops r16 back
	pop r15  ;pops r15 back
	pop r14  ;pops r14 back
	reti

;*******************************************************************
;* 
;* "multiplex_display" - Multiplex the Four Digit LED Display
;*
;* Description: Updates a single digit of the display and increments
;* the digit_num to the value of the digit position to be displayed
;* next.
;* Author:					Pengxu Chen
;* Version:					3.0						
;* Last updated:			11/19/2020
;* Target:					ATmega4809 @ 3.3MHz
;* Number of words:         40
;* Number of cycles:        24
;* Low registers modified:	none
;* High registers modified: r16, r17, r18, YL, YH
;*
;* Parameters:
;* led_display: a four byte array that holds the segment values
;* pattern for each digit of the display. led_display[0] holds the 
;* segment for digit 0 (the rightmost digit) and so on.
;* digit_num: a byte variable, the least significant two bits  
;* provide the index of the next digit to be displayed.
;*
;* Returns: Outputs segment pattern and turns on digit driver for 
;* the next position in the display to be turned ON in the 
;* multiplexing sequence.
;*
;* Notes: 
;*
;*******************************************************************
multiplex_display:
	ldi YL, LOW(led_display)
	ldi YH, HIGH(led_display);set Y to point to start of led_display
	lds r17, digit_num      ;load digit number to r17
	cpi r17, 0              ;compare r17 with 0
	breq digit_zero         ;branch to digit_zero if r17 equal to 0
	cpi r17, 1              ;compare r17 with 1
	breq digit_one          ;branch to digit_one if r17 equal to 1
	cpi r17, 2              ;compare r17 with 2
	breq digit_two          ;branch to digit_two if r17 equal to 2
	cpi r17, 3              ;compare r17 with 3
	breq digit_three        ;branch to digit_three if r17 equal to 3
;multiplex display for digit0
digit_zero:
	ldi r16, 0xef       ;load r16 with 0x7f
	ldd r18, Y+0        ;load led_display[0] to r18
	out VPORTD_OUT, r18 ;output r18 to portD
	out VPORTC_OUT, r16 ;output r16 to portC
	rjmp check_digit_num;jump to check_digit_num
;multiplex display for digit1
digit_one:
	ldi r16, 0xdf       ;load r16 with 0xbf
	ldd r18, Y+1        ;load led_display[1] to r18
	out VPORTD_OUT, r18 ;output r18 to portD
	out VPORTC_OUT, r16 ;output r16 to portC
	rjmp check_digit_num;jump to check_digit_num
;multiplex display for digit2
digit_two:
	ldi r16, 0xbf       ;load r16 with 0xdf
	ldd r18, Y+2        ;load led_display[2] to r18 
	out VPORTD_OUT, r18 ;output r18 to portD
	out VPORTC_OUT, r16 ;output r16 to portC
	rjmp check_digit_num;jump to check_digit_num
;multiplex display for digit3
digit_three:
	ldi r16, 0x7f       ;load r16 with 0xef
	ldd r18, Y+3        ;load led_display[3] to r18 
	out VPORTD_OUT, r18 ;output r18 to portD
	out VPORTC_OUT, r16 ;output r16 to portC
	rjmp check_digit_num;jump to check_digit_num
;set digit number to digit0
reset_digit:
	ldi r17, 0           ;load r17 with 0
	rjmp update_digit_num;jump to update_digit_num

check_digit_num:
	inc r17              ;increase r17 by 1
	cpi r17, 4           ;compare r17 wtih 4
	breq reset_digit     ;branch to reset_digit if r17 is 4 

update_digit_num:
	sts digit_num, r17  ;store r17 to digit_num
	ret                 ;return to stack
;*******************************************************************
;* 
;* "hex_to_7seg" - Hexadecimal to Seven Segment Conversion
;*
;* Description: Converts a right justified hexadecimal digit to the 
;* seven segment pattern required to display it. Pattern is right 
;* justified a through g. Pattern uses 0s to turn segments on ON.
;*
;* Author:					Pengxu Chen
;* Version:					1.0						
;* Last updated:			11/19/2020
;* Target:					ATmega4809
;* Number of words:			10
;* Number of cycles:		15
;* Low registers modified:		none		
;* High registers modified:		r16, r18, ZL, ZH
;*
;* Parameters: r18: right justified hex digit, high nibble 0
;* Returns: r18: segment values a through g right justified
;*
;* Notes: 
;*
;*******************************************************************
hex_to_7seg:
	andi r18, 0x0F			  ;clear ms nibble
    ldi ZH, HIGH(hextable * 2);set Z to point to start of table
    ldi ZL, LOW(hextable * 2)
    ldi r16, $00			;add offset to Z pointer
    add ZL, r18
    adc ZH, r16
    lpm r18, Z         ;load byte from table pointed to by Z
	ldi ZL, LOW(led_display)
	ldi ZH, HIGH(led_display);load adress of led_display to Z
	ret

    ;Table of segment values to display digits 0 - F
    ;!!! seven values must be added - verify all values
hextable: .db $81, $CF, $92, $86, $CC, $A4, $A0, $8F
		  .db $80, $8C, $88, $E0, $B1, $C2, $B0, $B8

;*******************************************************************
;* 
;* "post_display" - post display
;*
;* Description: This program quickly multiplex each digit on display
;* to display "8.", so that it looks like all the digits displays
;* "8." at the same time. After one second, all the digits are
;*  turned off.
;*
;* Author:					Pengxu Chen
;* Version:					2.0
;* Last updated:			11/19/2020
;* Target:					ATmega4809
;* Number of words:         8
;* Number of cycles:        3341865
;* Low registers modified:
;* High registers modified: r16, r19
;*
;* Parameters: None
;* Returns:    None
;*
;* Notes: 
;*
;*******************************************************************
post_display:
	ldi r19, 159  ;call var_delay 159 times
loop:
	ldi r16, 63     ;each time "var_delay" delays for 6.3ms
	rcall var_delay
	dec r19
	brne loop
	ldi r16, 0xff       
	out VPORTC_OUT, r16 ;turn off all the digit
	ret

;*******************************************************************
;* 
;* "var_delay" - varriable delay
;*
;* Description: Delay the system for certain period of time
;*
;* Author:					Pengxu Chen
;* Version:					2.0
;* Last updated:			11/12/2020
;* Target:					ATmega4809
;* Number of words:         7
;* Number of cycles:        16654
;* Low registers modified:
;* High registers modified: r16, r17
;*
;* Parameters: None
;* Returns:    None
;*
;* Notes: 
;*
;*******************************************************************
var_delay:
outer_loop:
	ldi r17,110    ;inner loop counter
inner_loop:
	dec r17
	brne inner_loop ;if r17 is not equal 0, brach to "inner_loop" 
	dec r16
	brne outer_loop ;if r16 is not equal 0, brach to "outer_loop" 
	ret

;*******************************************************************
;* 
;* "bin16_to_BCD" - 16-bit Binary to BCD Conversion
;*
;* Description: Converts a 16-bit unsigned binary number to a five
;* digit packed BCD number. Uses subroutine div16u from Atmel
;* application note AVR200
;*
;* Author:					Ken Short
;* Version:					0.0
;* Last updated:			11/19/2020
;* Target:					ATmega4809
;* Number of words:         15
;* Number of cycles:        1258
;* Low registers modified:	r14, r15
;* High registers modified: r16, r17, r18, r19, r20, r22, r23, r24
;*
;* Parameters: r17:r16 16-bit unsigned right justified number to
;*             be converted.
;* Returns:		r24:r23:r22 five digit packed BCD result.
;*
;* Notes: 
;* Subroutine uses repeated division by 10 to perform conversion.
;*******************************************************************
bin16_to_BCD:
	ldi r19, 0		;high byte of divisor for div16u
	ldi r18, 10		;low byte of the divisor for div16u

	rcall div16u	;divide original binary number by 10
	mov r22, r14   ;result is BCD digit 0 (least significant digit)
	rcall div16u	;divide 1st division's result by 10, as digit 1 
	swap r14		;swap digit 1 for packing
	or r22, r14		;pack

	rcall div16u	;divide 2rd division's result by 10, as digit 2 
	mov r23, r14	;place in r23
	rcall div16u	;divide 3rd division's result by 10, as digit 3 
	swap r14		;swap digit 3 for packing
	or r23, r14		;pack

	rcall div16u	;divide 4th division's result by 10, as digit 4 
	mov r24, r14	;place in r24

	ret


;Subroutine div16u is from Atmel application note AVR200

;*******************************************************************
;*
;* "div16u" - 16/16 Bit Unsigned Division
;*
;* This subroutine divides the two 16-bit numbers 
;*# "dd16uH:dd16uL" (dividend) and "dv16uH:dv16uL" (divisor). 
;* The result is placed in "dres16uH:dres16uL" and the remainder in
;* "drem16uH:drem16uL".
;*  
;* Number of words	:19
;* Number of cycles	:235/251 (Min/Max)
;* Low registers used	:2 (drem16uL,drem16uH)
;* High registers used  :5 (dres16uL/dd16uL,dres16uH/dd16uH,dv16uL,
;*						    dv16uH, dcnt16u)
;*
;*******************************************************************

;***** Subroutine Register Variables

.def	drem16uL=r14
.def	drem16uH=r15
.def	dres16uL=r16
.def	dres16uH=r17
.def	dd16uL	=r16
.def	dd16uH	=r17
.def	dv16uL	=r18
.def	dv16uH	=r19
.def	dcnt16u	=r20

;***** Code

div16u:	clr	drem16uL	;clear remainder Low byte
	sub	drem16uH,drem16uH;clear remainder High byte and carry
	ldi	dcnt16u,17	;init loop counter
d16u_1:	rol	dd16uL		;shift left dividend
	rol	dd16uH
	dec	dcnt16u		;decrement counter
	brne	d16u_2		;if done
	ret			;    return
d16u_2:	rol	drem16uL	;shift dividend into remainder
	rol	drem16uH
	sub	drem16uL,dv16uL	;remainder = remainder - divisor
	sbc	drem16uH,dv16uH	;
	brcc	d16u_3		;if result negative
	add	drem16uL,dv16uL	;    restore remainder
	adc	drem16uH,dv16uH
	clc			;    clear carry to be shifted into result
	rjmp	d16u_1		;else
d16u_3:	sec			;    set carry to be shifted into result
	rjmp	d16u_1

;*******************************************************************
;*
;* "mpy16u" - 16x16 Bit Unsigned Multiplication
;*
;* This subroutine multiplies the two 16-bit register variables 
;* mp16uH:mp16uL and mc16uH:mc16uL.
;* The result is placed in m16u3:m16u2:m16u1:m16u0.
;*  
;* Number of words	:14 + return
;* Number of cycles	:153 + return
;* Low registers used	:None
;* High registers used  :7 (mp16uL,mp16uH,mc16uL/m16u0,mc16uH/
;*                          m16u1,m16u2, m16u3,mcnt16u)	
;*
;*******************************************************************

;***** Subroutine Register Variables

.def	mc16uL	=r16		;multiplicand low byte
.def	mc16uH	=r17		;multiplicand high byte
.def	mp16uL	=r18		;multiplier low byte
.def	mp16uH	=r19		;multiplier high byte
.def	m16u0	=r18		;result byte 0 (LSB)
.def	m16u1	=r19		;result byte 1
.def	m16u2	=r20		;result byte 2
.def	m16u3	=r21		;result byte 3 (MSB)
.def	mcnt16u	=r22		;loop counter

;***** Code

mpy16u:	clr	m16u3		;clear 2 highest bytes of result
	clr	m16u2
	ldi	mcnt16u,16	;init loop counter
	lsr	mp16uH
	ror	mp16uL

m16u_1:	brcc	noad8		;if bit 0 of multiplier set
	add	m16u2,mc16uL	;add multiplicand Low to byte 2 of res
	adc	m16u3,mc16uH	;add multiplicand high to byte 3 of res
noad8:	ror	m16u3		;shift right result byte 3
	ror	m16u2		;rotate right result byte 2
	ror	m16u1		;rotate result byte 1 and multiplier High
	ror	m16u0		;rotate result byte 0 and multiplier Low
	dec	mcnt16u		;decrement loop counter
	brne	m16u_1		;if not done, loop more
	ret
