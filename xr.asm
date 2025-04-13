
;  Copyright 2024, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


            #include include/bios.inc
            #include include/kernel.inc


          ; Executable program header

            org   2000h-6
            dw    start
            dw    end-start
            dw    start

start:      br    skipspc


          ; Build information

            db    11+80h                ; month
            db    27                    ; day
            dw    2024                  ; year
            dw    1                     ; build

            db    'See github.com/dmadole/MiniDOS-xr for more info',0


          ; ASCII control character definitions used for XMODEM protocol.

#define NUL 0       ; null is used instead of zero
#define SOH 1       ; start-of-header starts 128-byte packet
#define ETX 3       ; end-of-test recognized to cancel (control-c)
#define EOT 4       ; end-of-text is received after all packets
#define ACK 6       ; acknowledge is send following a valid packet
#define NAK 21      ; negative acknowledgement is sent after an error
#define CAN 24      ; cancel to abort transmission and abandon file


          ; I/O pin definitions for the bit-bang serial routines. These are by
          ; default compatible with the 1802/Mini and Pico/Elf machines.
          ; 
          ; BRMK and BRSP branch on mark and space input, resepectively, and
          ; and SEMK and SESP set the output to the mark and space states.

#define BRMK bn2
#define BRSP b2
#define SESP req
#define SEMK seq


          ; Proces the command-line argument, first skip any leading spaces,
          ; then set pointer to name, zero-terimate it, and check for any
          ; trailing spaces.

skipspc:    lda   ra                    ; skip any leading spaces
            lbz   dousage
            sdi   ' '
            lbdf  skipspc

            ghi   ra                    ; save pointer to filename
            phi   rf
            glo   ra
            plo   rf

            dec   rf                    ; back up to first character

skipnam:    lda   ra                    ; skip over filename characters
            lbz   doopen
            sdi   ' '
            lbnf  skipnam

            ldi   0                     ; zero terminate the filename
            dec   ra
            str   ra
            inc   ra

skipend:    lda   ra                    ; skip over any trailing spaces
            lbz   doopen
            sdi   ' '
            lbdf  skipend

dousage:    sep   scall                 ; either no argument or more than one
            dw    o_inmsg
            db    'USAGE: xr filename',13,10,0

            ldi   1                     ; return failure status
            sep   sret


          ; If the command line provided a filename, open it, creating it if
          ; it doesn't exist, or truncating it if it does exist.

doopen:     ldi   1+2                   ; create and truncate flags
            plo   r7

            ldi   fildes.1              ; pointer to file descriptor
            phi   rd
            ldi   fildes.0
            plo   rd

            sep   scall                 ; open and start receive if success
            dw    o_open
            lbnf  proceed

            sep   scall                 ; else output error message
            dw    o_inmsg
            db    'ERROR: cannot open output file',13,10,0

            ldi   1                     ; return failure status
            sep   sret


          ; The use of SCRT call and return has a high overhead that limits
          ; throughput via the BIOS calls. Each call and return adds 66 machine
          ; cycles, which at 4 MHz is 132 microseconds. At 57,600 baud there is
          ; 173 microseconds available per byte sent or received, so the call
          ; and return takes up 75% of the time available for each byte.
          ; 
          ; To work around this, we call the BIOS routines using a simple SEP
          ; subroutine call instead. We run with R5 as the program counter
          ; and load the BIOS entry point into R3 and then SEP R3. At the end
          ; of the BIOS routine, the SEP R5 returns directly to the caller.
          ;
          ; This obviously has limitations, in particular, it breaks SCRT while
          ; we are running with R5 as the PC, so this assumes that the BIOS
          ; routines themselves do not call subroutines with SCRT. For MBIOS
          ; and BIOS this is true, at least for the UART routines.
          ;
          ; We also want to be able to have a subroutine, so we will run with
          ; R6 as the main program counter. We will SEP R5 to call our
          ; subroutines, and they will SEP R3 to call BIOS subroutines.
          ;
          ; Send and receive subroutines are in the same page so we will set
          ; the high byte of the address into r5 once here, and we will set
          ; the low bytes for send and receive into r7 high and low bytes.

proceed:    ghi   re                    ; save terminal control echo flag
            stxd

            ani   %11111110             ; clear the echo flag for xmodem
            phi   re

            adi   %00000010             ; if 0 or 254 then use uart port
            ani   %11111100
            lbz   setuart


          ; Since RE.1 is between 2 and 254 then it means its a bit delay
          ; time and we are using the soft UART. Now decide which one.

            sep   scall                 ; say which we have chosen
            dw    o_inmsg
            db    'Receive XMODEM using EF/Q port... ',0


          ; If BIOS is MBIOS based on first version byte being 2, then we
          ; should use Nitro timing, otherwise use Riley timing.

            ldi   0fff9h.1              ; get pointer to version in bios
            phi   rb
            ldi   0fff9h.0
            plo   rb

            ldn   rb                    ; if first byte is 2 then mbios
            smi   2
            lbz   ismbios

            ldi   getsefq.0             ; else riley bios, set slow uart
            plo   r7
            ldi   putsefq.0
            phi   r7
            ldi   getsefq.1
            phi   r9

            lbr   prepare               ; and start xmodem transfer

ismbios:    ldi   getnitr.0             ; mbios to set fast nitro uart
            plo   r7
            ldi   putnitr.0
            phi   r7
            ldi   getnitr.1
            phi   r9

            lbr   prepare               ; and start xmodem transfer


          ; Since the UART code works through the BIOS API, it is universal
          ; regardless of the port or BIOS types.

setuart:    sep   scall                 ; say which we have chosen
            dw    o_inmsg
            db    'Receive XMODEM using UART port... ',0

            ldi   getuart.0             ; set routines for uart console port
            plo   r7
            ldi   putuart.0
            phi   r7
            ldi   getuart.1
            phi   r9


          ; Now that the UART is selected switch up the program counter
          ; and subroutine counter to prepare for transfer.

prepare:    glo   r6                    ; save r6 for main program counter
            stxd
            ghi   r6
            stxd

            glo   r5                    ; save r5 for subroutine pointer
            stxd
            ghi   r5
            stxd

            ldi   startit.1             ; switch program counter now to r6
            phi   r6
            ldi   startit.0
            plo   r6

            sep   r6                    ; continues below with p as r6


          ; We are running with R6 as the program counter now. Initialize
          ; the one-time things we need for the transfer.

startit:    ghi   r9                    ; set high byte of subroutine
            phi   r5

            ldi   0                     ; clear flag bits
            phi   r8

            ldi   1                     ; first expected packet is one
            plo   r8

            ldi   buffer.1              ; set buffer pointer to start
            phi   ra
            ldi   buffer.0
            plo   ra


          ; Flush the input until nothing has been received for about one
          ; second by calling input repeatedly until it times out. Then fall
          ; though and send a NAK character.

            glo   r7                    ; set subroutine pointer to input
            plo   r5

waitnak:    ldi   51                    ; keep getting input until timeout
            sep   r5
            lbnf  waitnak


          ; Send a NAK to provoke the sender to either start transmitting or
          ; to resend the last packet because it was in error.
            
sendnak:    ghi   r7                    ; set pointer to send byte routine
            plo   r5

            ldi   NAK                   ; send the NAK to transmitter
            sep   r5


          ; Receive the start of a packet, which for a normal XMODEM packet
          ; will be a SOH character.

recvsoh:    ghi   ra                    ; reset pointer to current buffer
            phi   rf
            glo   ra
            plo   rf

            ldi   255                   ; get byte, nak if long timeout
            sep   r5
            lbdf  sendnak

            xri   SOH^NUL               ; if soh then start of regular packet  
            lbz   recvpkt

            xri   EOT^SOH               ; if eot then transfer is all done
            lbz   alldone

            xri   ETX^EOT               ; if eot then transfer is all done
            lbz   abandon

            xri   CAN^ETX               ; if eot then transfer is all done
            lbz   abandon

            lbr   waitnak               ; any thing else, flush input and nak


          ; Get the block number and block number check byte and save for
          ; checking later. We do this outside of the data read so that it
          ; doesn't clog up the stacking of data segments in the buffer.

recvpkt:    ldi   51                    ; get block number, nak if timeout
            sep   r5
            lbdf  sendnak

            plo   rd                    ; save to check later on

            ldi   51                    ; get block check, nak if timeout
            sep   r5
            lbdf  sendnak

            phi   rd                    ; save to check later on


          ; Read the 128 data bytes into the buffer. Since the buffer is page-
          ; aligned, the XMODEM blocks will be half-page aligned to we can use
          ; the buffer index as the counter also.

nextpkt:    ldi   51                   ; get data byte, nak if timeout
            sep   r5
            lbdf  sendnak

            str   rf                   ; write byte into buffer and advance
            inc   rf

            glo   rf                   ; repeat until at 128 byte boundary
            ani   %1111111
            lbnz  nextpkt


          ; Read the final byte of the packet, the checksum. Save this for the
          ; moment, we will check it later when we calculate the checksum.

            ldi   51                   ; get the checksum, nak if timeout
            sep   r5
            lbdf  sendnak

            plo   re                   ; save into accumulator for checksum


          ; Check that the block number is valid (the block and block check
          ; are one's complements) and that the block is the one we are 
          ; expecting. As a special case, if we see the prior block again,
          ; send an ACK so that the transmitter will move forward.

            glo   rd                    ; its easier if we add 1 to block
            adi   1
            str   r2

            ghi   rd                    ; if check fails then wait and nak
            add
            lbnz  waitnak

            glo   r8                    ; if prior block then wait and ack
            sm
            lbz   waitack

            adi   1                     ; if not expected then wait and nak
            lbnz  waitnak


          ; Calculate the checksum of the data by subtracting all the data
          ; bytes from the checksum byte. If everything is correct, the result
          ; will be zero. The loop is unrolled by a factor of four for speed.

            ghi   ra                    ; reset pointer to start of packet
            phi   rf
            glo   ra
            plo   rf

            sex   rf                    ; argument for sm will by data bytes

sumloop:    glo   re                    ; subtrack four data bytes from sum
            sm
            inc   rf
            sm
            inc   rf
            sm
            inc   rf
            sm
            inc   rf
            plo   re

            glo   rf                    ; repeat until 128 byte boundary
            ani   %1111111
            lbnz  sumloop

            sex   r2                    ; set x back to r2 stack pointer

            glo   re                    ; error if sum not zero, flush and nak
            lbnz  waitnak


          ; If we have a full sector worth of data (four XMODEM packets), then
          ; write it out to the file. The write subroutine is called with
          ; SEP R3 since we need the PC back to R3 to call the kernel.

            ghi   rf                    ; don't write if not four packets
            smi   2+buffer.1
            lbnz  nowrite

            ldi   dowrite.1             ; set pointer to write subroutine
            phi   r3
            ldi   dowrite.0
            plo   r3

            sep   r3                    ; call subroutine to write data


          ; Get ready for the next block: increment the block number and
          ; set the buffer pointer just following the block just received.

nowrite:    glo   r8                    ; increment block number
            adi   1
            plo   r8

            ghi   rf                    ; advance buffer pointer
            phi   ra
            glo   rf
            plo   ra


          ; As a special case, that occurs with Tera Term, for example, the
          ; receiving side may queue multiple NAKs before it is ready to 
          ; send, and then send the first packet multiple times as a result.
          ; To help recover from this quickly, flush any remaining input only
          ; after the first packet, using a quick timeout.

            ghi   r8                    ; check flag if not first packet
            ani   1
            lbnz  sendack

            ghi   r8                    ; if flag not set, set it now
            ori   1
            phi   r8

            ldi   10                    ; set a very short timeout then wait
            lskp


          ; If a packet is received that is a duplicate of the last packet,
          ; then some kind of loss or corruption has occurred. To aid in error
          ; recovery, flush any remaining input before sending an ACK.

waitack:    ldi   51                    ; read input until there is no more
            sep   r5
            lbnf  waitack


          ; Send an ACK immediately in response to the good packet, and loop
          ; back and get the next packet.

sendack:    ghi   r7                    ; set subroutine pointer to send
            plo   r5

            ldi   ACK                   ; send ack since a good packet
            sep   r5

            lbr   recvsoh               ; and then get the next packet


          ; If the transfer is cancelled, store the normal SCRT environment
          ; and return to the operating system.

abandon:    ldi   cleanup.1             ; point to subroutine to restore
            phi   r7
            ldi   cleanup.0
            plo   r7

            sep   r7                    ; recover r5,r6 and set pc to r3

            sep   scall                 ; output cancelled message
            dw    o_inmsg
            db    'cancelled.',13,10,0

            ldi   fildes.1              ; pointer to file descriptor
            phi   rd
            ldi   fildes.0
            plo   rd

            sep   scall                 ; close output file
            dw    o_close

            ldi   1                     ; return failure status
            sep   sret


          ; After the last data packet, acknowledge the EOT end marker, then
          ; return back to the normal program counter and SCRT setup for
          ; final file operations and return to kernel.

alldone:    ghi   r7                    ; set subroutine pointer to send
            plo   r5

            ldi   ACK                   ; acknowledge end of packets
            sep   r5


          ; Restore the normal SCRT environment, flush the buffer to disk,
          ; output sucess and return.

            ldi   cleanup.1             ; point to subroutine to restore
            phi   r7
            ldi   cleanup.0
            plo   r7

            sep   r7                    ; recover r5,r6 and set pc to r3

            sep   scall                 ; output success message
            dw    o_inmsg
            db    'complete.',13,10,0

            glo   rf                    ; get length of data in buffer
            smi   buffer.0
            plo   rc
            ghi   rf
            smbi  buffer.1
            phi   rc

            ldi   buffer.1              ; get pointer to buffer data
            phi   rf
            ldi   buffer.0
            plo   rf

            ldi   fildes.1              ; pointer to file descriptor
            phi   rd
            ldi   fildes.0
            plo   rd

            sep   scall                 ; write remaining data to file
            dw    o_write

            sep   scall                 ; close the output file
            dw    o_close

            ldi   0                     ; return with success status
            sep   sret


          ; Reset the system to the normal SCRT environment by restoring r5
          ; and r6 and resetting the program counter to r3. Also restore the
          ; echo flag in RE.1.

cleanup:    ghi   r6                    ; move return address to r3
            phi   r3
            glo   r6
            plo   r3

            irx                         ; restore scrt return pointer
            ldxa
            phi   r5
            ldxa
            plo   r5

            ldxa                        ; restore return to kernel address
            phi   r6
            ldxa
            plo   r6

            ldx                         ; restore terminal echo flag
            phi   re

            sep   r3                    ; reset program counter to r3


          ; When we have a full 512-byte buffer, this subroutine writes it out
          ; to the file. This is called from the main program via SEP R3 since
          ; we can't use SCRT because of R6 being the program counter. This
          ; then allows us to restore R5 and call the kernel to write the data
          ; as normal before returning to the main program.

dowrite:    irx                         ; restore the srt return routine
            ldxa
            phi   r5
            ldx
            plo   r5

            ldi   512.1                 ; write a whole sector of data
            phi   rc
            ldi   512.0
            plo   rc

            ldi   buffer.1              ; reset pointer to start of buffer
            phi   rf

            ldi   fildes.1              ; pointer to file descriptor
            phi   rd
            ldi   fildes.0
            plo   rd

            sep   scall                 ; write the sector into the file
            dw    o_write

            ldi   buffer.1              ; reset pointer to start of buffer
            phi   rf

            glo   r5                    ; save scrt routine pointer again
            stxd
            ghi   r5
            stxd

            ghi   r9                    ; setup pointer to console routines
            phi   r5
            glo   r7
            plo   r5

            sep   r6                    ; return to main program


          ; File descriptor for output file. This is put here rather than
          ; at the end to optimize page alignment. It really doesn't matter
          ; where it is since we don't run from ROM.

fildes:     dw    0,0
            dw    dta
            dw    0
            db    0
            dw    0,0
            dw    0
            dw    0,0


          ; -----------------------------------------------------------------
          ; This is a simplified version of the Nitro soft UART implementing
          ; the same timing as the Mike Riley BIOS UART. Since the resolution
          ; is eight cycles the timing is much less complex, and there is
          ; also no decompression needed of the timing factor.
          ; 
          ; Like the version above, this has been folded so that thr return
          ; point is just before the entry point so that the program counter
          ; gets set for a repeated call.


          ; The delay after the start bit needs to be 1.5 times the bit time.
          ; This is implemented with a separate loop that delays 4 cycles per
          ; count which is the 0.5 part, then it falls into the regular bit
          ; delay of eight cycles per count, giving 12 cycles total.

efqcomp:    ghi   re                    ; delay four cycles per count
efqhalf:    smi   2
            bdf   efqhalf


          ; This is where we loop back for each additional bit afterward

efqloop:    ghi   re                    ; delay eitght cycles per count
efqtime:    smi   2
            nop
            lbdf  efqtime

            nop                         ; to make the timing just perfect

            BRMK  efqshrc               ; if space then shift in a zero bit

            glo   re                    ; data is space so shift in a zero
            shr
            br    efqnext

efqshrc:    shl
            glo   re                    ; data is mark so shift in a one bit
            shrc

efqnext:    plo   re                    ; save new bit, delay a little more
            nop
            nop

            lbdf  efqloop               ; more bits until stop bit emerges

efqstop:    BRSP  efqstop               ; wait until the stop bit starts

efqretn:    sep   r6                    ; return with pc pointing to start


          ; This is the entry point of the bit-bang UART receive routine. The
          ; first thing to do is watch for a start bit, but we need to have a
          ; time limit of how long to wait. Since we need to maintain high
          ; timing resolution, we check for the start bit change every-other
          ; instruction interleaved into the timing loop.

getsefq:    BRSP  efqinit               ; save timeout value
            phi   rb

            BRSP  efqinit               ; loop within the loop to add time
            ldi   3

efqdlay:    BRSP  efqinit               ; decrement loop counter in d
            smi   1

            BRSP  efqinit               ; loop until delay finished
            lbnz  efqdlay

            BRSP  efqinit               ; decrement main timer loop counter
            dec   rb

            BRSP  efqinit               ; check the high byte for zero
            ghi   rb

            BRSP  efqinit               ; if zero, then we have timed out
            lbz   efqretn

            BRMK  getsefq               ; continue until something happens


          ; The same shift register that is used to receive bits into is also
          ; used to count loops by preloading it with all ones except the last
          ; bit, which will shift out as zero when all the register is full.

efqinit:    ldi   %01111111              ; set stop bit into byte buffer
            plo   re

            lbr   efqcomp                ; enter regular bit delay routine


          ; -----------------------------------------------------------------
          ; The send version of Nitro adapted to Riley BIOS soft UART timing.

putsefq:    plo   re                    ; save byte to send to shift register

            ghi   re                    ; wait for minimum stop bit time
efqwait:    smi   2
            nop
            lbdf  efqwait


          ; Once the stop bit is timed, send the start bit and delay, and end
          ; the delay with DF set as we then jump into the send loop and this
          ; level will get shifted into the shift register just before we exit
          ; and so determines the stop bit level.

            ghi   re                    ; here and twice for timing reasons
            ghi   re

            SESP                        ; set start bit at space level

efqstrt:    smi   2                     ; delay eight cycles per count
            nop
            lbdf  efqstrt

            shl                         ; d is negative so this sets df
            lbr  efqshft

          ; This is where subsequent bits loop back after the start.

efqmore:    ghi   re                    ; delay 4 cycles for each 4 counts
efqbits:    smi   2
            nop
            lbdf  efqbits

efqshft:    nop                         ; extra delay for timing
            nop
            nop

            glo   re                    ; shift count bit in, data bit out
            shrc
            plo   re

            bdf   efqmark               ; if bit is one then that's a mark

            SESP                        ; else set space output, next bit
            lbr   efqmore

efqmark:    SEMK                        ; set mark output, do next bit
            lbnz  efqmore


          ; When the shift register is all zeros, we have sent 8 data bits and
          ; set the stop bit level. Return through the SEP in GETBITS so that
          ; the PC is reset to receive a byte each time after sending one.

            lbr   efqretn               ; return through getbits to set pc


          ; Make sure both entry points are in the same page.

          #if getsefq.1 != putsefq.1
            #error getsefq and putsefq not in the same page
          #endif


          ; Since we don't see R5.1 on each subroutine call the addresses
          ; for input and output need to be on the same page, align here to
          ; accomplish that for the following.

            org   (($-1)|255)+1


          ; ------------------------------------------------------------------
          ; This implements a receive byte with timeout function for the UART
          ; using BIOS routines by polling with F_UTEST to check if a byte is
          ; received while counting down a timer. To do this all quickly 
          ; enough, a special calling routine is used; see the notes elsewhere
          ; for a detailed explanation.
          ;
          ; The routine is folded on itself so that the return resets the
          ; subroutine instruction pointer back to the beginning of the routine
          ; so that it can quickly be called again.

uartchr:    ldi   f_uread.1             ; set subroutine pointer to read
            phi   r3
            ldi   f_uread.0
            plo   r3

            sep   r3                    ; read byte, no timeout so clear df
            adi   0

uartret:    sex   r2                    ; bios might have changed x, return
            sep   r6


          ; Entry point to read a byte from the UART with a timeout in RB.

getuart:    phi   rb

            ldi   f_utest.1             ; set subroutine pointer to test
            phi   r3
            ldi   f_utest.0
            plo   r3

            sep   r3                    ; if a byte is ready, then read it
            bdf   uartchr

            dec   rb                    ; else test again if time is not up
            ghi   rb
            bnz   getuart

            smi   0                     ; timer expired, return with df set
            br    uartret


          ; ------------------------------------------------------------------
          ; Send a byte through the UART using the F_UTYPE routine in BIOS.
          ; Aside from the calling convention, this is very simple. Return
          ; through GETUART so that the PC is setup for sending a byte.

putuart:    plo   re                    ; save the byte to send

            ldi   f_utype.1             ; set subroutine pointer to type
            phi   r3
            ldi   f_utype.0
            plo   r3

            glo   re                    ; get output byte and send it
            sep   r3

            br    uartret               ; return through getuart


          ; Make sure both entry points are in the same page.

          #if getuart.1 != putuart.1
            #error getuart and putuart not in the same page
          #endif


          ; ------------------------------------------------------------------
          ; This is a complex update of the Nitro UART from MBIOS; it has been
          ; modified to move the cycles for the bit rate factor decompression
          ; into the time of the start bit to minimize time and allow back-to-
          ; back bytes to be received without having to pre-decompress.
          ;
          ; This version also implements a timeout which is needed for XMODEM.
          ; The timeout value is in RB.1 with a value of 255 being about five
          ; seconds with a 4 MHz clock rate.
          ;
          ; The routine has also been folded into itself so that the return
          ; point is just before the entry point to facilitate calling by SEP
          ; by causing the entry point to be reset automaticaly each call.


          ; If greater than 64, then 1.5 bit times is more than 8 bits so we
          ; can't simply use the normal delay loop which has an 8-bit counter.
          ; So we do the half bit first then fall into a normal one-bit delay.

nitcomp:    shl                         ; double then add back and save
            add
            str   r2

            shr                         ; half and adjust for cycle count
            smi   6

nithalf:    smi   4                     ; delay in increments of 4 cycles
            bdf   nithalf

            adi   nitfrac+1             ; calculate jump from remainder
            plo   r5

            skp                         ; delay for between 2-5 cycles
            skp
            lskp
nitfrac:    ldi   0


          ; Delay for a full bit time using decompressed value from stack.

nitloop:    ldn   r2                   ; get delay time

nittime:    smi   4                    ; delay in increments of 4 cycles
            bdf   nittime

            adi   nitjump+1            ; calculate jump from remainder
            plo   r5

            skp                        ; delay for between 2-5 cycles
            skp
            lskp
nitjump:    ldi   0

            BRSP  nitspac               ; if space then shift in a zero bit

            glo   re                    ; data is mark so shift in a one bit
            shrc
            br    nitnext

nitspac:    glo   re                    ; data is space so shift in a zero
            shr
            plo   re

nitnext:    plo   re                    ; more bits to read if byte not zero
            bdf   nitloop

nitstop:    BRSP  nitstop               ; wait until the stop bit starts

nitretn:    sep   r6                    ; return with pc pointing to start


          ; This is the entry point of the bit-bang UART receive routine. The
          ; first thing to do is watch for a start bit, but we need to have a
          ; time limit of how long to wait. Since we need to maintain high
          ; timing resolution, we check for the start bit change every-other
          ; instruction interleaved into the timing loop.

getnitr:    BRSP  nitinit               ; save timeout value
            phi   rb

            BRSP  nitinit               ; loop within the loop to add time
            ldi   3

nitdlay:    BRSP  nitinit               ; decrement loop counter in d
            smi   1

            BRSP  nitinit               ; loop until delay finished
            lbnz  nitdlay

            BRSP  nitinit               ; decrement main timer loop counter
            dec   rb

            BRSP  nitinit               ; check the high byte for zero
            ghi   rb

            BRSP  nitinit               ; if zero, then we have timed out
            lbz   nitretn

            BRMK  getnitr               ; continue until something happens


          ; The same shift register that is used to receive bits into is also
          ; used to count loops by preloading it with all ones except the last
          ; bit, which will shift out as zero when all the register is full.

nitinit:    ldi   %01111111              ; set stop bit into byte buffer
            plo   re


          ; If the time factor is greater than 64 then we add twice the amount
          ; in excess of 64 back to it, so that each step above 64 amounts to
          ; three cycles instead of one. This gives the full 0-255 range from
          ; the 7 bits allotted by sacrificing resolution at higher values.

            ghi   re                    ; uncompress the stored delay value,
            shr
            smi   1                     ; shift right then subtract one, save
            str   r2                    ; on stack for below and re for use

            smi   63                    ; if value is less than 63 leave as-is
            bdf   nitcomp


          ; If we are in the 0 to 63 part of the range, simply add half back
          ; to the value to get the 1.5 bit times from start bit to the middle
          ; of the first data bit, then do to the normal delay.

            ldn   r2                    ; divide by two and then add to self
            shr
            add

            br    nittime               ; enter regular bit delay routine


          ; ------------------------------------------------------------------
          ; This is the transmit routine of the Nitro soft UART. This returns
          ; following setting the level of the stop bit to maximize time for
          ; processing, especially to receive a following character from the
          ; other end. To avoid stop bit violations, we therefore need to 
          ; delay on entry just a little less than a bit time.

putnitr:    plo   re                    ; save byte to send to shift register

            ghi   re                    ; uncompress the stored delay value,
            shr
            smi   1                     ; shift right then subtract one, save
            str   r2                    ; on stack for below and re for use

            smi   63                    ; if value is less than 63 leave as-is
            bnf   nitwait

            shl                         ; otherwise multiply by 2 then add to
            add                         ; saved value giving final range, save
            str   r2

            smi   4                     ; adjust for extra decompression time

nitwait:    smi   4                     ; wait for minimum stop bit time
            bdf   nitwait


          ; Once the stop bit is timed, send the start bit and delay, and end
          ; the delay with DF set as we then jump into the send loop and this
          ; level will get shifted into the shift register just before we exit
          ; and so determines the stop bit level.

            SESP                        ; set start bit level

            ldn   r2                    ; get bit time, do again as a no-op
            ldn   r2

nitstrt:    smi   4                     ; delay 4 cycles for each 4 counts
            bdf   nitstrt

            adi   nitsetf+1             ; jump into table for fine delay
            plo   r5


          ; For each bit we time the bulk delay with a loop and then jump into
          ; a specially-constructed table to create the fine delay to a single
          ; machine cycle. This is where we loop back for each bit to do this.
          ; Exit from the delay with DF clear as this will get shifted into
          ; the shift register, when it is all zeros that marks the end.

nitmore:    ldn   r2                    ; get bit time factor

nitbits:    smi   4                     ; delay 4 cycles for each 4 counts
            bdf   nitbits

            sdi   nitclrf-1             ; jump into table for fine delay
            plo   r5

nitclrf:    skp                         ; delays from here are 5,4,3,2 cycles
            skp
            lskp
nitsetf:    ldi   0

            glo   re                    ; shift count bit in, data bit out
            shrc
            plo   re

            bdf   nitmark               ; if bit is one then that's a mark

            SESP                        ; else set space output, next bit
            br    nitmore

nitmark:    SEMK                        ; set mark output, do next bit
            bnz   nitmore


          ; When the shift register is all zeros, we have sent 8 data bits and
          ; set the stop bit level. Return through the SEP in GETBITS so that
          ; the PC is reset to receive a byte each time after sending one.

            br    nitretn               ; return through getbits to set pc


          ; Make sure both entry points are in the same page.

          #if getnitr.1 != putnitr.1
            #error getnitr and putnitr not in the same page
          #endif


          ; The data buffer needs to be page aligned to simplify the pointer
          ; math so go ahead and align both of these here. Neither is
          ; included in the executable though since they are 'ds'.

            org   (($-1)|255)+1

dta:        ds    512                   ; sector buffer for fildes
buffer:     ds    512                   ; xmodem data receive buffer

end:        end   begin

