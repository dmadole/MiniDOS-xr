# MiniDOS-xr

This is an XMODEM receiver program for Mini/DOS that supports both the bit-banged console port and a hardware UART. Both are compatible on either 1802/Mini or Pico/Elf type machines; the appropriate UART is chosen automatically.

This implements the full XMODEM specification including timeouts and error recovery and is coded to be as fast as possible and will support 57600 baud with a 4 MHz clock through a UART.

