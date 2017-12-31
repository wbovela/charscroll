This code is an exercise to investigate how to smooth scroll characters by redefining the character set instead of using the hardware scrolling method built into the VIC chip. The idea is to designate a number of characters who's definitions are changed to match those in a custom message. These designated characters (the Ticker) are then ROLled to the left (using the ROL command for moving bits). 

The code in this example isn't optimised and doesn'r pretend to be the only or even the best solution. It is meant as an exersice for myself and perhaps can help others to understand how things work.

I used Georg Rottensteiner's C64Studio development environment and the ACME syntax of assembly code.
