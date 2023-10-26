# Setun70
Emulator of the Soviet ternary computer "Setun-70" (Сетунь-70).

Strtup ROM code with some IO and interrupt support is in the file ROM.ods. This file uses macros, and serves as a translator of Setun 70 assembler instructions. The first page of ROM.ods explains how to use it (in Russian). 

To build and run, do "make run". This will start four terminal emulator windows: one for Setun-70 CPU, one for Consul-254 teletype emulator, and two more to log the keyboard input (in punched tape form and as is). At the moment, the emulator runs a simple echo process that sends back the codes it reads from the console.
