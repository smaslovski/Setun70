# Setun70
Emulator of the Soviet ternary computer "Setun-70" (Сетунь-70).

Strtup ROM code with some IO and interrupt support is in the file ROM.ods. This file uses macros, and serves as a translator of Setun 70 assebler. The first page of ROM.ods explains how to use it (in Russian). 

To build and run, do "make run". This will start four terminal emulator windows: one for Setu 70 CPU, one for Consul-254 emulator, and two more to log the keybord output (in punched tape for and as is). At the moment, the emulator runs a simple echo process that sends backs the codes it reads from the console.
