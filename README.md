wol
===

A program and library to a send WoL Magic Packet, to remotely start a computer.

This program is useful if you have a computer that you keep at a friend's house
because he has a fast internet connection. But he keeps turning the machine
off, saying that it warms up his room and your not using it anyway.
But now you need a way to turn the machine back on...

Usage:
------

    > $ wol hostname macaddress [portnumber]

If the hostname is not statically known, you can try using LAN broadcast
(`255.255.255.255`).

