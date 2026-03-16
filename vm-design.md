vm design

...

passing values on the stack and sizing..

1. value / variable shit

to minimize codesize, i should do this:

load <id>
   |
vars: [ 0 => off0 (u32), 1 => off1 (u32)]
   |
buf/alloc: [ 0... ]
