How do we handle em. In my compiler it's kind of a free for all.
Basically, with the nanboxing (actually, just pointer boxing), we don't need any metadata EXCEPT FOR POINTERS TO OG STRUCTS.
But this happens for stuff like deconstructions n stuff n references.

x = 1
m = { dupa: { kupa: 123 } }
n = (&m.dupa)&.kupa

^^^ only copy when
  1. assigning to variables
  2. making structs.
  NOT when referencing variables!

Basically, get rid of metadata - the default is Value.Type!

Only when we move it around our interpreter do we pack it in a value.
