## valgrind + gdb

start debugging

```
valgrind --num-callers=500 --track-origins=yes --leak-check=full --vgdb=yes --vgdb-error=0 zig-out/bin/kkc
```


stop

```
monitor v.kill 
```
