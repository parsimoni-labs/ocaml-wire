#!/usr/bin/env python3
# measure.py CMD...: run CMD, print its wall time and the peak resident
# memory of its waited-for children (F* and the Z3 it runs).
import resource, subprocess, sys, time
t = time.time()
rc = subprocess.call(sys.argv[1:])
wall = time.time() - t
ru = resource.getrusage(resource.RUSAGE_CHILDREN)
print(f"wall {wall:.1f}s user {ru.ru_utime:.1f}s maxrss {ru.ru_maxrss / 1048576:.0f}MB rc {rc}", file=sys.stderr)
sys.exit(rc)
