# hydraudp test script
# 
# - Connect two recorder to IP 192.168.1.1 (e.g. IVAP) and 192.168.1.3 (hipox) with the network
# - activate a maual camera using the atm
# - kill the hydraupd startet from atm (not the server) -> camera freezes
# - start this script and observe: no crashes, no frozen pictures on display, good switch timing

set -x
count=0

while [ $count -lt 25 ]; do
	{ echo "1192.168.1.1#1";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.1#2";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.1#3";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.1#4";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "4192.168.1.1#1#2#3#4";  sleep 1;  } > /var/run/hydraudp.fifo
	let count+=1
done

while [ $count -lt 50 ]; do
	{ echo "1192.168.1.3#1";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.3#2";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.3#3";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "1192.168.1.3#4";  sleep 1;  } > /var/run/hydraudp.fifo
	{ echo "4192.168.1.3#1#2#3#4";  sleep 1;  } > /var/run/hydraudp.fifo
	let count+=1
done

