Video access to remote recorder devices (MR3xxx hipox/MR4xxx himx0294-ivap)
Ralf Schröder (C) 2016,2017 DResearch Fahrzeugelektronik GmbH


# Version 1.0 State: simple demonstrator

The version 1.0 was developed as example application for accessing video
streams from MR3xxx(hipox)/MR4xxx(himx0294-ivap) recorder devices using
the public available library libdrtp. As documented within that code, it's
not recommended to use this in productive applications.
There are hints to improve the application.


# Version 1.1: stable version

The version 1.1 is a first stable and tested release. Tests are done:
* using the test.sh-Script (stress test) against all recorder device
  release versions 4 of MR3060-6 (hipox) and MR4860 (himx/ivap) documented
  internally within HYP-17629 (JIRA).
* as development-accompanying tests (interface, function, optic).
* long running test with release 4.7.152 to exclude resource problem.


## Functional description

The program (hydraudp.c) implements two software applications:
* a server application for continues run as systemd service (option -S).
  The server is a gstreamer application, which manages the connection to
  the recorder devices and displaying streams in quadrants (currently
  single and quad views). The control interface is a simple named FIFO
  /var/run/hydraudp.fifo. The FIFO protocol is line oriented and simple
  ASCII, so other programs could use it. By design, only one service is
  running and will be restarted if not available.
* an adapter application providing the functionality of version 1.0 to
  avoid changes in the main application. The adapter can be started with
  a simple command line requesting the display and can be killed via
  SIGINT/SIGTERM to stop the streams on the display. The stopping works
  with crashes/SIGKILL too but the timing for display restarts can be
  better with a correct signal shutdown. It's recommended to run one
  adapter only, however overlapping in live time is no problem.
  Technically it opens the FIFO, writes command(s) and waits for a
  signal to send a stop command and to close the FIFO gracefully. On
  hard termination the FIFO is closed too and the server receives a HUP.
  If there is no other writer (that could be a problem on overlapping
  executions and imply a longer switch delay for new requests with old
  running streams).

The protocol of the FIFO should be read in hydraudp.c function server_cmd.
It's a simple letter to indicate a command and arguments separated by '#'.
The implementation can change the command set and parameter handling if
necessary.

Logging is done to journald so catching any error messages or traces from
application side is not necessary. The log level and direction to stderr 
can be controlled by options. It would be nice to integrate the logging
completely into the gstreamer logging facility and to use systemd tagging.
But not with that version, for the development and customer reports the
implemented functionality should be sufficient.

The build binary hydraudp is placed into /usr/bin and available as service
hydraudp in /lib/systems/system/hydraudp.service.


## Server details

The server runs within a glib mainloop reading the FIFO as GChannel. For
all quadrants a gstreamer pipeline is creaded lazy on first use. Currently
4 quadrants are allowed (constant define) to implement a single and quad
view. This can be extended without problems even with scaling/cut options
e.g. for dual views. Pipelines are never destroyed in live time only
paused to reduce the preparation time for views significantly. On activation
the stream parameter and quadrant information are connected to the pipeline
and the state goes to running. There are checks to avoid changes on running
pipelines and use of the same stream on different places (probably a main
problem with version 1.0 with racy kill/start behavior). The video area
can be easy adapted attending the remarks on the WIDTH/HEIGHT defines to
other display sizes.

All running pipelines are checked periodically (100ms) for progress 
* in receiving RTP frames. After a timeout (a define in code, currently
  1000ms) the RTP connection is restarted. The restart window is increased
  by 1 second until 20 seconds are reached to avoid overrunning the remote
  RTP server in case of overload situations (e.g. alarm trigger).
* in display quadrants. After a timeout (a define in code, currently 700ms)
  the quadrant is colored black using the frame buffer device directly.
  There is a rare refresh of the quadrant with black if the pipeline is
  frozen. This happens also on connection problems! Pipelines can freeze
  with different reasons and extremely nondeterministic diagnostics:
    * incoming connection problems (catched with restarts).
	* diagnosed pipeline errors (error handler with abort of the
	  application), happens on bit errors in decoder instances and often
	  crashes the decoding HW requiring an application restart.
	* internal, often unreported pipeline errors (sometimes seen with
	  GST_DEBUG=3 and above), e.g. dma memory errors within the kernel
	  (stop/restart the pipeline helps)

Actually the escalation if the pipeline freezes is a black screen (except
for reported errors a restart of the server). If desired a larger timeout
could restart the server and more a service restart limit definition
could reboot the system. This error handling has other strange implications
and should be requested explicitly for implementation! A server restart
will not recover the last display command even if an adapter process is
running!

### Display flickering

It can be observed, that quadrants are randomly filled black. In most
cases the problem is caused by network problems
* bad cable material
* not fixed connectors
* bad switch, network performance, overload

Technically all streams are transmitted using UDP to realize a fast
glass to glass delay. TCP with retransmit on errors would create a high
delay within the transport, but it's good for recording streams. It is
legal to drop UDP packages, however a working correct designed network
should not drop packages! RTP streams are checked by sequence numbers,
missing packages (about 1400 bytes) are reported and frames (30-60kB)
with incomplete packages never or with special tag delivered to the
application. This creates holes in the stream, if many packages are
thrown away black quadrants with recovery actions. Starting recovery
the early implies reconnect delay so it is fine tuning to get correct
delay values within the applications.

We integrated a simple ping check with packet size 1400 bytes to report
network problems a critical error within the journal. This should be
observed in case of problems.

Other reasons can be found on the RTP server side. The devices are
designed for recording first. In overload situations the storage system
becomes priority for display streams. Reasons for overload are:
* recording of streams with more then 5MByte/sec in summary.
* depending on the used hardware "bad" configurations overloading a
  subsystem (graphic processor).
* situations like system start, update, configurations, service requests
  (backup requests from fleet management), ...
* bad harddiscs (e.g. with errors).
* environment situations which stop the harddisc (vibrations). 

Note, that old MR3060 recorder are much less powerful than the new
generation. It's not a good idea to run 4 JPEG encoder instances in full
PAL resolution. This is attended within the server code.

In doubt of configuration/network problems configure a recording pause, 
activate it and observe the system again. If the problems continue, the 
network has a problem.
