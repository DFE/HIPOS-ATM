SUMMARY = "A user tool hydra streaming udp"
SECTION = "base"
LICENSE = "GPLv2"
LIC_FILES_CHKSUM = "file://LICENSE;md5=d41d8cd98f00b204e9800998ecf8427e"

PR = "r0"

S = "${WORKDIR}"
DEPENDS = "libdrtp gstreamer glib-2.0"

SRC_URI = "file://hydraudp.c \
	   file://LICENSE \
"

do_compile() {
	${CC} ${CFLAGS} ${LDFLAGS} `pkg-config --libs --cflags glib-2.0 gstreamer-1.0` hydraudp.c -o hydraudp -ldrtp
}

do_install() {
	install -d ${D}${bindir}

	install -m 0755	${WORKDIR}/hydraudp	${D}${bindir}
	
}
