SUMMARY = "A user tool hydra streaming udp"
SECTION = "base"
LICENSE = "CLOSED"

PR = "r1"

S = "${WORKDIR}"
DEPENDS = "libdrtp gstreamer glib-2.0 systemd"

SRC_URI = "\
	file://hydraudp.c \
	file://hydraudp.service \
"

inherit systemd pkgconfig

SYSTEMD_SERVICE_${PN} = "hydraudp.service"

do_compile() {
	${CC} ${CFLAGS} ${LDFLAGS} `pkg-config --libs --cflags glib-2.0 gstreamer-1.0 libsystemd` -DVERSION_STRING=\"${PV}\" hydraudp.c -o hydraudp -ldrtp
}

do_install() {
	install -d ${D}${bindir}
	install -m 0755	${WORKDIR}/hydraudp	${D}${bindir}
	
	install -d ${D}${base_libdir}/systemd/system
	install -m 0644 ${S}/hydraudp.service ${D}${base_libdir}/systemd/system/
}
