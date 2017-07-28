DESCRIPTION = "post-update step"
SECTION = "base"
LICENSE = "CLOSED"

inherit systemd allarch

RDEPENDS_${PN} = " u-boot-fw-utils bash "

PR = "r8"

SRC_URI = " file://fw-update.service \
            file://fw-good \
          "

FILES_${PN} = " ${base_libdir}/systemd \
                ${bindir}/hip-boot \
              "

SYSTEMD_SERVICE_${PN} = "fw-update.service"

do_install () {
  install -d ${D}${systemd_unitdir}/system
  install -m 0644 ${WORKDIR}/fw-update.service ${D}${systemd_unitdir}/system/
  sed -i -e 's,@BASE_BINDIR@,${base_bindir},g' \
         -e 's,@SYSCONFDIR@,${sysconfdir},g' \
         -e 's,@BINDIR@,${bindir},g' \
         -e 's,@SYSTEMD_UNITDIR@,${systemd_unitdir},g' \
         ${D}${systemd_unitdir}/system/fw-update.service

  install -d ${D}${bindir}
  install -m 0755 ${WORKDIR}/fw-good ${D}${bindir}/fw-good
}

