LICENSE = "LGPLv2.1"
LIC_FILES_CHKSUM = "file://LICENSE;md5=d41d8cd98f00b204e9800998ecf8427e"
SECTION = "multimedia"
DEPENDS = "poppler"
SRCREV = "629"
SRC_URI = "svn://svn.gecoitalia.biz:18080/svn;protocol=http;module=atm"

S = "${WORKDIR}/atm/"

inherit qmake5 systemd

SRC_URI += " \
        file://atm.service \
	file://xorg.conf \
	file://start.sh \
        "

FILES_${PN} = "${base_libdir}/systemd \
               ${sysconfdir}/systemd \
	       ${sysconfdir}/atm/* \
	       ${bindir}/* \"

SYSTEMD_SERVICE_${PN} = "atm.service"
SYSTEMD_AUTO_ENABLE_${PN} = "enable"

do_configure () {

   export REVISION=${SRCREV}
   set +e
   qmake5_base_do_configure
   
}

do_install () {
  install -d ${D}${sysconfdir}/atm
  install -d ${D}${sysconfdir}/atm/config
  install -d ${D}${sysconfdir}/atm/doc
  install -d ${D}${sysconfdir}/atm/stylesheet
  install -d ${D}/usr/bin/
  install -d ${D}${base_libdir}/systemd/system

  rm -f ${S}db
  sh ${S}restoredb.sh

  cp -f ${WORKDIR}/build/atm ${D}/usr/bin/

  cp -f ${S}db ${D}${sysconfdir}/atm
  cp -f ${S}config/* ${D}${sysconfdir}/atm/config/
  cp -f ${S}doc/* ${D}${sysconfdir}/atm/doc/
  cp -f ${S}stylesheet/* ${D}${sysconfdir}/atm/stylesheet/
  cp -f ${S}screenshot.raw ${D}${sysconfdir}/atm
  cp -f ${S}translate* ${D}${sysconfdir}/atm
  install -m 0755 ${WORKDIR}/start.sh ${D}${sysconfdir}/atm/

  install -m 0644 ${WORKDIR}/atm.service ${D}${base_libdir}/systemd/system/

}
