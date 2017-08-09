# common stuff for all images

require recipes-images/angstrom/systemd-image.bb

IMAGE_FEATURES += " package-management "

# remove connman
IMAGE_INSTALL_remove = "connman connman-angstrom-settings connman-plugin-loopback connman-plugin-ethernet connman-plugin-wifi connman-tools connman-client"

IMAGE_INSTALL += "          \
  kernel-modules            \
  kernel-devicetree         \
  linux-firmware-ralink     \
  htop                      \
  bash                      \
  python-netserver          \
  python-subprocess         \
  udev-extraconf            \
  hip-udev-extra-rules      \
  iptables		    \
  bluez5		    \
  openvpn		    \
  ntp ntp-utils     	    \
  hipos-system-tuning	    \
  hipos-usb-check	    \
  tzdata tzdata-misc tzdata-posix tzdata-right tzdata-africa tzdata-americas tzdata-antarctica tzdata-arctic tzdata-asia tzdata-atlantic tzdata-australia tzdata-europe tzdata-pacific \
"

IMAGE_INSTALL_append_hikirk += "   \
  u-boot-fw-utils           \
  libdrbcc                  \
  drbcc                     \
  hipos-device-info         \
  hipos-time                \
  hip-machinfo              \
  hipos-network-setup       \
"

IMAGE_INSTALL_append_himx0294 += "   \
  u-boot-fw-utils           \
  libdrbcc                  \
  libdrtp                   \
  drbcc                     \
  hipos-device-info         \
  hipos-time                \
  hip-machinfo              \
  hipos-network-setup       \
  hipos-net-check           \
"


IMAGE_INSTALL_append_himx0294 += " \
  tslib \
  tslib-calibrate \
  tslib-tests \
  packagegroup-hipos-qt     \
  packagegroup-hipos-qt-examples \
  packagegroup-fsl-gstreamer1.0 \
  packagegroup-hipos-gstreamer \
  gstreamer1.0-plugins-imx \
  eglinfo-fb \
  fsl-alsa-plugins \
"

IMAGE_INSTALL_append_imx6qsabrelite += " \
  tslib \
  tslib-calibrate \
  tslib-tests \
  packagegroup-hipos-qt \
  packagegroup-hipos-qt-examples \
  packagegroup-fsl-gstreamer1.0 \
  packagegroup-hipos-gstreamer \
  gstreamer1.0-plugins-imx \
  eglinfo-fb \
  fsl-alsa-plugins \
"

# profiling
IMAGE_INSTALL += "  \
  systemd-analyze   \
"

# helpers
IMAGE_INSTALL += "  \
  mtd-utils         \
  mc                \
  nano              \
  screen            \
  file              \
  minicom           \
  ppp		    \
"

# system / plumbing level tools
IMAGE_INSTALL += "  \
    ethtool         \
    ltrace          \
    strace          \
    pciutils        \
    tcpdump         \
    iperf           \
    iperf3          \
    procps          \
    wireless-tools  \
    init-ifupdown   \
"

# smb support
IMAGE_INSTALL += " samba-systemd "

# http/webdav support
IMAGE_INSTALL += "         \
    lighttpd               \
    lighttpd-module-webdav \
    lighttpd-module-alias  \
"

# file system tools
IMAGE_INSTALL += "      \
    e2fsprogs           \
    e2fsprogs-mke2fs    \
    dosfstools          \
    parted              \
    mdadm               \
    hdparm		\
    smartmontools	\
"
