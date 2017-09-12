require hipos-image.inc

export IMAGE_BASENAME = "hipos-atm"

IMAGE_LINGUAS = "en-us de-de it-it"

IMAGE_FSTYPES = "tar.bz2"

IMAGE_INSTALL_remove_himx0294 += "   \
   packagegroup-hipos-qt     \
   packagegroup-hipos-qt-examples \
   packagegroup-hipos-gstreamer \
"
IMAGE_INSTALL += " \
  sqlite3 \
  poppler \
  atm \
  qtbase-fonts \
  qtbase-plugins \
  icu \
  gstreamer \
  gst-plugins-base-app \
  gst-plugins-base-meta \
  gst-plugins-good-meta \
  gst-plugins-ugly-meta \
  gst-meta-audio \
  gst-meta-video \
  gst-plugins-base-tcp \
  gst-plugins-good-meta \
  gst-plugins-good-udp \
  gst-plugins-good-rtp \
  gst-plugins-good-rtpmanager \
  gst-plugins-good-rtsp \
  gstreamer1.0-plugins-base-meta \
  gstreamer1.0-plugins-good-meta \
  gstreamer1.0-plugins-bad-meta \
  v4l-utils \
  live555 \
  hydraudp \
  packagegroup-fonts-truetype \
  liberation-fonts \
  psplash \
  fw-update \
"
