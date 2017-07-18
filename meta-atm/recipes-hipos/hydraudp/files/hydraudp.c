/* Editor hints for vim
 * vim:set ts=4 sw=4 noexpandtab:  */
/**\file
 * \brief  Video access to remote recorder devices
 * \author Ralf Schröder
 * 
 * (C) 2016 DResearch Fahrzeugelektronik GmbH
 *
 * RSR: comments and code modifications added at 04.01.2017 
 *
 * $Id: bf53b0c27dea0b53306c820a719536508d8855e1 $
 *
 */
#include <stdio.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>
#include <time.h>
#include <sys/time.h>
#include <gst/gst.h>

#include <drtp.h>

#define HIMX0294_ST4022           /**< himx0294 ST4022 boards with 0 vin and 1 vout connector connector */
char REMOTE_IP[64] = "172.29.23.18";  /**< IP address of the remote device providing streams */
#define HEIGHT 768                /**< display target height */
#define WIDTH  960                /**< display target width */
#define DEMO_DELAY_SEC 10         /**< switch time for demo function */
#define MIN_DELAY_MSEC 300        /**< stream lost detection in ms */


/* e.g. ST4022 without multiplex circuit on vout */
#define FRAMEBUFFER "/dev/fb1"    /**< Frame buffer device of the display, the physical connector depends on the used hardware */
#define MAX_VOUT 1                /**< variant 1 display, no other supported vout connectors */
#define MAX_VIN 0                 /**< no vin connectors with that hardware */

/** Helper macros to simplify the code */
#define CHECK_PTR(P)  ((P) || (printf("line %u '%s' is NULL\n",  __LINE__, #P), abort(), 0))
#define CHECK_TRUE(B) ((B) || (printf("line %u '%s' is FALSE\n", __LINE__, #B), abort(), FALSE))

static struct remote_struct *rCam[4];
static int numCam = 0;
static int start = 1;
static GMainLoop *s_pGMainloop;
static int hydraVer = 1;

/** Simple file write helper */
static void write_file(const char* filename, const char* what, unsigned size) {
    int fd = open(filename, O_WRONLY);
    int ret;
    if (fd < 0) {
        printf("open %s failed: %s\n", filename, strerror(errno));
        return;
    }
    if ((int) size != (ret = write(fd, what, size))) {
        printf("write %s to %s failed (ret:%d): %s\n", what, filename, ret, strerror(errno));
    }
    close(fd);
    printf("%s:  %s to %s\n", __func__, what, filename);
}

/** Switch on/off the physical connector of the given frame buffer (off means no sync signal's are generated) */
static void fb_sync(unsigned fb, gboolean state) {
    char buf[64];
    sprintf(buf, "/sys/class/graphics/fb%u/blank", fb);
    if (g_file_test(buf, G_FILE_TEST_EXISTS)) {
        printf("%s: fb%u %s\n", __func__, fb, (state ? "on" : "off"));
        write_file(buf, (state ? "0" : "1"), 1);
    } else {
        printf("%s: %s not supported\n", __func__, buf);
    }
}

/** Connector activation. This process is highly hardware dependent. */
static int vout_activation(unsigned vout, int cam) {
    vout--;
    assert(vout < MAX_VOUT);
    assert(cam <= MAX_VIN);
    if (cam > 0) {
        printf("bypassc setting  vout/%u to %u not supported by hardware\n", vout + 1, cam);
        return -1;
    } else {
        printf("%s:  set vout/%u to %d\n", __func__, vout + 1, cam);
        if (0 == cam) {
            fb_sync(2 * vout, TRUE);
        } else {
            fb_sync(2 * vout, FALSE);
        }
        return 0;
    }
}


/*** RTP standard stuff taken directly from RFC ***/

/*
 * Table K.1 from JPEG spec.
 */
static const int jpeg_luma_quantizer[64] = {
    16, 11, 10, 16, 24, 40, 51, 61,
    12, 12, 14, 19, 26, 58, 60, 55,
    14, 13, 16, 24, 40, 57, 69, 56,
    14, 17, 22, 29, 51, 87, 80, 62,
    18, 22, 37, 56, 68, 109, 103, 77,
    24, 35, 55, 64, 81, 104, 113, 92,
    49, 64, 78, 87, 103, 121, 120, 101,
    72, 92, 95, 98, 112, 100, 103, 99
};

/*
 * Table K.2 from JPEG spec.
 */
static const int jpeg_chroma_quantizer[64] = {
    17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99
};

/*
 * Call MakeTables with the Q factor and two u_char[64] return arrays
 */
static void MakeTables(int q, u_char *lqt, u_char *cqt) {
    int i;
    int factor = q;

    if (q < 1) factor = 1;
    if (q > 99) factor = 99;
    if (q < 50)
        q = 5000 / factor;
    else
        q = 200 - factor * 2;

    for (i = 0; i < 64; i++) {
        int lq = (jpeg_luma_quantizer[i] * q + 50) / 100;
        int cq = (jpeg_chroma_quantizer[i] * q + 50) / 100;

        /* Limit the quantizers to 1 <= q <= 255 */
        if (lq < 1) lq = 1;
        else if (lq > 255) lq = 255;
        lqt[i] = lq;

        if (cq < 1) cq = 1;
        else if (cq > 255) cq = 255;
        cqt[i] = cq;
    }
}

/*
   The following routines can be used to create the JPEG marker segments
   corresponding to the table-specification data that is absent from the
   RTP/JPEG body.
 */

static u_char lum_dc_codelens[] = {
    0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
};

static u_char lum_dc_symbols[] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
};

static u_char lum_ac_codelens[] = {
    0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d,
};

static u_char lum_ac_symbols[] = {
    0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12,
    0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
    0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08,
    0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
    0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16,
    0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
    0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
    0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
    0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
    0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
    0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
    0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
    0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
    0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
    0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
    0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4,
    0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
    0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea,
    0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
    0xf9, 0xfa,
};

static u_char chm_dc_codelens[] = {
    0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
};

static u_char chm_dc_symbols[] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
};

static u_char chm_ac_codelens[] = {
    0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77,
};

static u_char chm_ac_symbols[] = {
    0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21,
    0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
    0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91,
    0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
    0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34,
    0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
    0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38,
    0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
    0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
    0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
    0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
    0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96,
    0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
    0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4,
    0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
    0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2,
    0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
    0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9,
    0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
    0xf9, 0xfa,
};

static u_char * MakeQuantHeader(u_char *p, u_char *qt, int tableNo) {
    *p++ = 0xff;
    *p++ = 0xdb; /* DQT */
    *p++ = 0; /* length msb */
    *p++ = 67; /* length lsb */
    *p++ = tableNo;
#if 0
    memcpy(p, qt, 64);
    return (p + 64);
#else  /* We need the reorder, somehow the code from standard is incorrect */
    int z;
    /* jpeg_natural_order[i] is the index (in natural order) of 8x8 table entry (in zig-zag order) */
    static const int jpeg_natural_order[] ={
        0, 1, 8, 16, 9, 2, 3, 10,
        17, 24, 32, 25, 18, 11, 4, 5,
        12, 19, 26, 33, 40, 48, 41, 34,
        27, 20, 13, 6, 7, 14, 21, 28,
        35, 42, 49, 56, 57, 50, 43, 36,
        29, 22, 15, 23, 30, 37, 44, 51,
        58, 59, 52, 45, 38, 31, 39, 46,
        53, 60, 61, 54, 47, 55, 62, 63,
    };

    for (z = 0; z < 64; z++) {
        int i = jpeg_natural_order[z];
        *p++ = qt[i];
    }
    return p;
#endif
}

static u_char * MakeHuffmanHeader(u_char *p, u_char *codelens, int ncodes, u_char *symbols, int nsymbols, int tableNo, int tableClass) {
    *p++ = 0xff;
    *p++ = 0xc4; /* DHT */
    *p++ = 0; /* length msb */
    *p++ = 3 + ncodes + nsymbols; /* length lsb */
    *p++ = (tableClass << 4) | tableNo;
    memcpy(p, codelens, ncodes);
    p += ncodes;
    memcpy(p, symbols, nsymbols);
    p += nsymbols;
    return (p);
}

static u_char *
MakeDRIHeader(u_char *p, u_short dri) {
    *p++ = 0xff;
    *p++ = 0xdd; /* DRI */
    *p++ = 0x0; /* length msb */
    *p++ = 4; /* length lsb */
    *p++ = dri >> 8; /* dri msb */
    *p++ = dri & 0xff; /* dri lsb */
    return (p);
}

/*
 *  Arguments:
 *    type, width, height: as supplied in RTP/JPEG header
 *    lqt, cqt: quantization tables as either derived from
 *         the Q field using MakeTables() or as specified
 *         in section 4.2.
 *    dri: restart interval in MCUs, or 0 if no restarts.
 *
 *    p: pointer to return area
 *
 *  Return value:
 *    The length of the generated headers.
 *
 *    Generate a frame and scan headers that can be prepended to the
 *    RTP/JPEG data payload to produce a JPEG compressed image in
 *    interchange format (except for possible trailing garbage and
 *    absence of an EOI marker to terminate the scan).
 */
static int MakeHeaders(u_char *p, int type, int w, int h, u_char *lqt, u_char *cqt, u_short dri) {
    u_char *start = p;

    /* convert from blocks to pixels */
    w <<= 3;
    h <<= 3;

    *p++ = 0xff;
    *p++ = 0xd8; /* SOI */

    p = MakeQuantHeader(p, lqt, 0);
    p = MakeQuantHeader(p, cqt, 1);

    if (dri != 0)
        p = MakeDRIHeader(p, dri);

    *p++ = 0xff;
    *p++ = 0xc0; /* SOF */
    *p++ = 0; /* length msb */
    *p++ = 17; /* length lsb */
    *p++ = 8; /* 8-bit precision */
    *p++ = h >> 8; /* height msb */
    *p++ = h; /* height lsb */
    *p++ = w >> 8; /* width msb */
    *p++ = w; /* wudth lsb */
    *p++ = 3; /* number of components */
    *p++ = 0; /* comp 0 */
    if (type == 0)
        *p++ = 0x21; /* hsamp = 2, vsamp = 1 */
    else
        *p++ = 0x22; /* hsamp = 2, vsamp = 2 */
    *p++ = 0; /* quant table 0 */
    *p++ = 1; /* comp 1 */
    *p++ = 0x11; /* hsamp = 1, vsamp = 1 */
    *p++ = 1; /* quant table 1 */
    *p++ = 2; /* comp 2 */
    *p++ = 0x11; /* hsamp = 1, vsamp = 1 */
    *p++ = 1; /* quant table 1 */
    p = MakeHuffmanHeader(p, lum_dc_codelens,
            sizeof (lum_dc_codelens),
            lum_dc_symbols,
            sizeof (lum_dc_symbols), 0, 0);
    p = MakeHuffmanHeader(p, lum_ac_codelens,
            sizeof (lum_ac_codelens),
            lum_ac_symbols,
            sizeof (lum_ac_symbols), 0, 1);
    p = MakeHuffmanHeader(p, chm_dc_codelens,
            sizeof (chm_dc_codelens),
            chm_dc_symbols,
            sizeof (chm_dc_symbols), 1, 0);
    p = MakeHuffmanHeader(p, chm_ac_codelens,
            sizeof (chm_ac_codelens),
            chm_ac_symbols,
            sizeof (chm_ac_symbols), 1, 1);

    *p++ = 0xff;
    *p++ = 0xda; /* SOS */
    *p++ = 0; /* length msb */
    *p++ = 12; /* length lsb */
    *p++ = 3; /* 3 components */
    *p++ = 0; /* comp 0 */
    *p++ = 0; /* huffman table 0 */
    *p++ = 1; /* comp 1 */
    *p++ = 0x11; /* huffman table 1 */
    *p++ = 2; /* comp 2 */
    *p++ = 0x11; /* huffman table 1 */
    *p++ = 0; /* first DCT coeff */
    *p++ = 63; /* last DCT coeff */
    *p++ = 0; /* sucessive approx. */

    return (p - start);
};


/*** end of RTP standard stuff ***/


/*** handle RTP connection  ***/

/** Connecton object holding all relevant information */
struct remote_struct /* inherits struct drtp_stream_handle_t */ {
    struct drtp_stream_handle_t sh; /**< librtp stream handle, first member for cast to struct remote_struct in rtp callbacks */
	unsigned vin; /* vin id for tracing */

    GstElement *pipeline; /**< pipeline container */
    GstElement *appsrc; /**< application source */
    GstElement *vpudec; /**< decoder */
    //GstElement *gelem; /**< XXX */
    GstElement *display; /**< display sink */

    unsigned q; /**< rtp subheader q */
    unsigned height; /**< rtp subheader height */
    unsigned width; /**< rtp subheader width */
    unsigned type; /**< rtp subheader type */
    unsigned header_offset; /**< rtp subheader type */
    unsigned char framebuf[150 * 1024]; /**< rtp frame buffer memory */

    gint frame_count; /**< frame counter (statistics only) */
    //gint frame_ts; /**< last frame timestamp from display */
	struct timeval display_ts;
	struct timeval frame_ts;
	GMutex mutex_ts;  /**< timestamp mutex because setting is not atomic */
	int reconnect_delay;  /** reconnect deley in sec to avoid overloading the recorder */
    
    gboolean trace_fames; /**< trace flag */
};

/** libdrtp error callback */
static void rtp_error_cb(const char *msg) {
    if (msg) {
        unsigned len = strlen(msg);
        if (msg[len - 1] == '\n') {
            len -= 1;
        }
        printf("libdrtp: %.*s\n", len, msg);
    }
}

/** Called before receiving a new frame to provide a frame buffer. We use a simple static one here */
static void * rtp_memory_cb(struct drtp_stream_handle_t* sh, uint32_t *len) {
    struct remote_struct *remote = (struct remote_struct*) sh;
    void * data = remote->framebuf + remote->header_offset;
    *len = sizeof (remote->framebuf) - remote->header_offset;
    return data;
}

/** Called for each read frame, note: the requested resolution can differ with the request in case of shared vout-streams with multiple display applications */
static enum drtp_status rtp_frame_cb(struct drtp_stream_handle_t* sh, uint32_t flags, uint32_t cam, uint32_t width, uint32_t height, uint64_t ts, void* data, uint32_t len, struct rtp_with_subheaders_t * header) {
    enum drtp_status ret = DRTP_SUCCESS;
    struct remote_struct* remote = (struct remote_struct*) sh;

    //remote->trace_fames = 1;
    if (remote->trace_fames) printf("%s: %p cam:%u %ux%u flags:%02X size:%u ts:%llu\n", __func__, data, cam, width, height, flags, len, ts);
    assert(remote->framebuf + remote->header_offset == data);
    assert(sizeof (remote->framebuf) >= remote->header_offset + len);
    if (header->rtp.bits.bit.type != RTP_TYPE_JPEG) {
        printf("%s: only jpeg supported: %d\n", __func__, header->rtp.bits.bit.type);
	} else if (
			//!remote->header_offset || (header->sub.jpeg.bit.type != remote->type) || (header->sub.jpeg.bit.width != remote->width) ||
			(header->sub.jpeg.bit.type != remote->type) || (header->sub.jpeg.bit.width != remote->width) ||
			(header->sub.jpeg.bit.height != remote->height) || (header->sub.jpeg.bit.q != remote->q)
			) {
		/* The received stream does not include a jpeg header, we have to reconstruct it as proposed in the rtp rfc. It's unchanged, so we reuse it
		 * and put it on start of out buffer. To simplify the procedure, we do not display the first frame.
		 */
		//u_char lqt[64], cqt[64];
		//MakeTables(header->sub.jpeg.bit.q, lqt, cqt);
		//remote->header_offset = MakeHeaders(remote->framebuf, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, lqt, cqt, 0);
		remote->type = header->sub.jpeg.bit.type;
		remote->width = header->sub.jpeg.bit.width;
		remote->height = header->sub.jpeg.bit.height;
		remote->q = header->sub.jpeg.bit.q;
		if ((((uint8_t*)data)[0] != 0xFF) && (((uint8_t*)data)[1] != 0xD8))
		{
			u_char lqt[64], cqt[64];
			MakeTables(header->sub.jpeg.bit.q, lqt, cqt);
			remote->header_offset = MakeHeaders(remote->framebuf, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, lqt, cqt, 0);
			printf("%s generate header with q:%u type:%u w:%u h:%u header:%u\n", __func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, remote->header_offset);
		} else {
			printf("%s header with q:%u type:%u w:%u h:%u\n", __func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, 8*header->sub.jpeg.bit.width, 8*header->sub.jpeg.bit.height);
		}
	} else {
        if (1
#ifndef FORWARD_INCLOMPLETE_FRAMES
                && !(flags & DRTP_PACKET_LOST)
#endif
                ) {
            GstBuffer *buf = gst_buffer_new_allocate(0, remote->header_offset + len, 0);
            gsize s = gst_buffer_fill(buf, 0, remote->framebuf, remote->header_offset + len);
            if (remote->trace_fames) printf("%s q:%u type:%u w:%u h:%u header:%u data:%u size:%u\n", __func__, header->sub.jpeg.bit.q, header->sub.jpeg.bit.type, header->sub.jpeg.bit.width, header->sub.jpeg.bit.height, remote->header_offset, len, s);
            GstFlowReturn fret;
            g_signal_emit_by_name(remote->appsrc, "push-buffer", buf, &fret);
            if (fret != GST_FLOW_OK) {
                printf("%s: buffer not pushed", __func__);
			} else {
				g_mutex_lock(&remote->mutex_ts);
				gettimeofday(&remote->frame_ts, 0);
				g_mutex_unlock(&remote->mutex_ts);
			}
            gst_buffer_unref(buf); /* Free the buffer now that we are done with it */
        }
    }
    return ret;
}

/** Called for each frame on display sinks to update the progress timestamp */
static GstPadProbeReturn display_progress_cb(GstPad *pad, GstPadProbeInfo *info, gpointer _remote) {
    (void) pad;
    (void) info;
    struct remote_struct *remote = _remote;
    g_atomic_int_inc(&remote->frame_count);
	/* RSR here a stamp in ms should be used to detect frozen streams in about 300ms. Attend then correct locking for access in other threads. */
    //g_atomic_int_set(&remote->frame_ts, time(0));
	g_mutex_lock(&remote->mutex_ts);
	gettimeofday(&remote->display_ts, 0);
	g_mutex_unlock(&remote->mutex_ts);
    return GST_PAD_PROBE_OK;
}


static void on_error(GstBus *bus, GstMessage *message, gpointer _remote)
{
    struct remote_struct *remote = _remote;
	(void)bus;
	if (GST_MESSAGE_ERROR == GST_MESSAGE_TYPE(message))
	{
		GError *err = NULL;
		gchar *dbg_info = NULL;
		gst_message_parse_error (message, &err, &dbg_info);
		printf("gst error message: vin/%u element %s: %s (%s)\n", remote->vin, GST_OBJECT_NAME(message->src), err->message, (dbg_info ? dbg_info : ""));
		g_error_free (err);
		g_free (dbg_info);
	} else {
		printf("%s vin/%u called with type %d\n", __func__, remote->vin, GST_MESSAGE_TYPE(message));
	}
}


/** Activate the given vin-connector of a remote device in display coordinates x,y with given dimension, no wait 
 *   pipeline: appsrc ! imxvpudec ! imxg2dvideosink
 *   Note: Running 4CIF can affect the remote device (performance issue). It's recommended to use 2CIF with scaling.
 *   Multiple streams of one source is because of the sharing on source not a performance issue, but as implemented
 *   here a network issue (multiple network transmissions). The first requested resolution wins, other streams have to
 *   scale.
 */
static struct remote_struct* remote_vin_start(unsigned vin, unsigned x, unsigned y, unsigned width, unsigned height) {
    
    struct remote_struct *remote = g_new0(struct remote_struct, 1);
	GstBus *bus;
	remote->vin = vin;
	g_mutex_init(&remote->mutex_ts);
    CHECK_PTR(remote->pipeline = gst_pipeline_new(NULL));

    CHECK_PTR(remote->appsrc = gst_element_factory_make("appsrc", NULL));
    CHECK_TRUE(gst_bin_add(GST_BIN(remote->pipeline), remote->appsrc));

    GstCaps *caps = gst_caps_new_empty_simple("image/jpeg");
    g_object_set(remote->appsrc, "caps", caps, NULL);
    gst_caps_unref(caps);

    CHECK_PTR(remote->vpudec = gst_element_factory_make("imxvpudec", NULL));
    CHECK_TRUE(gst_bin_add(GST_BIN(remote->pipeline), remote->vpudec));

    CHECK_PTR(remote->display = gst_element_factory_make("imxg2dvideosink", NULL));
    //CHECK_PTR(remote->display = gst_element_factory_make("imxipuvideosink", NULL));
    CHECK_TRUE(gst_bin_add(GST_BIN(remote->pipeline), remote->display));
	printf("%s: %ux%u %ux%u\n", __func__, x, y, width, height);
    g_object_set(remote->display, "framebuffer", FRAMEBUFFER, "force-aspect-ratio", FALSE, "window-x-coord", x, "window-y-coord", y, "window-width", width, "window-height", height, NULL);
    GstPad* pad;
    CHECK_PTR(pad = gst_element_get_static_pad(remote->display, "sink"));
    gst_pad_add_probe(pad, GST_PAD_PROBE_TYPE_BUFFER, display_progress_cb, remote, NULL);
    gst_object_unref(pad);
    pad = 0;

    //CHECK_PTR(remote->gelem = gst_element_factory_make("imxipuvideotransform", NULL));
    //CHECK_TRUE(gst_bin_add(GST_BIN(remote->pipeline), remote->gelem));

    CHECK_TRUE(gst_element_link_many(remote->appsrc, remote->vpudec, remote->display, NULL));
	bus = gst_pipeline_get_bus(GST_PIPELINE (remote->pipeline));
	gst_bus_add_signal_watch(bus);
	g_signal_connect (bus, "message::error", (GCallback)on_error, remote);
    CHECK_TRUE(GST_STATE_CHANGE_FAILURE != gst_element_set_state(remote->pipeline, GST_STATE_PLAYING));

	gettimeofday(&remote->display_ts, 0);
	remote->frame_ts = remote->display_ts;
	//remote->frame_ts = time(0);
        
        if (width == WIDTH)
	{
                printf("starting %s vin/%u on display [%u,%u] width %u height %u (704x288)\n", REMOTE_IP, vin, x, y, width, height);
		CHECK_TRUE(DRTP_SUCCESS == drtp_udp_stream_start(&remote->sh, rtp_frame_cb, rtp_memory_cb, REMOTE_IP, vin, 704, 288));
		
	} else { // quad
                printf("starting %s vin/%u on display [%u,%u] width %u height %u (352x288)\n", REMOTE_IP, vin, x, y, width, height);
		CHECK_TRUE(DRTP_SUCCESS == drtp_udp_stream_start(&remote->sh, rtp_frame_cb, rtp_memory_cb, REMOTE_IP, vin, 704/2, 288/2));
		
	}
        
        
    return remote;
}

/** Somehow timer controlled check function, mainly to realize a display notification for frozen streams (not implemented here). On network problems a restart could help. */
static void remote_vin_check(struct remote_struct* remote) {
	// RSR this is much to slow to detect frozen streams. This code must be improved.
	// This code also does not detect gstreamer failures (find reasons with GST_DEBUG=4 on program start), what can happen. For that cases
	// you will need gstreamer error handler, pipline/program restarts etc. That's rather complex code, not part of the demo.
#if 0
	if (g_atomic_int_get(&remote->frame_ts) + DEMO_DELAY_SEC / 2 <= time(0)) {
		printf("rtp stream failure, try to restart\n"); /* or draw the quadrant with a nice no cam picture... */
		enum drtp_status ret = drtp_stream_restart(&remote->sh); /* this call reconnects the UDP rtp stream, works well after server problems */
		if (ret != DRTP_SUCCESS) {
			printf("%s restart rtp stream failed: %s\n", __func__, drtp_strerror(ret));
		} else {
			g_atomic_int_set(&remote->frame_ts, time(0));
		}
	}
#else
	struct timeval now;
	gettimeofday(&now, 0);
	g_mutex_lock(&remote->mutex_ts);
	int display_progess_ms = (now.tv_sec - remote->display_ts.tv_sec) * 1000 + (now.tv_usec - remote->display_ts.tv_usec) / 1000;
	int src_progess_ms = (now.tv_sec - remote->frame_ts.tv_sec) * 1000 + (now.tv_usec - remote->frame_ts.tv_usec) / 1000;
	g_mutex_unlock(&remote->mutex_ts);
	if (src_progess_ms > MIN_DELAY_MSEC + 1000 * remote->reconnect_delay)
	{
		/* RSR: I recommend an error handling id reconnect_delay >= 3..10 */
		printf("rtp stream failure (%dms), try to restart\n", display_progess_ms); /* or draw the quadrant with a nice no cam picture... */
		enum drtp_status ret = drtp_stream_restart(&remote->sh); /* this call reconnects the UDP rtp stream, works well after server problems */
		if (ret != DRTP_SUCCESS) {
			printf("%s restart rtp stream failed: %s\n", __func__, drtp_strerror(ret));
		} else {
			remote->reconnect_delay++;
			g_mutex_lock(&remote->mutex_ts);
			gettimeofday(&remote->frame_ts, 0);
			g_mutex_unlock(&remote->mutex_ts);
		}
	} else {
		if (src_progess_ms > MIN_DELAY_MSEC)
		{
			printf("display problems in vin/%u delay %dms\n", remote->vin, display_progess_ms); /* or draw the quadrant with a nice no cam picture... */
		} else {
			//printf("rtp stream check %d/%dms\n", src_progess_ms, display_progess_ms); /* or draw the quadrant with a nice no cam picture... */
			remote->reconnect_delay = 0;
		}
	}
#endif
}

/** Stop the rtp streaming and display pipeline */
static void remote_vin_stop(struct remote_struct* remote) {
    assert(remote);
    enum drtp_status ret = drtp_stream_stop(&remote->sh);
    if (DRTP_SUCCESS != ret) {
        printf("%s stopping rtp stream vin/%u failed: %s\n", __func__, remote->vin, drtp_strerror(ret));
    } else {
        printf("%s stopped rtp stream vin/%u after %u frames\n", __func__, remote->vin, remote->frame_count);
    }
    CHECK_TRUE(GST_STATE_CHANGE_FAILURE != gst_element_set_state(remote->pipeline, GST_STATE_NULL));
    gst_object_unref(remote->pipeline);
    remote->pipeline = 0;
    g_free(remote);
}

static void remote_vin_fast_stop(struct remote_struct* remote)
{
	assert(remote);
	enum drtp_status ret = drtp_stream_stop(&remote->sh); /* notify the remote side to avoid resource problems with hanging rtp streams */
	/* Stopping the pipline is nice but need some time, so leave it alone */
}

void sighandler(int signum)
{
   printf("Caught signal %d, coming out...\n", signum);
   int i;
   start = 0;
    if(signum!=SIGABRT){
        for (i = 0; i < numCam; i++) {
            //remote_vin_stop(rCam[i]); //RSR: speedup the shutdown procedure
            remote_vin_fast_stop(rCam[i]);
        }
        g_main_loop_quit(s_pGMainloop);
    }else{
        exit(1);
    }
}

static gboolean cam_check_timer(gpointer _numCam)
{
	unsigned numCam = GPOINTER_TO_UINT(_numCam);
	int i;
	for (i = 0; i < numCam; i++) {
		remote_vin_check(rCam[i]);
	}
	return TRUE;
}


int main(int argc, char** argv) {
    
    int i, w, h, x, y;

    //signal(SIGINT, sighandler);
    //CHECK_TRUE(SIG_ERR != signal(SIGKILL, sighandler)); /* RSR: this can never be catched, returns an error, do never use SIGKILL here, this leaks stream resources!!! */
    CHECK_TRUE(SIG_ERR != signal(SIGTERM, sighandler)); /* RSR: use SIGTERM not SIGINT to stop that program correctly */
    CHECK_TRUE(SIG_ERR != signal(SIGINT, sighandler)); /* RSR: use SIGTERM not SIGINT to stop that program correctly */
    //signal(SIGCHLD, sighandler);
        
    numCam = argc - 2;
    
    sprintf(REMOTE_IP,"%s",argv[1]);

    w = WIDTH;
    h = HEIGHT;

    if (numCam > 1) {

        w = w / 2;
        h = h / 2;
    }

    time_t now;
    gst_init(NULL, NULL);
    CHECK_TRUE(DRTP_SUCCESS == drtp_init());
    drtp_set_error_logger(rtp_error_cb, 0);

    for (i = 0; i < numCam; i++) {
        printf("argv[ %d ] = %s\n", i, argv[ i + 2 ]);

        switch (i) {

            case 0:
                x = y = 0;
                break;
            case 1:
                x = w;
                y = 0;
                break;
            case 2:
                x = 0;
                y = h;
                break;
            case 3:
                x = w;
                y = h;
                break;

        }

        rCam[i] = remote_vin_start(atoi(argv[ i + 2 ]), x, y, w, h);
    }

	g_timeout_add(100, cam_check_timer, GUINT_TO_POINTER(numCam));
	s_pGMainloop = g_main_loop_new (0, FALSE);
	g_main_loop_run (s_pGMainloop);
	g_main_loop_unref (s_pGMainloop);

#if 0
    while (start) {

        for (i = 0; i < numCam; i++) {
            remote_vin_check(rCam[i]);
        }
        //sleep(1);
        usleep(250000);

    }
    /*
    remote_vin_stop(r1);
    r1 = 0;
    remote_vin_stop(r2);
    r2 = 0;
    remote_vin_stop(r3);
    r3 = 0;
    remote_vin_stop(r4);
    r4 = 0;
    */
#endif
    return 0;
}

/* Editor hints for emacs
 *
 * Local Variables:
 * mode:c
 * c-basic-offset:4
 * indent-tabs-mode:t
 * tab-width:4
 * End:
 * 
 * NO CODE BELOW THIS! */
