/*
 * AC-3 parser
 * Copyright (c) 2003 Fabrice Bellard
 * Copyright (c) 2003 Michael Niedermayer
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "config.h"
#include "config_components.h"

#include "libavutil/channel_layout.h"
#include "parser.h"
#include "ac3defs.h"
#include "ac3tab.h"
#include "ac3_parser.h"
#include "ac3_parser_internal.h"
#include "aac_ac3_parser.h"
#include "get_bits.h"


#define AC3_HEADER_SIZE 7

#if CONFIG_AC3_PARSER

static const uint8_t eac3_blocks[4] = {
    1, 2, 3, 6
};

/**
 * Table for center mix levels
 * reference: Section 5.4.2.4 cmixlev
 */
static const uint8_t center_levels[4] = { 4, 5, 6, 5 };

/**
 * Table for surround mix levels
 * reference: Section 5.4.2.5 surmixlev
 */
static const uint8_t surround_levels[4] = { 4, 6, 7, 6 };

/*
static int parse_ac3_emdf(GetBitContext *gbc, int *num_objects_oamd, int *num_objects_joc)
{
    av_log(NULL, AV_LOG_DEBUG, "parse_ac3_emdf\n");
    return 0;
}
*/

int avpriv_eac3atmos_parse_header(const uint8_t *buf, int buf_size, AACAC3ParseContext *phdr)
{
    GetBitContext gbc;
    init_get_bits(&gbc, buf, buf_size * 8);

    int sync_word = get_bits(&gbc, 16);
    if (sync_word != 0x0B77)
        return 0;

    int frame_type = get_bits(&gbc, 2);
    // substreamid
    skip_bits(&gbc, 3);
    int frame_size = (get_bits(&gbc, 11) + 1) << 1;
    if(frame_size < AC3_HEADER_SIZE)
        return AAC_AC3_PARSE_ERROR_FRAME_SIZE;

    int num_blocks = 6;
    int sample_rate;
    int sr_shift;
    int channels;
    int channel_mode;
    int lfe_on;
    int bit_rate;
    int sr_code = get_bits(&gbc, 2);
    if (sr_code == 3) {
        int sr_code2 = get_bits(&gbc, 2);
        if(sr_code2 == 3)
            return AAC_AC3_PARSE_ERROR_SAMPLE_RATE;
        sample_rate = ff_ac3_sample_rate_tab[sr_code2] / 2;
        sr_shift = 1;
    } else {
        num_blocks = eac3_blocks[get_bits(&gbc, 2)];
        sample_rate = ff_ac3_sample_rate_tab[sr_code];
        sr_shift = 0;
    }

    channel_mode = get_bits(&gbc, 3);
    lfe_on = get_bits1(&gbc);

    bit_rate = 8LL * frame_size * sample_rate /
                    (num_blocks * 256);
    channels = ff_ac3_channels_tab[channel_mode] + lfe_on;


    int i, blk;
    int dialog_normalization[2];
    int compression_exists[2];
    int preferred_downmix;

    int bitstream_id = get_bits(&gbc, 5);
    if (bitstream_id != 0x10)
      return 0;

    /* volume control params */
    for (i = 0; i < (channel_mode ? 1 : 2); i++) {
        dialog_normalization[i] = -get_bits(&gbc, 5);
        compression_exists[i] = get_bits1(&gbc);
        if (compression_exists[i]) {
            skip_bits(&gbc, 8);
        }
    }

    /* dependent stream channel map */
    if (frame_type == EAC3_FRAME_TYPE_DEPENDENT) {
        if (get_bits1(&gbc)) {
            skip_bits(&gbc, 16); // skip custom channel map
        }
    }

    /* mixing metadata */
    if (get_bits1(&gbc)) {
        /* center and surround mix levels */
        if (channel_mode > AC3_CHMODE_STEREO) {
            preferred_downmix = get_bits(&gbc, 2);
            if((channel_mode & 0x1) && (channel_mode > 0x2)) {
                /* if three front channels exist */
                skip_bits(&gbc, 3);
                skip_bits(&gbc, 3);
            }
            if (channel_mode & 4) {
                /* if a surround channel exists */
                skip_bits(&gbc, 3);
                skip_bits(&gbc, 3);
            }
        }

        /* lfe mix level */
        int lfe_mix_level_exists;
        if (lfe_on && (lfe_mix_level_exists = get_bits1(&gbc))) {
            skip_bits(&gbc, 5);
        }

        /* info for mixing with other streams and substreams */
        if (frame_type == EAC3_FRAME_TYPE_INDEPENDENT) {
            for (i = 0; i < (channel_mode ? 1 : 2); i++) {
                // TODO: apply program scale factor
                if (get_bits1(&gbc)) {
                    skip_bits(&gbc, 6);  // skip program scale factor
                }
            }
            if (get_bits1(&gbc)) {
                skip_bits(&gbc, 6);  // skip external program scale factor
            }
            /* skip mixing parameter data */
            switch(get_bits(&gbc, 2)) {
                case 1: skip_bits(&gbc, 5);  break;
                case 2: skip_bits(&gbc, 12); break;
                case 3: {
                    int mix_data_size = (get_bits(&gbc, 5) + 2) << 3;

                    if (get_bits(&gbc, 1))    //mixdata2e
                    {
                      skip_bits(&gbc, 6+1+3);  //premixcmpsel,drcsrc,premixcmpscl
                      if(get_bits(&gbc, 1))   //extpgmlscle
                        skip_bits(&gbc, 4);  //extpgmlscl
                      if(get_bits(&gbc, 1))   //extpgmcscle
                        skip_bits(&gbc, 4);  //extpgmcscl
                      if(get_bits(&gbc, 1))   //extpgmrscle
                        skip_bits(&gbc, 4);  //extpgmrscl
                      if(get_bits(&gbc, 1))   //extpgmlsscle
                        skip_bits(&gbc, 4);  //extpgmlsscl
                      if(get_bits(&gbc, 1))   //extpgmrsscle
                        skip_bits(&gbc, 4);  //extpgmrsscl
                      if(get_bits(&gbc, 1))   //extpgmlfescle
                        skip_bits(&gbc, 4);  //extpgmlfescl
                      if(get_bits(&gbc, 1))   //dmixscle
                        skip_bits(&gbc, 4);  //dmixscl
                      if (get_bits(&gbc, 1))  //addche
                      {
                        if(get_bits(&gbc, 1))  //extpgmaux1scle
                          skip_bits(&gbc, 4);  //extpgmaux1scl
                        if(get_bits(&gbc, 1))  //extpgmaux2scle
                          skip_bits(&gbc, 4);  //extpgmaux2scl
                      }
                    }
                    if(get_bits(&gbc, 1))      //mixdata3e
                    {
                      skip_bits(&gbc, 5);      //spchdat
                      if(get_bits(&gbc, 1))    //addspchdate
                      {
                        skip_bits(&gbc, 5+2);  //spchdat1,spchan1att
                        if(get_bits(&gbc, 1))  //addspchdat1e
                          skip_bits(&gbc, 5+2);//spchdat2,spchan2att
                      }
                    }

                    skip_bits_long(&gbc, mix_data_size);
                    align_get_bits(&gbc);
                    break;
                }
            }
            /* skip pan information for mono or dual mono source */
            if (channel_mode < AC3_CHMODE_STEREO) {
                for (i = 0; i < (channel_mode ? 1 : 2); i++) {
                    if (get_bits1(&gbc)) {
                        /* note: this is not in the ATSC A/52B specification
                           reference: ETSI TS 102 366 V1.1.1
                                      section: E.1.3.1.25 */
                        skip_bits(&gbc, 8);  // skip pan mean direction index
                        skip_bits(&gbc, 6);  // skip reserved paninfo bits
                    }
                }
            }
            /* skip mixing configuration information */
            if (get_bits1(&gbc)) {
                for (blk = 0; blk < num_blocks; blk++) {
                    if (num_blocks == 1 || get_bits1(&gbc)) {
                        skip_bits(&gbc, 5);
                    }
                }
            }
        }
    }

    /* informational metadata */
    if (get_bits1(&gbc)) {
        skip_bits(&gbc, 3);
        skip_bits(&gbc, 2); // skip copyright bit and original bitstream bit
        if (channel_mode == AC3_CHMODE_STEREO) {
            skip_bits(&gbc, 2);
            skip_bits(&gbc, 2);
        }
        if (channel_mode >= AC3_CHMODE_2F2R) {
            skip_bits(&gbc, 2);
        }
        for (i = 0; i < (channel_mode ? 1 : 2); i++) {
            if (get_bits1(&gbc)) {
                skip_bits(&gbc, 8); // skip mix level, room type, and A/D converter type
            }
        }
        if (sr_code != 3) {
            skip_bits1(&gbc); // skip source sample rate code
        }
    }

    /* converter synchronization flag
       If frames are less than six blocks, this bit should be turned on
       once every 6 blocks to indicate the start of a frame set.
       reference: RFC 4598, Section 2.1.3  Frame Sets */
    if (frame_type == EAC3_FRAME_TYPE_INDEPENDENT && num_blocks != 6) {
        skip_bits1(&gbc); // skip converter synchronization flag
    }

    /* original frame size code if this stream was converted from AC-3 */
    if (frame_type == EAC3_FRAME_TYPE_AC3_CONVERT && num_blocks != 6)
        skip_bits1(&gbc); // convsync
    /* if bit stream converted from AC-3 */
    if(frame_type == EAC3_FRAME_TYPE_AC3_CONVERT){
      uint8_t blkid = 0;
      if (num_blocks == 6)       /* 6 blocks per syncframe */
        blkid = 1;
      else
        blkid = get_bits1(&gbc);
      if (blkid)
        skip_bits(&gbc, 6); // skip frame size code
    }

    int addbsil = 0;
    int ec3_extension_type = 0;
    int complexity_index = 0;
    /* additional bitstream info */
    if (get_bits1(&gbc)) { // addbsie
        uint8_t addbsi[64] = {0};
        addbsil = get_bits(&gbc, 6) + 1;
        av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header addbsil = %d, %d\n", addbsil, frame_type);
        for (i = 0; i < addbsil; i++)
            addbsi[i] = get_bits(&gbc, 8);
        // defined in 8.3 of ETSI TS 103 420
        if ((addbsil) >= 2 && addbsi[0] & 0x1)
        {
          ec3_extension_type = 1;
          complexity_index = addbsi[1];
          av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header complexity_index = %d\n", complexity_index);
        }
    }
    /* certainly not atmos if we see this */
    if (addbsil < 2 || ec3_extension_type != 1)
      return 0;

    if (complexity_index == 0 || complexity_index > 16)
      return 0;

    /*
    av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header size_in_bits = %d, frame_size_bits = %d\n", gbc.size_in_bits, frame_size * 8);
    // 18 bits = auxdatae (1) + errorcheck (crcrsv (1) + crc2 (16))
    // backup from end of frame to auxdatae
    av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header1 gbc.index = %d\n", gbc.index);
    gbc.index = (frame_size * 8) - 18;
    av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header2 gbc.index = %d\n", gbc.index);
    if (gbc.index > gbc.size_in_bits)
        return 0;

    if (get_bits1(&gbc)) {
        // back up to beginning of auxdatal field
        av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header3 gbc.index = %d\n", gbc.index);
        gbc.index -= 14;
        av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header4 gbc.index = %d\n", gbc.index);
        // length in bits
        int auxdatal = get_bits(&gbc, 14);
        av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header4 auxdatal = %d\n", auxdatal);
        // back up to beginning of auxdatal payload
        gbc.index -= auxdatal;
        // EMDF syncword may start anywhere in stream, also mid-byte!
        while(get_bits_left(&gbc) > 16) {
            av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header5 gbc.index = %d\n", gbc.index);
            int emdf_syncword = show_bits(&gbc, 16);
            if (emdf_syncword == 0x5838) {
                av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header emdf_syncword = %d\n", emdf_syncword);
                uint16_t emdf_container_length = show_bits_long(&gbc, 32) & 0xFFFF;
                av_log(NULL, AV_LOG_DEBUG, "avpriv_eac3atmos_parse_header emdf_container_length %d\n",
                    emdf_container_length);
                if (emdf_container_length > frame_size - ((get_bits_left(&gbc) + 7) >> 3)) {
                    skip_bits1(&gbc);
                    continue;
                }
                int num_objects_joc;
                int num_objects_oamd;
                if (parse_ac3_emdf(&gbc, &num_objects_oamd, &num_objects_joc))
                    break;
            } else {
                skip_bits1(&gbc);
            }
        }
    }
    */


    /*
      More rubust way is to also check EMDF for JOC objects but that gets tricky as EMDF
      can be in two places (auxdata field or skipfld). As this is targeted to playing
      eac3/atmos on appletv4, it seems to prefer media with two addbsil,
      ec3_extension_type set and complexity_index between 1 and 16. Go figure...
    */
    return addbsil == 2 &&
        ec3_extension_type == 1 &&
        (complexity_index > 0 && complexity_index <= 16);
}

int ff_ac3_parse_header(GetBitContext *gbc, AC3HeaderInfo *hdr)
{
    int frame_size_code;

    memset(hdr, 0, sizeof(*hdr));

    hdr->sync_word = get_bits(gbc, 16);
    if(hdr->sync_word != 0x0B77)
        return AAC_AC3_PARSE_ERROR_SYNC;

    /* read ahead to bsid to distinguish between AC-3 and E-AC-3 */
    hdr->bitstream_id = show_bits_long(gbc, 29) & 0x1F;
    if(hdr->bitstream_id > 16)
        return AAC_AC3_PARSE_ERROR_BSID;

    hdr->num_blocks = 6;
    hdr->ac3_bit_rate_code = -1;

    /* set default mix levels */
    hdr->center_mix_level   = 5;  // -4.5dB
    hdr->surround_mix_level = 6;  // -6.0dB

    /* set default dolby surround mode */
    hdr->dolby_surround_mode = AC3_DSURMOD_NOTINDICATED;

    if(hdr->bitstream_id <= 10) {
        /* Normal AC-3 */
        hdr->crc1 = get_bits(gbc, 16);
        hdr->sr_code = get_bits(gbc, 2);
        if(hdr->sr_code == 3)
            return AAC_AC3_PARSE_ERROR_SAMPLE_RATE;

        frame_size_code = get_bits(gbc, 6);
        if(frame_size_code > 37)
            return AAC_AC3_PARSE_ERROR_FRAME_SIZE;

        hdr->ac3_bit_rate_code = (frame_size_code >> 1);

        skip_bits(gbc, 5); // skip bsid, already got it

        hdr->bitstream_mode = get_bits(gbc, 3);
        hdr->channel_mode = get_bits(gbc, 3);

        if(hdr->channel_mode == AC3_CHMODE_STEREO) {
            hdr->dolby_surround_mode = get_bits(gbc, 2);
        } else {
            if((hdr->channel_mode & 1) && hdr->channel_mode != AC3_CHMODE_MONO)
                hdr->  center_mix_level =   center_levels[get_bits(gbc, 2)];
            if(hdr->channel_mode & 4)
                hdr->surround_mix_level = surround_levels[get_bits(gbc, 2)];
        }
        hdr->lfe_on = get_bits1(gbc);

        hdr->sr_shift = FFMAX(hdr->bitstream_id, 8) - 8;
        hdr->sample_rate = ff_ac3_sample_rate_tab[hdr->sr_code] >> hdr->sr_shift;
        hdr->bit_rate = (ff_ac3_bitrate_tab[hdr->ac3_bit_rate_code] * 1000) >> hdr->sr_shift;
        hdr->channels = ff_ac3_channels_tab[hdr->channel_mode] + hdr->lfe_on;
        hdr->frame_size = ff_ac3_frame_size_tab[frame_size_code][hdr->sr_code] * 2;
        hdr->frame_type = EAC3_FRAME_TYPE_AC3_CONVERT; //EAC3_FRAME_TYPE_INDEPENDENT;
        hdr->substreamid = 0;
    } else {
        /* Enhanced AC-3 */
        hdr->crc1 = 0;
        hdr->frame_type = get_bits(gbc, 2);
        if(hdr->frame_type == EAC3_FRAME_TYPE_RESERVED)
            return AAC_AC3_PARSE_ERROR_FRAME_TYPE;

        hdr->substreamid = get_bits(gbc, 3);

        hdr->frame_size = (get_bits(gbc, 11) + 1) << 1;
        if(hdr->frame_size < AC3_HEADER_SIZE)
            return AAC_AC3_PARSE_ERROR_FRAME_SIZE;

        hdr->sr_code = get_bits(gbc, 2);
        if (hdr->sr_code == 3) {
            int sr_code2 = get_bits(gbc, 2);
            if(sr_code2 == 3)
                return AAC_AC3_PARSE_ERROR_SAMPLE_RATE;
            hdr->sample_rate = ff_ac3_sample_rate_tab[sr_code2] / 2;
            hdr->sr_shift = 1;
        } else {
            hdr->num_blocks = eac3_blocks[get_bits(gbc, 2)];
            hdr->sample_rate = ff_ac3_sample_rate_tab[hdr->sr_code];
            hdr->sr_shift = 0;
        }

        hdr->channel_mode = get_bits(gbc, 3);
        hdr->lfe_on = get_bits1(gbc);

        hdr->bit_rate = 8LL * hdr->frame_size * hdr->sample_rate /
                        (hdr->num_blocks * 256);
        hdr->channels = ff_ac3_channels_tab[hdr->channel_mode] + hdr->lfe_on;
    }
    hdr->channel_layout = ff_ac3_channel_layout_tab[hdr->channel_mode];
    if (hdr->lfe_on)
        hdr->channel_layout |= AV_CH_LOW_FREQUENCY;

    return 0;
}

// TODO: Better way to pass AC3HeaderInfo fields to mov muxer.
int avpriv_ac3_parse_header(AC3HeaderInfo **phdr, const uint8_t *buf,
                            size_t size)
{
    GetBitContext gb;
    AC3HeaderInfo *hdr;
    int err;

    if (!*phdr)
        *phdr = av_mallocz(sizeof(AC3HeaderInfo));
    if (!*phdr)
        return AVERROR(ENOMEM);
    hdr = *phdr;

    err = init_get_bits8(&gb, buf, size);
    if (err < 0)
        return AVERROR_INVALIDDATA;
    err = ff_ac3_parse_header(&gb, hdr);
    if (err < 0)
        return AVERROR_INVALIDDATA;

    return get_bits_count(&gb);
}

int av_ac3_parse_header(const uint8_t *buf, size_t size,
                        uint8_t *bitstream_id, uint16_t *frame_size)
{
    GetBitContext gb;
    AC3HeaderInfo hdr;
    int err;

    init_get_bits8(&gb, buf, size);
    err = ff_ac3_parse_header(&gb, &hdr);
    if (err < 0)
        return AVERROR_INVALIDDATA;

    *bitstream_id = hdr.bitstream_id;
    *frame_size   = hdr.frame_size;

    return 0;
}

static int ac3_sync(uint64_t state, AACAC3ParseContext *hdr_info,
        int *need_next_header, int *new_frame_start)
{
    int err;
    union {
        uint64_t u64;
        uint8_t  u8[8 + AV_INPUT_BUFFER_PADDING_SIZE];
    } tmp = { av_be2ne64(state) };
    AC3HeaderInfo hdr;
    GetBitContext gbc;

    if (tmp.u8[1] == 0x77 && tmp.u8[2] == 0x0b) {
        FFSWAP(uint8_t, tmp.u8[1], tmp.u8[2]);
        FFSWAP(uint8_t, tmp.u8[3], tmp.u8[4]);
        FFSWAP(uint8_t, tmp.u8[5], tmp.u8[6]);
    }

    init_get_bits(&gbc, tmp.u8+8-AC3_HEADER_SIZE, 54);
    err = ff_ac3_parse_header(&gbc, &hdr);

    if(err < 0)
        return 0;

    hdr_info->sample_rate = hdr.sample_rate;
    hdr_info->bit_rate = hdr.bit_rate;
    hdr_info->channels = hdr.channels;
    hdr_info->channel_layout = hdr.channel_layout;
    hdr_info->samples = hdr.num_blocks * 256;
    hdr_info->service_type = hdr.bitstream_mode;
    if (hdr.bitstream_mode == 0x7 && hdr.channels > 1)
        hdr_info->service_type = AV_AUDIO_SERVICE_TYPE_KARAOKE;
    if(hdr.bitstream_id>10)
        hdr_info->codec_id = AV_CODEC_ID_EAC3;
    else if (hdr_info->codec_id == AV_CODEC_ID_NONE)
        hdr_info->codec_id = AV_CODEC_ID_AC3;

    *new_frame_start  = (hdr.frame_type != EAC3_FRAME_TYPE_DEPENDENT);
    *need_next_header = *new_frame_start || (hdr.frame_type != EAC3_FRAME_TYPE_AC3_CONVERT);
    return hdr.frame_size;
}

static av_cold int ac3_parse_init(AVCodecParserContext *s1)
{
    AACAC3ParseContext *s = s1->priv_data;
    s->header_size = AC3_HEADER_SIZE;
    s->sync = ac3_sync;
    return 0;
}


const AVCodecParser ff_ac3_parser = {
    .codec_ids      = { AV_CODEC_ID_AC3, AV_CODEC_ID_EAC3 },
    .priv_data_size = sizeof(AACAC3ParseContext),
    .parser_init    = ac3_parse_init,
    .parser_parse   = ff_aac_ac3_parse,
    .parser_close   = ff_parse_close,
};

#else

int avpriv_ac3_parse_header(AC3HeaderInfo **phdr, const uint8_t *buf,
                            size_t size)
{
    return AVERROR(ENOSYS);
}

int av_ac3_parse_header(const uint8_t *buf, size_t size,
                        uint8_t *bitstream_id, uint16_t *frame_size)
{
    return AVERROR(ENOSYS);
}
#endif
