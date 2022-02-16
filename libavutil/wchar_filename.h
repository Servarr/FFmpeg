/*
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

#ifndef AVUTIL_WCHAR_FILENAME_H
#define AVUTIL_WCHAR_FILENAME_H

#ifdef _WIN32
#include <windows.h>
#include "mem.h"

av_warn_unused_result
static inline int utf8towchar(const char *filename_utf8, wchar_t **filename_w)
{
    int num_chars;
    num_chars = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, filename_utf8, -1, NULL, 0);
    if (num_chars <= 0) {
        *filename_w = NULL;
        return 0;
    }
    *filename_w = (wchar_t *)av_calloc(num_chars, sizeof(wchar_t));
    if (!*filename_w) {
        errno = ENOMEM;
        return -1;
    }
    MultiByteToWideChar(CP_UTF8, 0, filename_utf8, -1, *filename_w, num_chars);
    return 0;
}

av_warn_unused_result
static inline int wchartocp(unsigned int code_page, const wchar_t *filename_w,
                            char **filename)
{
    DWORD flags = code_page == CP_UTF8 ? MB_ERR_INVALID_CHARS : 0;
    int num_chars = WideCharToMultiByte(code_page, flags, filename_w, -1,
                                        NULL, 0, NULL, NULL);
    if (num_chars <= 0) {
        *filename = NULL;
        return 0;
    }
    *filename = av_calloc(num_chars, sizeof(char));
    if (!*filename) {
        errno = ENOMEM;
        return -1;
    }
    WideCharToMultiByte(code_page, flags, filename_w, -1,
                        *filename, num_chars, NULL, NULL);
    return 0;
}

av_warn_unused_result
static inline int wchartoutf8(const wchar_t *filename_w, char **filename)
{
    return wchartocp(CP_UTF8, filename_w, filename);
}

av_warn_unused_result
static inline int wchartoansi(const wchar_t *filename_w, char **filename)
{
    return wchartocp(CP_ACP, filename_w, filename);
}

av_warn_unused_result
static inline int utf8toansi(const char *filename_utf8, char **filename)
{
    wchar_t *filename_w = NULL;
    int ret = -1;
    if (utf8towchar(filename_utf8, &filename_w))
        return -1;

    if (!filename_w) {
        *filename = NULL;
        return 0;
    }

    ret = wchartoansi(filename_w, filename);
    av_free(filename_w);
    return ret;
}
#endif

#endif /* AVUTIL_WCHAR_FILENAME_H */
